;;; magpi.el --- Lean actionable Pi workbench -*- lexical-binding: t; -*-
;; Version: 0.1.0
;; Keywords: tools
;; Package-Requires: ((emacs "29.1") (magit "3.0"))

(require 'project)
(require 'subr-x)
(require 'magpi-core)
(require 'magpi-launch)
(require 'magpi-backend)
(require 'magpi-pimacs-backend)
(require 'magpi-status)
(require 'magpi-transient)

(defvar magpi--attempts (make-hash-table :test #'equal)
  "Latest immutable attempt value by attempt ID.")

(defvar magpi--handles (make-hash-table :test #'equal)
  "Opaque backend handles by attempt ID; never part of the domain record.")

(defvar magpi--refresh-timer nil)

(defun magpi--root ()
  (file-name-as-directory
   (expand-file-name
    (if-let ((project (project-current)))
        (project-root project)
      default-directory))))

(defun magpi--new-id ()
  (substring
   (md5 (format "%s:%s:%s:%s" (float-time) (random) (emacs-pid) (user-uid)))
   0 12))

(defun magpi--capture-context (kind)
  "Capture semantic context KIND, rejecting unknown kinds.

Point and Region are file evidence.  Magpi status and Pimacs chat are porcelain;
capturing them would feed the workbench back into the agent prompt."
  (pcase kind
    ('none (list :kind 'none))
    ((or 'region 'point)
     (unless (magpi-launch-source-buffer-p)
       (user-error "Magpi context %s needs a source file, not workbench porcelain"
                   kind))
     (if (eq kind 'region)
         (progn
           (unless (use-region-p)
             (user-error "No active region"))
           (list :kind 'region
                 :file buffer-file-name
                 :line (line-number-at-pos (region-beginning))
                 :text (buffer-substring-no-properties
                        (region-beginning) (region-end))))
       (list :kind 'point
             :file buffer-file-name
             :line (line-number-at-pos)
             :text (string-trim
                    (buffer-substring-no-properties
                     (line-beginning-position) (line-end-position))))))
    (_ (user-error "Unknown Magpi context kind: %S" kind))))

(defun magpi--handle-event (id event)
  "Reduce EVENT into the latest attempt for immutable ID, then refresh.

The listener captures only ID.  This is the one mutation point for the attempt
registry; reduction itself is functional.  Restated facts keep the same attempt
object and do not schedule another paint."
  (when-let ((attempt (gethash id magpi--attempts)))
    (let ((next (magpi-attempt-reduce attempt event)))
      (unless (eq next attempt)
        (puthash id next magpi--attempts)
        (magpi--schedule-refresh)))))

(defun magpi--record-launch-problem (attempt error-data)
  "Return ATTEMPT marked as disconnected after launch ERROR-DATA."
  (magpi-attempt-reduce
   (magpi-attempt-reduce attempt
                         (list :type 'problem-observed
                               :problem (error-message-string error-data)))
   '(:type disconnected)))

(defun magpi-spawn-spec (id intent launch)
  "Declare and launch immutable attempt ID with INTENT and LAUNCH.

Once a backend launch is attempted, failures remain attributable observations;
they never erase the attempt record or a returned handle."
  (let ((attempt (make-magpi-attempt
                  :id id :intent intent :launch launch
                  :observation (magpi-observation-initial)
                  :started-at (current-time)))
        handle
        spawn-attempted)
    ;; Store the initial value before a backend can synchronously emit events.
    (puthash id attempt magpi--attempts)
    (condition-case error-data
        (progn
          (setq spawn-attempted t
                handle (magpi-backend-spawn
                        magpi-backend attempt
                        (lambda (event) (magpi--handle-event id event))))
          (puthash id handle magpi--handles)
          (magpi-backend-send-initial magpi-backend handle attempt))
      (error
       (if spawn-attempted
           (let ((attributable (or (gethash id magpi--attempts) attempt)))
             ;; Preserve any synchronous facts emitted before the failure as well
             ;; as the failure attribution itself.
             (puthash id (magpi--record-launch-problem attributable error-data)
                      magpi--attempts)
             (when handle
               (puthash id handle magpi--handles))
             (message "Magpi launch for %s failed: %s"
                      id (error-message-string error-data)))
         (remhash id magpi--attempts))))
    (magpi--schedule-refresh)
    (gethash id magpi--attempts)))

(defun magpi-spawn-from-options (options)
  "Capture validated semantic OPTIONS and launch their immutable specification."
  (let* ((root (magpi--root))
         (context (magpi--capture-context (plist-get options :context-kind)))
         (intent (magpi-normalize-intent (plist-get options :intent)))
         (launch (magpi-launch-build
                  root
                  (or (plist-get options :profile) magpi-default-profile)
                  (or (plist-get options :authority) magpi-default-authority)
                  context
                  (plist-get options :model)
                  (if (plist-member options :effort)
                      (plist-get options :effort)
                    'profile))))
    (magpi-spawn-spec (magpi--new-id) intent launch)))

;;;###autoload
(defun magpi-spawn ()
  "Configure and spawn a Magpi attempt."
  (interactive)
  (magpi-launch))

(defun magpi--attempts-for-root (root)
  (let (attempts)
    (maphash
     (lambda (_id attempt)
       (when (equal (file-truename
                     (magpi-launch-spec-root (magpi-attempt-launch attempt)))
                    (file-truename root))
         (push attempt attempts)))
     magpi--attempts)
    (sort attempts
          (lambda (a b)
            (time-less-p (magpi-attempt-started-at a)
                         (magpi-attempt-started-at b))))))

(defun magpi--reconcile-attempts-for-root (root)
  "Ask the backend to re-emit live observations for ROOT's attempts.

This is a snapshot pull, not part of event-driven paint.  Running-model,
session usage, and similar transport-only facts appear when the snapshot
replies, which reduce like any other event.  Calling this from refresh
re-enters the mailbox."
  (let ((root (file-truename root)))
    (maphash
     (lambda (id attempt)
       (when (equal (file-truename
                     (magpi-launch-spec-root (magpi-attempt-launch attempt)))
                    root)
         (when-let ((handle (gethash id magpi--handles)))
           (magpi-backend-reconcile
            magpi-backend handle
            (lambda (event) (magpi--handle-event id event))))))
     magpi--attempts)))

(defun magpi--refresh-visible-buffers ()
  (setq magpi--refresh-timer nil)
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (derived-mode-p 'magpi-status-mode)
        (ignore-errors (magit-refresh-buffer))))))

(defun magpi--schedule-refresh ()
  (when (timerp magpi--refresh-timer)
    (cancel-timer magpi--refresh-timer))
  (setq magpi--refresh-timer
        (run-with-idle-timer 0.15 nil #'magpi--refresh-visible-buffers)))

(defun magpi--visit-status-target (target)
  "Visit the exact typed TARGET selected in a Magpi status buffer."
  (let* ((id (plist-get target :attempt-id))
         (attempt (gethash id magpi--attempts)))
    (unless attempt
      (user-error "This attempt is no longer available"))
    (pcase (plist-get target :kind)
      ('observed-file
       (let* ((root (file-name-as-directory
                     (file-truename
                      (magpi-launch-spec-root (magpi-attempt-launch attempt)))))
              (file (file-truename
                     (expand-file-name (plist-get target :path) root))))
         ;; Evidence is lexical and pure in the reducer; validate filesystem
         ;; containment only at this effectful navigation boundary.
         (unless (file-in-directory-p file root)
           (user-error "Observed file is outside the attempt project"))
         (find-file file)))
      ('attempt
       (if-let ((handle (gethash id magpi--handles)))
           (magpi-backend-visit magpi-backend handle)
         (user-error "This attempt has no backend handle"))))))

;;;###autoload
(defun magpi-status (&optional root)
  "Open the Magit-backed Magpi status buffer for ROOT.

The first paint reads the registry.  One snapshot pull then fills in
transport-only facts such as running model and token usage; later paints are
driven only by new observations."
  (interactive)
  (let ((root (file-name-as-directory
               (expand-file-name (or root (magpi--root))))))
    (magpi-status-open root
                       (lambda () (magpi--attempts-for-root root))
                       #'magpi--visit-status-target
                       #'magpi-spawn
                       (lambda () (magpi--reconcile-attempts-for-root root)))
    (magpi--reconcile-attempts-for-root root)))

(define-key pimacs-chat-mode-map (kbd "C-c m m") #'magpi-status)
(define-key pimacs-chat-mode-map (kbd "C-c m s") #'magpi-spawn)

(provide 'magpi)
;;; magpi.el ends here
