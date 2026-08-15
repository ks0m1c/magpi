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

(defvar magpi--attempts (make-hash-table :test #'equal))
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
  (pcase kind
    ("None" (list :kind 'none))
    ("Region"
     (unless (use-region-p)
       (user-error "No active region"))
     (list :kind 'region
           :file buffer-file-name
           :line (line-number-at-pos (region-beginning))
           :text (buffer-substring-no-properties
                  (region-beginning) (region-end))))
    (_
     (list :kind 'point
           :file buffer-file-name
           :line (line-number-at-pos)
           :text (string-trim
                  (buffer-substring-no-properties
                   (line-beginning-position) (line-end-position)))))))



(defun magpi--register-attempt (attempt handle)
  "Register ATTEMPT and reduce events delivered for HANDLE."
  (puthash (magpi-attempt-id attempt) attempt magpi--attempts)
  (setf (magpi-attempt-backend-handle attempt) handle)
  (magpi-backend-subscribe
   magpi-backend handle
   (lambda (event) (magpi--handle-event attempt event))))

(defun magpi-spawn-spec (spec)
  "Launch the already-frozen SPEC through the configured backend."
  (let* ((id (magpi-launch-spec-id spec))
         (attempt
          (make-magpi-attempt
           :id id
           :name (magpi-launch-spec-name spec)
           :intent (magpi-launch-spec-intent spec)
           :root (magpi-launch-spec-root spec)
           :profile (magpi-launch-spec-profile spec)
           :model (magpi-launch-spec-model spec)
           :thinking (magpi-launch-spec-thinking spec)
           :authority (magpi-launch-spec-authority spec)
           :context (magpi-launch-spec-context spec)
           :launch-spec spec
           :status 'starting
           :started-at (current-time)))
         (handle (magpi-backend-spawn magpi-backend spec)))
    (magpi--register-attempt attempt handle)
    (when-let ((message (magpi-launch-spec-first-message spec)))
      (magpi-backend-send magpi-backend handle message))
    (magpi--schedule-refresh)
    attempt))

(defun magpi-spawn-from-options (options)
  "Capture OPTIONS at dispatch and launch their immutable specification."
  (let* ((root (magpi--root))
         (context (magpi--capture-context (plist-get options :context-kind)))
         (spec (magpi-launch-build
                (magpi--new-id) root
                (plist-get options :intent)
                (or (plist-get options :profile) magpi-default-profile)
                (or (plist-get options :authority) magpi-default-authority)
                context)))
    (magpi-spawn-spec spec)))

;;;###autoload
(defun magpi-spawn ()
  "Configure and spawn a Magpi attempt."
  (interactive)
  (magpi-launch))

(defun magpi--handle-event (attempt event)
  "Reduce EVENT for ATTEMPT, then refresh the read-only projection."
  (magpi-attempt-apply-event attempt event)
  (magpi--schedule-refresh))

(defun magpi--attempts-for-root (root)
  (let (attempts)
    (maphash
     (lambda (_id attempt)
       (when (equal (file-truename (magpi-attempt-root attempt))
                    (file-truename root))
         (push attempt attempts)))
     magpi--attempts)
    (sort attempts
          (lambda (a b)
            (time-less-p (magpi-attempt-started-at a)
                         (magpi-attempt-started-at b))))))


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
  (let ((attempt (gethash (plist-get target :attempt-id) magpi--attempts)))
    (unless attempt
      (user-error "This attempt is no longer available"))
    (pcase (plist-get target :kind)
      ('observed-file
       (let* ((root (magpi-attempt-root attempt))
              (file (expand-file-name (plist-get target :path) root)))
         ;; Observed-file evidence is project-scoped; never let a malformed
         ;; event turn RET into an arbitrary filesystem visit.
         (unless (file-in-directory-p file root)
           (user-error "Observed file is outside the attempt project"))
         (find-file file)))
      ('attempt
       (magpi-backend-visit magpi-backend
                            (magpi-attempt-backend-handle attempt))))))

;;;###autoload
(defun magpi-status (&optional root)
  "Open the Magit-backed Magpi status buffer for ROOT."
  (interactive)
  (let ((root (file-name-as-directory
               (expand-file-name (or root (magpi--root))))))
    (magpi-status-open root
                       (lambda () (magpi--attempts-for-root root))
                       #'magpi--visit-status-target
                       #'magpi-spawn)))

(define-key pimacs-chat-mode-map (kbd "C-c m m") #'magpi-status)
(define-key pimacs-chat-mode-map (kbd "C-c m s") #'magpi-spawn)
(define-key pimacs-chat-mode-map (kbd "m") #'pimacs-select-model)

(provide 'magpi)
;;; magpi.el ends here
