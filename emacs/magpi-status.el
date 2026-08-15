;;; magpi-status.el --- Magit-backed status projection for Magpi -*- lexical-binding: t; -*-

;; This adapter deliberately depends only on Magit's reusable presentation
;; primitives and Magpi's read model.  Runtime ownership remains elsewhere.

(require 'magit-mode)
(require 'magit-section)
(require 'magpi-core)

(defvar-local magpi-status-root nil
  "Project root represented by the current Magpi status buffer.")

(defvar-local magpi-status-attempts-function nil
  "Zero-argument function returning attempts for `magpi-status-root'.")

(defvar-local magpi-status-visit-function nil
  "Function called with the typed target at point, if any.")

(defvar-local magpi-status-spawn-function nil
  "Interactive function used to launch an attempt from this status buffer.")

(defun magpi-status--label (status)
  (pcase status
    ('starting "starting")
    ('running "running")
    ('idle "idle")
    ('attention "attention")
    ('disconnected "disconnected")
    (_ "unknown")))

(defun magpi-status--intent (attempt)
  "Return ATTEMPT's intention or its deliberately quiet placeholder."
  (let ((intent (magpi-attempt-intent attempt)))
    (if (string-empty-p (or intent ""))
        (propertize "◯" 'face 'shadow)
      intent)))

(defun magpi-status--insert-attempt (attempt)
  "Insert ATTEMPT as a typed, foldable Magit section."
  (let ((id (magpi-attempt-id attempt))
        (intent (magpi-status--intent attempt)))
    (magit-insert-section (magpi-attempt id nil)
      (magit-insert-heading
       (format "%s    %s · %s"
               intent
               (magpi-status--label (magpi-attempt-status attempt))
               (magpi-attempt-profile attempt)))
      (insert (format "    intent     %s\n" intent))
      (insert (format "    authority  %s\n" (magpi-attempt-authority attempt)))
      (when-let ((activity (magpi-attempt-activity attempt)))
        (insert (format "    activity   %s\n" activity)))
      (when-let ((last (magpi-attempt-last-response attempt)))
        (unless (string-empty-p last)
          (insert (format "    last       %s\n" last))))
      (when-let ((files (magpi-attempt-observed-files attempt)))
        (magit-insert-section (magpi-observed-files id nil)
          (magit-insert-heading "    observed files")
          (dolist (file files)
            (magit-insert-section (magpi-observed-file (cons id file) nil)
              (insert (format "      %s\n" file)))))))))

(defun magpi-status-refresh-buffer ()
  "Render the status projection from its read-model function.

The `magit-mode' refresh transaction preserves fold state, point, and window
position using section identity; this function only describes the projection."
  (magit-insert-section (magpi-status magpi-status-root)
    (magit-insert-heading
     (format "MAGPI · %s"
             (file-name-nondirectory
              (directory-file-name magpi-status-root))))
    (let ((attempts (and magpi-status-attempts-function
                         (funcall magpi-status-attempts-function))))
      (if attempts
          (mapc #'magpi-status--insert-attempt attempts)
        (insert "\n  No attempts. Press s to state an intention and spawn.\n")))))

(defun magpi-status-target-at-point ()
  "Return the typed Magpi target at point, or nil.

Identity comes from Magit section values, never rendered text."
  (cond
   ((magit-section-value-if 'magpi-observed-file)
    (pcase-let ((`(,attempt-id . ,path)
                 (magit-section-value-if 'magpi-observed-file)))
      (list :kind 'observed-file :attempt-id attempt-id :path path)))
   ((magit-section-value-if 'magpi-attempt)
    (list :kind 'attempt
          :attempt-id (magit-section-value-if 'magpi-attempt)))))

(defun magpi-status-visit-at-point ()
  "Visit the typed Magpi object at point, or toggle its section."
  (interactive)
  (if-let ((target (magpi-status-target-at-point)))
      (if magpi-status-visit-function
          (funcall magpi-status-visit-function target)
        (user-error "No visitor is configured for this Magpi buffer"))
    (when-let ((section (magit-current-section)))
      (magit-section-toggle section))))

(defun magpi-status-spawn ()
  "Launch an attempt through the status buffer's configured command."
  (interactive)
  (if magpi-status-spawn-function
      (call-interactively magpi-status-spawn-function)
    (user-error "No spawn command is configured for this Magpi buffer")))

(defun magpi-status-refresh ()
  "Refresh the current Magpi status buffer."
  (interactive)
  (magit-refresh-buffer))

(defvar-keymap magpi-status-mode-map
  :doc "Keymap for `magpi-status-mode'."
  :parent magit-mode-map
  "RET" #'magpi-status-visit-at-point
  "s" #'magpi-status-spawn
  "g" #'magpi-status-refresh
  "q" #'quit-window)

(define-derived-mode magpi-status-mode magit-mode "Magpi"
  "Read-only Magpi porcelain composed from Magit sections and refreshes."
  (setq-local truncate-lines t))

(defun magpi-status-open (root attempts-function visit-function spawn-function)
  "Show ROOT using Magit's transactional status-buffer machinery.

ATTEMPTS-FUNCTION is a zero-argument read-model query.  VISIT-FUNCTION accepts
a typed target from `magpi-status-target-at-point`; SPAWN-FUNCTION is invoked
interactively.  Neither callback may infer identity from rendered text."
  (let* ((root (file-name-as-directory (expand-file-name root)))
         (name (format "*Magpi:%s*"
                       (file-name-nondirectory (directory-file-name root)))))
    (magit-setup-buffer #'magpi-status-mode nil
      :buffer name
      :directory root
      (magpi-status-root root)
      (magpi-status-attempts-function attempts-function)
      (magpi-status-visit-function visit-function)
      (magpi-status-spawn-function spawn-function))))

(provide 'magpi-status)
;;; magpi-status.el ends here
