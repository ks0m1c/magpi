;;; magpi-transient.el --- Minimal launch interface for Magpi -*- lexical-binding: t; -*-

(require 'transient)
(require 'magpi-launch)

(declare-function magpi-spawn-from-options "magpi" (options))

(defun magpi-launch--read-profile (prompt initial-input _history)
  (completing-read prompt (mapcar #'car magpi-model-profiles)
                   nil t initial-input nil magpi-default-profile))

(defun magpi-launch--read-authority (prompt initial-input _history)
  (completing-read prompt '("Writer" "Read-only")
                   nil t initial-input nil magpi-default-authority))

(defun magpi-launch--read-context (prompt initial-input _history)
  (completing-read prompt '("Point" "Region" "None")
                   nil t initial-input nil
                   (if (use-region-p) "Region" "Point")))

(defun magpi-launch--initial-values (prefix)
  (oset prefix value
        (list (concat "--profile=" magpi-default-profile)
              (concat "--authority=" magpi-default-authority)
              (concat "--context=" (if (use-region-p) "Region" "Point")))))

(defun magpi-launch-dispatch ()
  "Freeze the configured choices and hand them to Magpi orchestration."
  (interactive)
  (let ((args (transient-args 'magpi-launch)))
    (magpi-spawn-from-options
     (list :intent (transient-arg-value "--intent=" args)
           :profile (transient-arg-value "--profile=" args)
           :authority (transient-arg-value "--authority=" args)
           :context-kind (transient-arg-value "--context=" args)))))

;;;###autoload
(transient-define-prefix magpi-launch ()
  "Configure the single frozen specification for a Magpi attempt."
  :init-value #'magpi-launch--initial-values
  [["Launch"
    ("i" "Intention" "--intent=" :always-read t)
    ("p" "Profile" "--profile=" :reader magpi-launch--read-profile)
    ("c" "Context" "--context=" :reader magpi-launch--read-context)
    ("a" "Authority" "--authority=" :reader magpi-launch--read-authority)]
   ["Actions"
    ("RET" "Spawn" magpi-launch-dispatch)]])

(provide 'magpi-transient)
;;; magpi-transient.el ends here
