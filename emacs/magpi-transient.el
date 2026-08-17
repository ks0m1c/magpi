;;; magpi-transient.el --- Minimal launch interface for Magpi -*- lexical-binding: t; -*-

(require 'transient)
(require 'magpi-launch)

(declare-function magpi-spawn-from-options "magpi" (options))

(defun magpi-launch--read-profile (prompt initial-input _history)
  (completing-read prompt (mapcar #'car magpi-launch-profiles)
                   nil t initial-input nil magpi-default-profile))

(defun magpi-launch--read-effort (prompt initial-input _history)
  (completing-read prompt (mapcar #'car magpi-launch-efforts)
                   nil t initial-input nil magpi-default-effort))

(defun magpi-launch--read-model (prompt initial-input _history)
  (completing-read prompt magpi-launch-models
                   nil nil initial-input nil magpi-default-model))

(defun magpi-launch--read-authority (prompt initial-input _history)
  (completing-read prompt '("Writer" "Read-only")
                   nil t initial-input nil
                   (magpi-launch-authority-label magpi-default-authority)))

(defun magpi-launch--read-context (prompt initial-input _history)
  (completing-read prompt '("Point" "Region" "None")
                   nil t initial-input nil
                   (magpi-launch-context-kind-label
                    (magpi-launch-default-context-kind))))

(defun magpi-launch--initial-values (prefix)
  (oset prefix value
        (list (concat "--profile=" magpi-default-profile)
              (concat "--effort=" magpi-default-effort)
              (concat "--model=" magpi-default-model)
              (concat "--authority="
                      (magpi-launch-authority-label magpi-default-authority))
              (concat "--context="
                      (magpi-launch-context-kind-label
                       (magpi-launch-default-context-kind))))))

(defun magpi-launch-dispatch ()
  "Validate transient labels and hand semantic choices to Magpi orchestration."
  (interactive)
  (let ((args (transient-args 'magpi-launch)))
    (magpi-spawn-from-options
     (list :intent (transient-arg-value "--intent=" args)
           :profile (transient-arg-value "--profile=" args)
           :effort (magpi-launch-effort-from-label
                    (transient-arg-value "--effort=" args))
           :model (transient-arg-value "--model=" args)
           :authority (magpi-launch-authority-from-label
                       (transient-arg-value "--authority=" args))
           :context-kind (magpi-launch-context-kind-from-label
                          (transient-arg-value "--context=" args))))))

;;;###autoload
(transient-define-prefix magpi-launch ()
  "Configure the single frozen specification for a Magpi attempt."
  :init-value #'magpi-launch--initial-values
  [["Intent"
    ("i" "Intention" "--intent=" :always-read t)]
   ["Runtime"
    ("m" "Model" "--model=" :reader magpi-launch--read-model)
    ("e" "Effort" "--effort=" :reader magpi-launch--read-effort)
    ("p" "Profile" "--profile=" :reader magpi-launch--read-profile)]
   ["Scope"
    ("c" "Context" "--context=" :reader magpi-launch--read-context)
    ("a" "Authority" "--authority=" :reader magpi-launch--read-authority)]
   ["Actions"
    ("RET" "Spawn" magpi-launch-dispatch)]])

(provide 'magpi-transient)
;;; magpi-transient.el ends here
