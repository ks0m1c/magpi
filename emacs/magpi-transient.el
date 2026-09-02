;;; magpi-transient.el --- Spawn surface: model, thinking, role, bind -*- lexical-binding: t; -*-

;; One job: collect launch choices and hand them to orchestration.

(require 'transient)
(require 'magpi-launch)

(defvar magpi-launch-execute-function nil
  "Function of one options plist invoked by `magpi-launch-dispatch'.
Installed by Magpi orchestration so the transient can autoload alone.")

;; Binary role must stay set: plain `transient-switches' cycles to nil.
;; Only define the class when real Transient eieio types are present.
(when (and (fboundp 'transient-infix-read)
           (get 'transient-switches 'cl--class))
  (defclass magpi-launch-role-switch (transient-switches) ()
    "Two-state role switch; never clears the choice.")
  (cl-defmethod transient-infix-read ((obj magpi-launch-role-switch))
    (if (equal (oref obj value) "--role=w")
        "--role=r"
      "--role=w")))

(defun magpi-launch--read-thinking (prompt initial-input _history)
  (completing-read prompt (mapcar #'car magpi-launch-thinking-choices)
                   nil t initial-input nil
                   (magpi-launch-thinking-choice-label magpi-default-thinking)))

(defun magpi-launch--read-model (prompt initial-input _history)
  "Read a canonical model id with normalized fuzzy completing-read."
  (magpi-launch-refresh-catalog (magpi-launch-current-root))
  ;; `basic' only: the table owns normalized fuzzy filtering.  Extra
  ;; styles would re-filter against the raw (unnormalized) query.
  (let ((completion-styles '(basic))
        (completion-category-defaults nil)
        (completion-category-overrides nil))
    (completing-read prompt (magpi-launch-model-completion-table)
                     nil nil initial-input nil
                     (magpi-launch-default-model))))

(defun magpi-launch--read-context (prompt initial-input _history)
  (completing-read prompt '("Point" "Region" "None")  ; None = do not bind
                   nil t initial-input nil
                   (magpi-launch-bind-label
                    (magpi-launch-default-bind))))

(defun magpi-launch--initial-values (prefix)
  (magpi-launch-refresh-catalog (magpi-launch-current-root))
  (oset prefix value
        (list (concat "--thinking="
                      (magpi-launch-thinking-choice-label magpi-default-thinking))
              (concat "--model=" (magpi-launch-default-model))
              (concat "--role="
                      (magpi-launch-role-label magpi-default-role))
              (concat "--context="
                      (magpi-launch-bind-label
                       (magpi-launch-default-bind))))))

(defun magpi-launch--options-from-args (args)
  "Translate transient ARGS into a semantic options plist.

Intention membership is Transient scope, not a special variable."
  (append
   (list :thinking (magpi-launch-thinking-from-label
                    (transient-arg-value "--thinking=" args))
         :model (transient-arg-value "--model=" args)
         :role (magpi-launch-role-from-label
                     (transient-arg-value "--role=" args))
         :bind (magpi-launch-bind-from-label
                        (transient-arg-value "--context=" args)))
   (when (member "--lease" args)
     (list :lease t))
   (when-let ((id (and (fboundp 'transient-scope) (transient-scope))))
     (list :intention-id id))))

(defun magpi-launch-dispatch ()
  "Validate transient labels and hand semantic choices to Magpi orchestration."
  (interactive)
  (unless magpi-launch-execute-function
    (require 'magpi)
    (unless magpi-launch-execute-function
      (user-error "Magpi spawn is not available")))
  (funcall magpi-launch-execute-function
           (magpi-launch--options-from-args (transient-args 'magpi-launch))))

;;;###autoload
(transient-define-prefix magpi-launch (intention-id)
  "Configure the single frozen specification for a Magpi action."
  :init-value #'magpi-launch--initial-values
  [["Model/Thinking"
    ("m" "Model" "--model=" :reader magpi-launch--read-model)
    ("t" "Thinking" "--thinking=" :reader magpi-launch--read-thinking)]
   ["Scope"
    ("c" "Bind" "--context=" :reader magpi-launch--read-context)
    ("w" "Role (Writer/Reader)" "--role=" :class magpi-launch-role-switch
     :choices ("w" "r")
     :argument-format "--role=%s"
     :argument-regexp "\\(--role=\\(w\\|r\\)\\)")
    ("W" "Exclusive writer" "--lease")]
   ["Spawn"
    ("s" "Spawn" magpi-launch-dispatch)
    ("RET" "Spawn" magpi-launch-dispatch)]]
  (interactive (list (and (featurep 'magpi-status)
                          (fboundp 'magpi-section-intention-id)
                          (magpi-section-intention-id))))
  (transient-setup 'magpi-launch nil nil :scope intention-id))

(provide 'magpi-transient)
;;; magpi-transient.el ends here
