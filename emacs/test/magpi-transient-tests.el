;;; magpi-transient-tests.el --- Tests for Magpi's launch transient -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'ert)
(require 'seq)

;; The pure test suite has no package load-path.  This only supplies enough of
;; Transient to load the declaration; dispatch itself is tested below.
(unless (require 'transient nil t)
  (defmacro transient-define-prefix (name _arguments &rest _body)
    `(defun ,name () (interactive)))
  (provide 'transient))

(require 'magpi-transient)

(ert-deftest magpi-launch-dispatch-translates-labels-to-semantic-choices ()
  (let ((args '("--intent=Repair token refresh"
                "--profile=Quick"
                "--effort=High"
                "--model=openai/gpt-4.1"
                "--context=Region"
                "--authority=Read-only"))
        captured)
    (cl-letf (((symbol-function 'transient-args) (lambda (_command) args))
              ((symbol-function 'transient-arg-value)
               (lambda (prefix values)
                 (when-let ((value (seq-find (lambda (arg) (string-prefix-p prefix arg)) values)))
                   (substring value (length prefix)))))
              ((symbol-function 'magpi-spawn-from-options)
               (lambda (options) (setq captured options))))
      (magpi-launch-dispatch))
    (should (equal captured
                   '(:intent "Repair token refresh" :profile "Quick"
                     :effort high :model "openai/gpt-4.1"
                     :authority read-only :context-kind region)))))

(ert-deftest magpi-launch-dispatch-inherits-model-and-profile-effort ()
  (let ((args '("--intent=Inspect"
                "--profile=Standard"
                "--effort=Profile"
                "--model=Inherit"
                "--context=Point"
                "--authority=Writer"))
        captured)
    (cl-letf (((symbol-function 'transient-args) (lambda (_command) args))
              ((symbol-function 'transient-arg-value)
               (lambda (prefix values)
                 (when-let ((value (seq-find (lambda (arg) (string-prefix-p prefix arg)) values)))
                   (substring value (length prefix)))))
              ((symbol-function 'magpi-spawn-from-options)
               (lambda (options) (setq captured options))))
      (magpi-launch-dispatch))
    (should (equal captured
                   '(:intent "Inspect" :profile "Standard"
                     :effort profile :model "Inherit"
                     :authority writer :context-kind point)))))

(provide 'magpi-transient-tests)
;;; magpi-transient-tests.el ends here
