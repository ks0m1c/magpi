;;; magpi-transient-tests.el --- Launch option contracts -*- lexical-binding: t; -*-

;; Observable contract: menu labels become semantic spawn options.
;; Real Transient layout lives in seam tests.

(require 'cl-lib)
(require 'ert)
(require 'seq)

(when (and (featurep 'transient)
           (not (fboundp 'transient--set-layout)))
  (unload-feature 'transient t))
(unless (and (require 'transient nil t)
             (fboundp 'transient--set-layout)
             (get 'transient-switches 'cl--class))
  (when (featurep 'transient)
    (unload-feature 'transient t))
  (dolist (sym '(transient-infix-read transient-switches transient--set-layout
                 transient-args transient-arg-value))
    (fmakunbound sym)
    (makunbound sym))
  (defmacro transient-define-prefix (name _arguments &rest _body)
    `(defun ,name () (interactive)))
  (defun transient-args (_command) nil)
  (defun transient-arg-value (prefix values)
    (when-let ((value (seq-find (lambda (arg) (string-prefix-p prefix arg)) values)))
      (substring value (length prefix))))
  (provide 'transient))

(require 'magpi-transient)

(ert-deftest magpi-launch-options-map-labels-to-semantics ()
  "Promise: thinking/model/role/context labels become domain values."
  (should (equal
           (magpi-launch--options-from-args
            '("--thinking=Deep"
              "--model=openai/gpt-4.1"
              "--context=Region"
              "--role=r"))
           '(:thinking high :model "openai/gpt-4.1"
             :role reader :bind region)))
  (should (equal
           (magpi-launch--options-from-args
            '("--thinking=Default"
              "--model=Inherit"
              "--context=Point"
              "--role=w"))
           '(:thinking nil :model "Inherit"
             :role writer :bind point))))

(ert-deftest magpi-launch-dispatch-delivers-options-to-executor ()
  "Promise: dispatch hands the semantic plist to the registered executor."
  (let* ((args '("--thinking=Deep"
                 "--model=openai/gpt-4.1"
                 "--context=None"
                 "--role=w"))
         (captured nil)
         (magpi-launch-execute-function
          (lambda (options) (setq captured options))))
    (cl-letf (((symbol-function 'transient-args) (lambda (_command) args)))
      (magpi-launch-dispatch))
    (should (equal captured
                   '(:thinking high :model "openai/gpt-4.1"
                     :role writer :bind none)))))

(ert-deftest magpi-launch-dispatch-refuses-missing-executor ()
  "Promise: missing orchestration is a user error, not void-function."
  (let ((magpi-launch-execute-function nil)
        (args '("--thinking=Default" "--model=Inherit"
                "--context=None" "--role=w")))
    (cl-letf (((symbol-function 'transient-args) (lambda (_command) args))
              ((symbol-function 'require) (lambda (&rest _) nil)))
      (should-error (magpi-launch-dispatch) :type 'user-error))))

(provide 'magpi-transient-tests)
;;; magpi-transient-tests.el ends here
