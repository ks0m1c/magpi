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
              "--role=w"
              "--lease"))
           '(:thinking nil :model "Inherit"
             :role writer :bind point :lease t))))

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

(ert-deftest magpi-launch-options-include-selected-intention ()
  "Promise: Transient scope becomes spawn membership, not a menu label."
  (cl-letf (((symbol-function 'transient-scope) (lambda (&rest _) "intent-1")))
    (should (equal (plist-get (magpi-launch--options-from-args
                               '("--thinking=Default" "--model=Inherit"
                                 "--context=None" "--role=w"))
                              :intention-id)
                   "intent-1")))
  (cl-letf (((symbol-function 'transient-scope) (lambda (&rest _) nil)))
    (should-not (plist-member (magpi-launch--options-from-args
                               '("--thinking=Default" "--model=Inherit"
                                 "--context=None" "--role=w"))
                              :intention-id))))
(ert-deftest magpi-launch-dispatch-refuses-missing-executor ()
  "Promise: missing orchestration is a user error, not void-function."
  (let ((magpi-launch-execute-function nil)
        (args '("--thinking=Default" "--model=Inherit"
                "--context=None" "--role=w")))
    (cl-letf (((symbol-function 'transient-args) (lambda (_command) args))
              ((symbol-function 'require) (lambda (&rest _) nil)))
      (should-error (magpi-launch-dispatch) :type 'user-error))))


(ert-deftest magpi-launch-loaddefs-autoload-not-prefix ()
  "Promise: loaddefs records an autoload, not the Transient prefix form.

Doom evaluates magpi-autoloads.el before Transient is loaded.  A copied
`transient-define-prefix' is void-function during `doom build'."
  (require 'loaddefs-gen)
  (let* ((source (find-library-name "magpi-transient"))
         (dir (make-temp-file "magpi-loaddefs" t))
         (out (expand-file-name "magpi-autoloads.el" dir)))
    (unwind-protect
        (progn
          (copy-file source (expand-file-name "magpi-transient.el" dir))
          (loaddefs-generate dir out)
          (with-temp-buffer
            (insert-file-contents out)
            (goto-char (point-min))
            (should (search-forward "(autoload 'magpi-launch" nil t))
            (goto-char (point-min))
            (should-not (search-forward "transient-define-prefix" nil t))))
      (delete-directory dir t))))

(provide 'magpi-transient-tests)
;;; magpi-transient-tests.el ends here
