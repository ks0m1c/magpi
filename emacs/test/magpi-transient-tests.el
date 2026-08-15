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

(ert-deftest magpi-launch-dispatch-passes-only-frozen-launch-choices ()
  (let ((args '("--intent=Repair token refresh"
                "--profile=Quick"
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
                     :authority "Read-only" :context-kind "Region")))))

(provide 'magpi-transient-tests)
;;; magpi-transient-tests.el ends here
