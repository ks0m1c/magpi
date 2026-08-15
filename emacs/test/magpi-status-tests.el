;;; magpi-status-tests.el --- Tests for Magpi's Magit adapter -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)

;; The core suite intentionally runs without Doom's package load-path.  Supply
;; the narrow Magit contract needed by this adapter when Magit is unavailable;
;; with Magit installed the real implementation is used instead.
(unless (require 'magit-mode nil t)
  (defvar magit-mode-map (make-sparse-keymap))
  (define-derived-mode magit-mode fundamental-mode "Magit")
  (defun magit-refresh-buffer () nil)
  (defun magit-mode-bury-buffer () nil)
  (defmacro magit-setup-buffer (&rest _arguments) nil)
  (provide 'magit-mode))

(unless (require 'magit-section nil t)
  (defun magit-section-value-if (_type) nil)
  (defun magit-current-section () nil)
  (defun magit-section-toggle (_section) nil)
  (defmacro magit-insert-section (&rest body) `(progn ,@(cdr body)))
  (defun magit-insert-heading (&rest _arguments) nil)
  (provide 'magit-section))

(require 'magpi-status)

(defmacro magpi-test-with-section-values (values &rest body)
  `(let ((values ,values))
     (cl-letf (((symbol-function 'magit-section-value-if)
                (lambda (type) (cdr (assq type values)))))
       ,@body)))

(ert-deftest magpi-status-target-is-derived-from-section-identity ()
  (magpi-test-with-section-values
   '((magpi-observed-file . ("attempt-1" . "lib/auth.ex")))
   (should (equal (magpi-status-target-at-point)
                  '(:kind observed-file
                    :attempt-id "attempt-1"
                    :path "lib/auth.ex")))))

(ert-deftest magpi-status-attempt-target-is-not-parsed-from-heading-text ()
  (magpi-test-with-section-values
   '((magpi-attempt . "attempt-1"))
   (should (equal (magpi-status-target-at-point)
                  '(:kind attempt :attempt-id "attempt-1")))))

(ert-deftest magpi-status-visit-dispatches-the-exact-typed-target ()
  (let (seen)
    (with-temp-buffer
      (magpi-status-mode)
      (setq-local magpi-status-visit-function (lambda (target) (setq seen target)))
      (magpi-test-with-section-values
       '((magpi-attempt . "attempt-1"))
       (magpi-status-visit-at-point)))
    (should (equal seen '(:kind attempt :attempt-id "attempt-1")))))

(ert-deftest magpi-status-renders-absent-intention-as-a-quiet-placeholder ()
  (let* ((attempt (make-magpi-attempt :id "attempt-1" :intent ""))
         (placeholder (magpi-status--intent attempt)))
    (should (equal placeholder "◯"))
    (should (eq (get-text-property 0 'face placeholder) 'shadow))))

(provide 'magpi-status-tests)
;;; magpi-status-tests.el ends here
