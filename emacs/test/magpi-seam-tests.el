;;; magpi-seam-tests.el --- Contract tests against real Magit/Transient -*- lexical-binding: t; -*-

;; Test Magpi's promises at the porcelain seams.
;; Domain and host libraries stay real.  Environment quirks are neutralized.
;; Do not assert internal call routes (require, callback symbols, hook lists).

(require 'ert)
(require 'cl-lib)
(require 'seq)

(require 'magpi-seams)
(magpi-seams-require)

(require 'magpi-launch)
(require 'magpi-transient)
(require 'magpi-status)
(require 'magpi-test-repo)

;;;; Launch menu — declarative model contract

(ert-deftest magpi-seam-launch-exposes-frozen-spec-axes ()
  "Promise: launch freezes model, thinking, role, and context.
Role is always present as w|r (never unset, never the symbol quote)."
  (let* ((args (magpi-seams-launch-args))
         (suffix (magpi-seams-role-suffix))
         (rendered (and suffix (transient-format-value suffix))))
    (should (stringp (transient-arg-value "--model=" args)))
    (should (stringp (transient-arg-value "--thinking=" args)))
    (should (stringp (transient-arg-value "--context=" args)))
    (should (member (transient-arg-value "--role=" args) '("w" "r")))
    (should (member (format "--role=%s"
                            (transient-arg-value "--role=" args))
                    args))
    (should suffix)
    (should (equal (oref suffix choices) '("w" "r")))
    (should (seq-every-p #'stringp (oref suffix choices)))
    (should (stringp rendered))
    (should-not (string-match-p "quote" rendered))
    (should (string-match-p "w" rendered))
    (should (string-match-p "r" rendered))))

(ert-deftest magpi-seam-launch-role-stays-binary-under-init ()
  "Promise: Role init from a prefix value always lands on w or r."
  (let* ((suffix (magpi-seams-role-suffix))
         (transient--prefix (get 'magpi-launch 'transient--prefix)))
    (should suffix)
    (dolist (arg '("--role=w" "--role=r"))
      (oset transient--prefix value (list arg))
      (transient-init-value suffix)
      (should (member (oref suffix value) '("--role=w" "--role=r")))
      (should (equal (oref suffix value) arg)))))

(ert-deftest magpi-seam-launch-dispatch-hands-semantic-options ()
  "Promise: dispatch turns live menu values into semantic spawn options."
  (let* ((args (magpi-seams-launch-args))
         (captured nil)
         (magpi-launch-execute-function
          (lambda (options) (setq captured options))))
    (cl-letf (((symbol-function 'transient-args) (lambda (_command) args)))
      (magpi-launch-dispatch))
    (should (stringp (plist-get captured :model)))
    (should (memq (plist-get captured :role) '(writer reader)))
    (should (memq (plist-get captured :bind) '(none point region)))
    (should (plist-member captured :thinking))
    ;; Labels never leak into the semantic contract.
    (should-not (stringp (plist-get captured :role)))
    (should-not (stringp (plist-get captured :thinking)))))

(ert-deftest magpi-seam-launch-dispatch-errors-without-orchestration ()
  "Promise: spawn is refused with a user error, never void-function."
  (let ((magpi-launch-execute-function nil)
        (args (magpi-seams-launch-args)))
    (cl-letf (((symbol-function 'transient-args) (lambda (_command) args))
              ;; Neutralize package load: the observable is the user error.
              ((symbol-function 'require) (lambda (&rest _) nil)))
      (should-error (magpi-launch-dispatch) :type 'user-error))))

;;;; Status — Magit-backed dashboard contract

(ert-deftest magpi-seam-status-opens-on-real-repository ()
  "Promise: status opens a Magpi buffer over a real Git root without void hooks."
  (magpi-test-with-repo (root)
    (let (buffer)
      (unwind-protect
          (progn
            (setq buffer
                  (save-window-excursion
                    (magpi-status-open
                     root
                     (lambda () nil)
                     (lambda () nil)
                     (lambda (_target) nil)
                     (lambda (_target) nil))
                    (current-buffer)))
            (with-current-buffer buffer
              (should (derived-mode-p 'magpi-status-mode))
              (should (equal magpi-status-root
                             (file-name-as-directory
                              (expand-file-name root))))
              (should (string-match-p "MAGPI"
                                      (buffer-substring-no-properties
                                       (point-min) (min (point-max) 200))))))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))

(ert-deftest magpi-seam-status-create-collects-intention-text ()
  "Promise: status create yields the authored intention string to the owner."
  (magpi-test-with-repo (root)
    (let* ((collected nil)
           (buffer nil))
      (unwind-protect
          (progn
            (setq buffer
                  (save-window-excursion
                    (magpi-status-open
                     root
                     (lambda () nil)
                     (lambda () nil)
                     (lambda (_target) nil)
                     (lambda (_target) nil)
                     nil nil nil nil
                     (lambda (intent)
                       (interactive (list (read-string "Intention: ")))
                       (setq collected intent)))
                    (current-buffer)))
            (with-current-buffer buffer
              (cl-letf (((symbol-function 'read-string)
                         (lambda (&rest _) "Repair auth")))
                (magpi-status-create))
              (should (equal collected "Repair auth"))))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))

(ert-deftest magpi-seam-status-create-requires-owner ()
  "Promise: create without an owner is a user error, not a void call."
  (let ((magpi-status-create-function nil))
    (should-error (magpi-status-create) :type 'user-error)))

(provide 'magpi-seam-tests)
;;; magpi-seam-tests.el ends here
