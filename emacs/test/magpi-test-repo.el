;;; magpi-test-repo.el --- Real Git fixtures with retain-on-failure -*- lexical-binding: t; -*-

;; Domain stays real.  Environment is neutralized.  Failures keep evidence.

(require 'cl-lib)
(require 'ert)

(defvar magpi-test-repo--counter 0)

(defun magpi-test-repo--run (directory &rest args)
  "Run Git ARGS in DIRECTORY with a neutralized identity and config."
  (let ((process-environment
         (append
          (list "GIT_CONFIG_NOSYSTEM=1"
                "GIT_CONFIG_GLOBAL=/dev/null"
                "GIT_CONFIG_SYSTEM=/dev/null"
                "GIT_AUTHOR_NAME=Magpi Test"
                "GIT_AUTHOR_EMAIL=magpi-test@example.invalid"
                "GIT_COMMITTER_NAME=Magpi Test"
                "GIT_COMMITTER_EMAIL=magpi-test@example.invalid"
                "GIT_AUTHOR_DATE=2000-01-01T00:00:00"
                "GIT_COMMITTER_DATE=2000-01-01T00:00:00")
          process-environment)))
    (with-temp-buffer
      (let ((status (apply #'process-file "git" nil (current-buffer) nil
                           "-C" directory args)))
        (unless (zerop status)
          (error "git %s failed in %s:\n%s"
                 (mapconcat #'identity args " ")
                 directory
                 (buffer-string)))
        (buffer-string)))))

(defun magpi-test-repo-create (&optional prefix)
  "Create an isolated temporary Git repository and return its path.
Identity, system config, and initial branch are neutralized."
  (let* ((root (make-temp-file (or prefix "magpi-test-repo-") t))
         (process-environment
          (append '("GIT_CONFIG_NOSYSTEM=1"
                    "GIT_CONFIG_GLOBAL=/dev/null"
                    "GIT_CONFIG_SYSTEM=/dev/null")
                  process-environment)))
    (magpi-test-repo--run root "init" "-b" "master")
    (magpi-test-repo--run root "config" "user.email" "magpi-test@example.invalid")
    (magpi-test-repo--run root "config" "user.name" "Magpi Test")
    (with-temp-file (expand-file-name "README" root)
      (insert "base\n"))
    (magpi-test-repo--run root "add" "README")
    (magpi-test-repo--run root "commit" "-m" "base")
    root))

(defmacro magpi-test-with-repo (spec &rest body)
  "Bind a neutralized temporary Git repository around BODY.

SPEC is (ROOT-VAR) or (ROOT-VAR PREFIX).
On success the repository is deleted.  On failure it is retained and its
path is reported before the error is re-signalled."
  (declare (indent 1) (debug ((symbolp &optional stringp) body)))
  (let ((root (car spec))
        (prefix (or (cadr spec) "magpi-test-repo-"))
        (ok (make-symbol "ok")))
    `(let* ((,root (magpi-test-repo-create ,prefix))
            (,ok nil))
       (unwind-protect
           (prog1 (progn ,@body)
             (setq ,ok t))
         (cond
          (,ok
           (when (file-directory-p ,root)
             (delete-directory ,root t)))
          ((file-directory-p ,root)
           (message "Magpi test evidence retained: %s" ,root)))))))

(defalias 'magpi-test-repo-git #'magpi-test-repo--run)

(provide 'magpi-test-repo)
;;; magpi-test-repo.el ends here
