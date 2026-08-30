;;; magpi-seams.el --- Load real Magit/Transient for seam tests -*- lexical-binding: t; -*-

;; Seam tests refuse stubs.  They exercise the same library contracts the
;; interactive session hits: Transient layout/init and Magit buffer setup.

(require 'cl-lib)
(require 'seq)
(require 'subr-x)

(defconst magpi-seams-required-packages
  '(compat cond-let llama seq transient with-editor magit-section magit)
  "Package directory names under a straight build root.")

(defun magpi-seams--candidate-roots ()
  "Return directories that may hold a straight build tree."
  (let ((roots '())
        (env (getenv "MAGPI_STRAIGHT_BUILD")))
    (when (and env (not (string-empty-p env)))
      (push env roots))
    (dolist (path (list
                   (expand-file-name
                    (format ".emacs.d/.local/straight/build-%s"
                            emacs-version)
                    (or (getenv "HOME") "~"))
                   (expand-file-name
                    ".emacs.d/.local/straight/build-30.2"
                    (or (getenv "HOME") "~"))
                   (expand-file-name
                    ".emacs.d/.local/straight/build"
                    (or (getenv "HOME") "~"))))
      (push path roots))
    (delete-dups (nreverse roots))))

(defun magpi-seams--usable-root (root)
  "Return ROOT when it contains the Magit/Transient package dirs."
  (when (and (stringp root)
             (file-directory-p root)
             (seq-every-p
              (lambda (pkg)
                (file-directory-p (expand-file-name (symbol-name pkg) root)))
              magpi-seams-required-packages))
    root))

(defun magpi-seams-straight-root ()
  "Locate a straight build root that can load real Magit and Transient."
  (seq-some #'magpi-seams--usable-root (magpi-seams--candidate-roots)))

(defun magpi-seams-add-load-path ()
  "Add real package directories to `load-path'.  Signal if unavailable."
  (let ((root (magpi-seams-straight-root)))
    (unless root
      (error "Magpi seam tests need a straight build (set MAGPI_STRAIGHT_BUILD).
Looked in: %s"
             (mapconcat #'identity (magpi-seams--candidate-roots) ", ")))
    (dolist (pkg magpi-seams-required-packages)
      (let ((dir (expand-file-name (symbol-name pkg) root)))
        (add-to-list 'load-path dir)))
    root))

(defvar magpi-seams-loaded nil
  "Non-nil after real Magit/Transient have been required.")

(defun magpi-seams-require ()
  "Load Magit and Transient for real.  Fail hard if they cannot load."
  (or magpi-seams-loaded
      (progn
        (magpi-seams-add-load-path)
        (require 'transient)
        (require 'magit)
        (require 'magit-mode)
        (require 'magit-section)
        (unless (fboundp 'transient-suffixes)
          (error "Seam load produced a stub Transient"))
        (unless (fboundp 'magit-setup-buffer-internal)
          (error "Seam load produced a stub Magit"))
        (setq magpi-seams-loaded t))))

(defun magpi-seams-role-suffix ()
  "Return the live Role switch from `magpi-launch', or nil."
  (seq-find (lambda (suffix)
              (object-of-class-p suffix 'magpi-launch-role-switch))
            (transient-suffixes 'magpi-launch)))

(defun magpi-seams-launch-args ()
  "Return infix values from a fully initialized `magpi-launch' prefix."
  (mapcan (lambda (obj)
            (and (not (oref obj inactive))
                 (not (oref obj inapt))
                 (transient--get-wrapped-value obj)))
          (transient-suffixes 'magpi-launch)))

(provide 'magpi-seams)
;;; magpi-seams.el ends here
