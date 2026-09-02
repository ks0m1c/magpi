;;; magpi-instrument.el --- Batch compile and xref for Magpi -*- lexical-binding: t; -*-

;; Makefile citizen.  Not a Magpi surface.  Not in the installable package.

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'bytecomp)

(defconst magpi-instrument-package-files
  '("emacs/magpi-store.el"
    "emacs/magpi-launch.el"
    "emacs/magpi-backend.el"
    "emacs/magpi-action.el"
    "emacs/magpi-intention.el"
    "emacs/magpi-transient.el"
    "emacs/magpi-pimacs-backend.el"
    "emacs/magpi-status.el"
    "emacs/magpi.el")
  "Compile order: each file may require only earlier Magpi files.")

(defun magpi-instrument-root ()
  (or (let ((env (getenv "MAGPI_ROOT")))
        (and env (not (string-empty-p env)) (file-name-as-directory env)))
      (locate-dominating-file default-directory "Makefile")
      default-directory))

(defun magpi-instrument--rel (file)
  (file-relative-name file (magpi-instrument-root)))

(defun magpi-instrument--el-files (dir)
  (when (file-directory-p dir)
    (seq-filter (lambda (f) (string-suffix-p ".el" f))
                (directory-files dir t "\\.el\\'" t))))

(defun magpi-instrument--corpus-files ()
  (append (mapcar (lambda (rel) (expand-file-name rel (magpi-instrument-root)))
                  magpi-instrument-package-files)
          (magpi-instrument--el-files
           (expand-file-name "emacs/test" (magpi-instrument-root)))))

(defun magpi-instrument--read-forms (file)
  (with-temp-buffer
    (insert-file-contents file)
    (emacs-lisp-mode)
    (goto-char (point-min))
    (let (forms)
      (condition-case nil
          (while t
            (forward-comment (point-max))
            (when (eobp) (signal 'end-of-file nil))
            (push (read (current-buffer)) forms))
        (end-of-file nil))
      (nreverse forms))))

(defun magpi-instrument--struct-type (spec)
  (if (consp spec) (car spec) spec))

(defun magpi-instrument--struct-constructor (spec)
  (cond
   ((not (consp spec))
    (intern (format "make-%s" spec)))
   ((assq :constructor (cdr spec))
    (let ((ctor (cadr (assq :constructor (cdr spec)))))
      (and ctor (not (eq ctor nil)) ctor)))
   (t (intern (format "make-%s" (car spec))))))

(defun magpi-instrument--struct-slots (form)
  (seq-filter #'symbolp
              (seq-drop-while (lambda (x) (or (stringp x) (consp x)))
                              (cddr form))))

(defun magpi-instrument--ours-p (name)
  (and name (symbolp name)
       (let ((s (symbol-name name)))
         (or (string-prefix-p "magpi" s)
             (string-prefix-p "make-magpi" s)
             (string-prefix-p "copy-magpi" s)))))

(defun magpi-instrument--push (acc kind name file &optional extra)
  (if (magpi-instrument--ours-p name)
      (cons (list :kind kind :name name :file file :extra extra) acc)
    acc))

(defun magpi-instrument--walk (form file acc)
  (cond
   ((not (consp form)) acc)
   (t
    (pcase (car form)
      ((or 'defun 'defmacro 'defsubst 'cl-defgeneric)
       (magpi-instrument--push acc (car form) (nth 1 form) file))
      ((or 'defvar 'defvar-local 'defvar-keymap 'defconst 'defcustom 'defface)
       (magpi-instrument--push acc (car form) (nth 1 form) file))
      ('defalias
       (let ((name (nth 1 form)))
         (magpi-instrument--push
          acc 'defalias (if (eq (car-safe name) 'quote) (cadr name) name) file)))
      ('define-derived-mode
       (magpi-instrument--push acc 'define-derived-mode (nth 1 form) file))
      ((or 'defclass 'cl-defclass)
       (magpi-instrument--push acc 'defclass (nth 1 form) file))
      ('cl-defstruct
       (let* ((spec (nth 1 form))
              (type (magpi-instrument--struct-type spec))
              (ctor (magpi-instrument--struct-constructor spec)))
         (setq acc (magpi-instrument--push acc 'cl-defstruct type file))
         (when ctor
           (setq acc (magpi-instrument--push acc 'constructor ctor file type)))
         (setq acc (magpi-instrument--push
                    acc 'predicate (intern (format "%s-p" type)) file type))
         (setq acc (magpi-instrument--push
                    acc 'copier (intern (format "copy-%s" type)) file type))
         (dolist (slot (magpi-instrument--struct-slots form))
           (setq acc (magpi-instrument--push
                      acc 'slot (intern (format "%s-%s" type slot)) file type)))
         acc))
      ((or 'progn 'eval-and-compile 'eval-when-compile 'prog1 'prog2)
       (dolist (x (cdr form))
         (setq acc (magpi-instrument--walk x file acc)))
       acc)
      ((or 'when 'unless 'if)
       (dolist (x (cddr form))
         (setq acc (magpi-instrument--walk x file acc)))
       acc)
      (_ acc)))))

(defun magpi-instrument--definitions ()
  (let (acc)
    (dolist (rel magpi-instrument-package-files)
      (let ((file (expand-file-name rel (magpi-instrument-root))))
        (dolist (form (magpi-instrument--read-forms file))
          (setq acc (magpi-instrument--walk form file acc)))))
    (nreverse acc)))

(defun magpi-instrument--corpus ()
  (mapconcat (lambda (file)
               (with-temp-buffer
                 (insert-file-contents file)
                 (buffer-string)))
             (magpi-instrument--corpus-files)
             "\n"))

(defun magpi-instrument--count (name)
  "Count NAME in the current buffer using Elisp symbol syntax."
  (let ((re (concat "\\_<" (regexp-quote (symbol-name name)) "\\_>"))
        (n 0))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward re nil t)
        (setq n (1+ n))))
    n))

(defun magpi-instrument--keymap-commands (map)
  (let (cmds)
    (when (keymapp map)
      (map-keymap
       (lambda (_key bind)
         (cond
          ((keymapp bind)
           (setq cmds (nconc cmds (magpi-instrument--keymap-commands bind))))
          ((and (symbolp bind) (commandp bind t))
           (push bind cmds))))
       map))
    cmds))

(defun magpi-instrument--bound-commands ()
  (delete-dups
   (append (magpi-instrument--keymap-commands magpi-command-map)
           (and (boundp 'magpi-status-mode-map)
                (magpi-instrument--keymap-commands magpi-status-mode-map)))))

(defun magpi-instrument--elc-dir ()
  (expand-file-name "emacs/.elc" (magpi-instrument-root)))

(defvar magpi-instrument--warning-lines nil)
(defvar magpi-instrument--magpi-warnings 0)

(defun magpi-instrument--warning-class (line)
  (cond
   ((string-match-p "eieio-childp\\|eieio-class-children" line) 'generated)
   ((string-match-p "interactive use only" line) 'transient-prefix)
   ((string-match-p "rxt--\\|pcre2el" line) 'prior)
   (t 'magpi)))

(defun magpi-instrument--report-warnings ()
  "Print classified compile warnings.  Return Magpi-owned warning count."
  (let ((grouped (list (cons 'magpi nil)
                       (cons 'transient-prefix nil)
                       (cons 'generated nil)
                       (cons 'prior nil)))
        (magpi 0))
    (dolist (line (nreverse magpi-instrument--warning-lines))
      (let ((class (magpi-instrument--warning-class line)))
        (when (eq class 'magpi) (setq magpi (1+ magpi)))
        (let ((cell (assq class grouped)))
          (when cell (setcdr cell (nconc (cdr cell) (list line)))))))
    (princ "== compile warnings ==\n")
    (dolist (class '(magpi transient-prefix generated prior))
      (let ((lines (cdr (assq class grouped))))
        (princ (format "%s: %d\n" class (length lines)))
        (dolist (line lines)
          (princ (format "  %s\n" line)))))
    (setq magpi-instrument--magpi-warnings magpi)
    magpi))

(defun magpi-instrument-exit-code (errors)
  (if (or (> errors 0) (> magpi-instrument--magpi-warnings 0)) 1 0))

(defun magpi-instrument-compile ()
  "Byte-compile package files.  Warnings go to stderr and the report."
  (let* ((root (magpi-instrument-root))
         (elc-dir (magpi-instrument--elc-dir))
         (byte-compile-error-on-warn nil)
         (byte-compile-verbose nil)
         (byte-compile-warnings '(not docstrings))
         (errors 0)
         (log (get-buffer-create "*Compile-Log*")))
    (setq magpi-instrument--warning-lines nil
          magpi-instrument--magpi-warnings 0)
    (when (file-directory-p elc-dir)
      (dolist (old (directory-files elc-dir t "\\.elc\\'" t))
        (delete-file old)))
    (make-directory elc-dir t)
    (princ (format "# compile  root=%s\n" root))
    (dolist (rel magpi-instrument-package-files)
      (let* ((file (expand-file-name rel root))
             (dest (expand-file-name (concat (file-name-base rel) ".elc")
                                     elc-dir))
             (byte-compile-dest-file-function (lambda (_) dest)))
        (with-current-buffer log (erase-buffer))
        (unless (byte-compile-file file)
          (setq errors (1+ errors)))
        (with-current-buffer log
          (dolist (line (split-string (buffer-string) "\n" t))
            (when (string-match-p "Warning:" line)
              (push (string-trim line) magpi-instrument--warning-lines))))))
    (princ (format "compile: %d error%s\n" errors (if (= errors 1) "" "s")))
    (magpi-instrument--report-warnings)
    errors))

(defun magpi-instrument--isolation ()
  (princ (format "isolation: require magpi => status=%s backend=%s\n"
                 (if (featurep 'magpi-status) "loaded" "nil")
                 (if (featurep 'magpi-pimacs-backend) "loaded" "nil"))))

(defun magpi-instrument-load ()
  (require 'magpi)
  (magpi-instrument--isolation)
  (require 'magpi-status)
  t)

(defun magpi-instrument--kind-rank (kind)
  (or (alist-get kind
                 '((defun . 0) (defmacro . 0) (defsubst . 0)
                   (cl-defgeneric . 1) (define-derived-mode . 2)
                   (defcustom . 3) (defvar . 4) (defvar-local . 4)
                   (defvar-keymap . 4) (defconst . 4) (defalias . 5)
                   (defface . 6) (defclass . 7) (cl-defstruct . 8)
                   (constructor . 9) (predicate . 9) (copier . 9)
                   (slot . 10)))
      50))

(defun magpi-instrument-xref ()
  "Print unused Magpi symbols and unbound interactive commands."
  (with-temp-buffer
    (delay-mode-hooks (emacs-lisp-mode))
    (magpi-instrument--xref-1)))

(defun magpi-instrument--xref-1 ()
  (let* ((defs (magpi-instrument--definitions))
         (bound (magpi-instrument--bound-commands))
         (seen (make-hash-table :test #'eq))
         unused unbound)
    (erase-buffer)
    (insert (magpi-instrument--corpus))
    (princ (format "# xref  defs=%d files=%d\n"
                   (length defs)
                   (length magpi-instrument-package-files)))
    (dolist (def defs)
      (let ((name (plist-get def :name)))
        (unless (gethash name seen)
          (puthash name t seen)
          (let* ((kind (plist-get def :kind))
                 (n (magpi-instrument--count name))
                 (private (string-match-p "--" (symbol-name name)))
                 (cmd (and (eq kind 'defun) (commandp name t) (not private))))
            (when (and (if (memq kind '(slot constructor copier predicate))
                           (zerop n)
                         (<= n 1))
                       (not (memq kind '(cl-defstruct copier predicate))))
              (push (cons n def) unused))
            (when (and cmd (not (memq name bound)) (<= n 1))
              (push (list name (plist-get def :file) n) unbound))))))
    (setq unused (sort unused
                       (lambda (a b)
                         (let ((ka (magpi-instrument--kind-rank
                                    (plist-get (cdr a) :kind)))
                               (kb (magpi-instrument--kind-rank
                                    (plist-get (cdr b) :kind))))
                           (or (< ka kb)
                               (and (= ka kb)
                                    (string< (symbol-name (plist-get (cdr a) :name))
                                             (symbol-name (plist-get (cdr b) :name)))))))))
    (setq unbound (sort unbound (lambda (a b) (string< (symbol-name (car a))
                                                       (symbol-name (car b))))))
    (princ "== unused (defined, never referenced in emacs/ or tests) ==\n")
    (if (null unused)
        (princ "(none)\n")
      (dolist (item unused)
        (let* ((def (cdr item))
               (kind (plist-get def :kind))
               (name (plist-get def :name))
               (file (magpi-instrument--rel (plist-get def :file)))
               (extra (plist-get def :extra)))
          (princ (format "%-8s %-42s %s%s\n"
                         kind
                         name
                         file
                         (if extra (format "  [%s]" extra) ""))))))
    (princ "== interactive, unused, not on Magpi keymaps ==\n")
    (if (null unbound)
        (princ "(none)\n")
      (dolist (item unbound)
        (princ (format "%-42s %s  count=%d\n"
                       (nth 0 item)
                       (magpi-instrument--rel (nth 1 item))
                       (nth 2 item)))))
    (princ (format "cleanup-candidates: unused=%d unbound-interactive=%d\n"
                   (length unused) (length unbound)))))

(defun magpi-instrument ()
  "Compile, load, xref.  Exit 1 on compile errors or Magpi-owned warnings."
  (let ((errors (magpi-instrument-compile)))
    (princ "\n")
    (condition-case err
        (magpi-instrument-load)
      (error (princ (format "load failed: %S\n" err))))
    (princ "\n")
    (magpi-instrument-xref)
    (kill-emacs (magpi-instrument-exit-code errors))))

(provide 'magpi-instrument)
;;; magpi-instrument.el ends here
