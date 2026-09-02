;;; magpi-store.el --- Repository-private plist records -*- lexical-binding: t; -*-

;; One job: read and write inert Magpi files.  Git names objects; this
;; folder remembers Magpi's pointers.  Unknown keys survive.  Missing
;; fields are absent; this store is local, not a protocol.
(require 'cl-lib)
(require 'subr-x)

(defvar read-eval)
(cl-defstruct magpi-unreadable path error)

(defun magpi-store-unix-time ()
  "Return integer unix time.  Floats are not an interchange grammar."
  (floor (float-time)))

(defun magpi-store-git (directory &rest arguments)
  "Run git ARGUMENTS in DIRECTORY and return trimmed standard output."
  (with-temp-buffer
    (let ((status (apply #'process-file "git" nil (current-buffer) nil
                         "-C" directory arguments))
          output)
      (setq output (string-trim (buffer-string)))
      (unless (zerop status)
        (user-error "Git %s" (if (string-empty-p output) "command failed" output)))
      output)))

(defun magpi-store-git-maybe (directory &rest arguments)
  "Run git ARGUMENTS in DIRECTORY, returning nil on failure."
  (condition-case nil
      (apply #'magpi-store-git directory arguments)
    (error nil)))

(defun magpi-store-toplevel (root)
  "Return the canonical Git working tree containing ROOT."
  (file-name-as-directory
   (magpi-store-git (expand-file-name root) "rev-parse" "--show-toplevel")))

(defun magpi-store-common-dir (root)
  "Return the absolute git-common-dir for ROOT, or nil."
  (let* ((root (and root (ignore-errors (magpi-store-toplevel root))))
         (common (and root (magpi-store-git-maybe
                            root "rev-parse" "--git-common-dir"))))
    (when common
      (file-name-as-directory
       (expand-file-name (if (file-name-absolute-p common)
                             common
                           (expand-file-name common root)))))))

(defun magpi-store-directory (root)
  "Return the repository-private Magpi folder for ROOT, or nil."
  (when-let ((common (magpi-store-common-dir root)))
    (file-name-as-directory (expand-file-name "magpi" common))))

(defun magpi-store-kind-directory (root kind)
  "Return Magpi KIND directory (`intentions' or `actions') for ROOT."
  (when-let ((store (magpi-store-directory root)))
    (file-name-as-directory (expand-file-name (symbol-name kind) store))))

(defun magpi-store-file (root kind id)
  (expand-file-name (concat id ".el") (magpi-store-kind-directory root kind)))

(defun magpi-store-extras (data known)
  "Return DATA keys not in KNOWN, preserving order."
  (let (extras)
    (while data
      (let ((key (pop data))
            (value (pop data)))
        (unless (memq key known)
          (setq extras (nconc extras (list key value))))))
    extras))

(defun magpi-store--omit-nils (plist)
  "Return PLIST without nil values.  Absence is the local grammar."
  (let (out)
    (while plist
      (let ((key (pop plist))
            (value (pop plist)))
        (when value
          (push key out)
          (push value out))))
    (nreverse out)))

(defun magpi-store-plist (named extras)
  "Return NAMED followed by EXTRAS whose keys NAMED does not already hold.
Nil values are omitted."
  (let ((keys (let (keys plist)
                (setq plist named)
                (while plist
                  (push (pop plist) keys)
                  (pop plist))
                keys)))
    (magpi-store--omit-nils
     (append named (magpi-store-extras extras keys)))))

(defun magpi-store-write (file plist)
  "Atomically replace FILE with PLIST.  The temporary lives beside FILE."
  (let ((directory (file-name-directory file))
        temporary)
    (make-directory directory t)
    (setq temporary (make-temp-file
                     (expand-file-name ".#magpi-" directory) nil ".el"))
    (unwind-protect
        (progn
          (with-temp-file temporary
            (let ((print-length nil) (print-level nil) (print-circle nil))
              (prin1 plist (current-buffer))
              (insert "\n")))
          (rename-file temporary file t))
      (when (and temporary (file-exists-p temporary))
        (delete-file temporary)))
    file))

(defun magpi-store-read (file)
  "Read FILE as a plist, or a `magpi-unreadable' value."
  (condition-case err
      (if (not (file-readable-p file))
          (make-magpi-unreadable :path file :error "absent")
        (with-temp-buffer
          (insert-file-contents file)
          (let* ((read-eval nil)
                 (data (read (current-buffer))))
            (skip-chars-forward " \t\n\r")
            (unless (eobp)
              (error "Trailing input"))
            (unless (and (listp data) (keywordp (car-safe data)))
              (error "Not a plist"))
            data)))
    (error (make-magpi-unreadable
            :path file :error (error-message-string err)))))

(defun magpi-store-id-ok (id file)
  "Return non-nil when ID is a path-safe stem equal to FILE's base name."
  (and (stringp id)
       (not (string-empty-p id))
       (not (string-match-p "[/\\\\]" id))
       (equal id (file-name-base file))))

(defun magpi-store-relative (root path)
  "Return PATH relative to ROOT when both are known."
  (and root path (file-relative-name (expand-file-name path)
                                     (file-name-as-directory
                                      (expand-file-name root)))))

(defun magpi-store-absolute (root path)
  "Return PATH as an absolute locator against ROOT.  Absolute PATH is kept."
  (cond
   ((null path) nil)
   ((file-name-absolute-p path) path)
   (root (expand-file-name path (file-name-as-directory
                                 (expand-file-name root))))
   (t path)))

(defun magpi-store-frozen-range (directory oid &optional ancestor-of-head)
  "Return Magit range OID..HEAD in DIRECTORY.

OID is a frozen locator, not a keep-alive.  Missing stays missing.
Standalone Magit doors pass ANCESTOR-OF-HEAD: the oid must still reach HEAD."
  (unless (and (stringp oid) (not (string-empty-p oid)))
    (user-error "Unknown origin"))
  (unless (and directory
               (magpi-store-git-maybe directory "cat-file" "-e" oid))
    (user-error "Origin %s is missing from Git" oid))
  (when ancestor-of-head
    (unless (magpi-store-git-maybe directory "merge-base" "--is-ancestor" oid "HEAD")
      (user-error "Spawn origin is not an ancestor of HEAD")))
  (format "%s..HEAD" oid))

(defun magpi-store-list (root kind)
  "Read every KIND record under ROOT.  Unreadable files stay visible."
  (let ((directory (magpi-store-kind-directory root kind))
        entries)
    (when (file-directory-p directory)
      (dolist (file (directory-files directory t "\\.el\\'"))
        (push (cons file (magpi-store-read file)) entries)))
    (nreverse entries)))

(defun magpi-store-new-id ()
  "Return a short opaque id for a Magpi store record.

Not a UUID library: one entropy source, one length, shared by intention
and action so the grammar does not fork for no reason."
  (substring
   (md5 (format "%s:%s:%s:%s" (float-time) (random) (emacs-pid) (user-uid)))
   0 12))

(provide 'magpi-store)
;;; magpi-store.el ends here
