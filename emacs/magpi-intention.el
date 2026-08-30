;;; magpi-intention.el --- Intention being: why, change, lease, audit -*- lexical-binding: t; -*-

;; An intention is why: durable objective, change, lease, and audit.  Actions reference it.

(require 'cl-lib)
(require 'subr-x)

(defconst magpi-intention-states '(active merged discarded)
  "Valid persisted intention state values.")

(defconst magpi-intention-state-transitions
  '((active . (merged discarded)))
  "Explicit intention transitions; merged and discarded are terminal.")

(cl-defstruct magpi-intention
  id objective bindings worktree-path branch base-ref state action-ids writer-lease
  audit source-root created-at)

(defun magpi-git (directory &rest arguments)
  "Run git ARGUMENTS in DIRECTORY and return trimmed standard output."
  (with-temp-buffer
    (let ((status (apply #'process-file "git" nil (current-buffer) nil
                         "-C" directory arguments))
          output)
      (setq output (string-trim (buffer-string)))
      (unless (zerop status)
        (user-error "Git %s" (if (string-empty-p output) "command failed" output)))
      output)))

(defun magpi-git--maybe (directory &rest arguments)
  "Run git ARGUMENTS in DIRECTORY, returning nil on failure."
  (condition-case nil
      (apply #'magpi-git directory arguments)
    (error nil)))

(defun magpi-git--repository-root (root)
  "Return the canonical Git repository root containing ROOT."
  (file-name-as-directory
   (magpi-git (expand-file-name root) "rev-parse" "--show-toplevel")))

(defun magpi-intention--store-directory (root)
  "Return the repository-private intention store for ROOT."
  (let* ((repository (magpi-git--repository-root root))
         (common (magpi-git repository "rev-parse" "--git-common-dir")))
    (file-name-as-directory
     (expand-file-name "magpi/intentions"
                       (if (file-name-absolute-p common)
                           common
                         (expand-file-name common repository))))))

(defun magpi-intention--file (root id)
  (expand-file-name (concat id ".el") (magpi-intention--store-directory root)))

(defun magpi-intention--plist (intention)
  "Return INTENTION as inert persisted data."
  (list :version 1
        :id (magpi-intention-id intention)
        :objective (magpi-intention-objective intention)
        :bindings (magpi-intention-bindings intention)
        :worktree-path (magpi-intention-worktree-path intention)
        :branch (magpi-intention-branch intention)
        :base-ref (magpi-intention-base-ref intention)
        :state (magpi-intention-state intention)
        :action-ids (magpi-intention-action-ids intention)
        :writer-lease (magpi-intention-writer-lease intention)
        :audit (magpi-intention-audit intention)
        :source-root (magpi-intention-source-root intention)
        :created-at (magpi-intention-created-at intention)))

(defun magpi-intention--from-plist (data)
  "Decode and validate persisted intention DATA."
  (let* ((state (or (plist-get data :state) (plist-get data :lifecycle)))
         (action-ids (or (plist-get data :action-ids) (plist-get data :task-ids)))
         (lease (plist-get data :writer-lease))
         (lease (cond
                 ((and lease (plist-get lease :action-id)) lease)
                 ((and lease (plist-get lease :task-id))
                  (plist-put (copy-sequence lease) :action-id
                             (plist-get lease :task-id)))
                 (t lease))))
    (unless (and (equal (plist-get data :version) 1)
                 (stringp (plist-get data :id))
                 (stringp (plist-get data :objective))
                 (memq state magpi-intention-states))
      (error "Invalid Magpi intention record"))
    (make-magpi-intention
     :id (plist-get data :id)
     :objective (plist-get data :objective)
     :bindings (or (plist-get data :bindings) (plist-get data :attachments))
     :worktree-path (plist-get data :worktree-path)
     :branch (plist-get data :branch)
     :base-ref (plist-get data :base-ref)
     :state state
     :action-ids action-ids
     :writer-lease lease
     :audit (or (plist-get data :audit)
                ;; Read prototype records once; the next save writes one audit.
                (append (plist-get data :events) (plist-get data :receipts)))
     :source-root (plist-get data :source-root)
     :created-at (plist-get data :created-at))))

(defun magpi-intention-save (intention)
  "Atomically persist INTENTION in its repository metadata."
  (let* ((directory (magpi-intention--store-directory
                     (magpi-intention-source-root intention)))
         (file (magpi-intention--file (magpi-intention-source-root intention)
                                     (magpi-intention-id intention)))
         (temporary (make-temp-file "magpi-intention-" nil ".el")))
    (make-directory directory t)
    (unwind-protect
        (progn
          (with-temp-file temporary
            (let ((print-length nil) (print-level nil))
              (prin1 (magpi-intention--plist intention) (current-buffer))
              (insert "\n")))
          (rename-file temporary file t))
      (when (file-exists-p temporary) (delete-file temporary)))
    intention))

(defun magpi-intention-load (root id)
  "Load persisted intention ID belonging to ROOT."
  (let ((file (magpi-intention--file root id)))
    (when (file-readable-p file)
      (with-temp-buffer
        (insert-file-contents file)
        (let ((read-eval nil))
          (magpi-intention--from-plist (read (current-buffer))))))))

(defun magpi-intention-list (root)
  "Return all valid persisted intentions belonging to ROOT."
  (let ((directory (magpi-intention--store-directory root)) intentions)
    (when (file-directory-p directory)
      (dolist (file (directory-files directory t "\\.el\\'"))
        (condition-case nil
            (with-temp-buffer
              (insert-file-contents file)
              (let ((read-eval nil))
                (push (magpi-intention--from-plist (read (current-buffer))) intentions)))
          (error nil))))
    (sort intentions (lambda (a b)
                       (> (or (magpi-intention-created-at a) 0)
                          (or (magpi-intention-created-at b) 0))))))

(defun magpi-intention--append (intention type &rest properties)
  "Return a copy of INTENTION with one timestamped audit entry appended."
  (let ((next (copy-magpi-intention intention))
        (entry (append (list :type type :at (float-time)) properties)))
    (setf (magpi-intention-audit next)
          (append (magpi-intention-audit intention) (list entry)))
    next))

(defun magpi-intention--slug (objective)
  (let ((slug (downcase (replace-regexp-in-string
                         "[^[:alnum:]]+" "-" (or objective "work")))))
    (setq slug (string-trim slug "-+" "-+"))
    (if (string-empty-p slug) "work" (substring slug 0 (min 32 (length slug))))))

(defun magpi-intention--new-id ()
  (substring (md5 (format "%s:%s:%s" (float-time) (random) (emacs-pid))) 0 10))

(defun magpi-intention-create-record (objective root &optional id)
  "Create and persist a lightweight active intention for OBJECTIVE at ROOT.

Creating an intention only records the authored objective.  Its Git worktree is
created lazily by `magpi-intention-ensure-worktree' when work actually starts."
  (let ((intention (make-magpi-intention
                    :id (or id (magpi-intention--new-id))
                    :objective objective :bindings nil
                    :worktree-path nil :branch nil :base-ref nil :state 'active
                    :action-ids nil :writer-lease nil :audit nil
                    :source-root (magpi-git--repository-root root)
                    :created-at (float-time))))
    (setq intention (magpi-intention--append intention 'created))
    (magpi-intention-save intention)))

(defun magpi-intention-ensure-worktree (intention)
  "Create INTENTION's managed worktree only when a task needs it."
  (unless (eq (magpi-intention-state intention) 'active)
    (user-error "Intention %s is %s" (magpi-intention-id intention)
                (magpi-intention-state intention)))
  (cond
   ((and (magpi-intention-worktree-path intention)
         (file-directory-p (magpi-intention-worktree-path intention))) intention)
   ((magpi-intention-worktree-path intention)
    (user-error "Magpi intention worktree is missing: %s"
                (magpi-intention-worktree-path intention)))
   (t
    (let* ((source-root (magpi-intention-source-root intention))
           (id (magpi-intention-id intention))
           (base-ref (or (magpi-git--maybe
                         source-root "symbolic-ref" "--quiet" "--short" "HEAD")
                        "HEAD"))
           (branch (format "magpi/%s-%s"
                           (magpi-intention--slug
                            (magpi-intention-objective intention)) id))
           (path (expand-file-name
                  (format ".magpi-worktrees/%s-%s"
                          (file-name-nondirectory
                           (directory-file-name source-root)) id)
                  (file-name-directory (directory-file-name source-root)))))
      (when (file-exists-p path)
        (user-error "Magpi intention path already exists: %s" path))
      (make-directory (file-name-directory path) t)
      (magpi-git source-root "worktree" "add" "-b" branch path "HEAD")
      (let ((next (copy-magpi-intention intention)))
        (setf (magpi-intention-worktree-path next) (file-name-as-directory path)
              (magpi-intention-branch next) branch
              (magpi-intention-base-ref next) base-ref)
        (setq next (magpi-intention--append next 'worktree-created :path path
                                             :base-ref base-ref :branch branch))
        (magpi-intention-save next))))))

(defun magpi-intention-bind (intention kind reference &optional label tags)
  "Bind KIND and REFERENCE to INTENTION, optionally labelled and TAGS.

A binding is a durable reference only: Magpi never copies its contents into
the intention record or sends it as task prose."
  (unless (memq kind '(file chat point region))
    (user-error "Unknown Magpi bind kind: %S" kind))
  (unless (and (stringp reference) (not (string-empty-p (string-trim reference))))
    (user-error "A Magpi binding needs a reference"))
  (let* ((tags (seq-uniq (seq-filter (lambda (tag)
                                      (and (stringp tag)
                                           (not (string-empty-p (string-trim tag)))))
                                    tags)
                         #'string=))
         (existing (seq-find (lambda (binding)
                               (and (eq (plist-get binding :kind) kind)
                                    (equal (plist-get binding :reference) reference)))
                             (magpi-intention-bindings intention)))
         (label (let ((label (or label (plist-get existing :label))))
                  (and (stringp label)
                       (not (string-empty-p (string-trim label)))
                       label)))
         (combined-tags (seq-uniq (append (plist-get existing :tags) tags) #'string=))
         (binding (append (list :kind kind :reference reference)
                             (when label (list :label label))
                             (when combined-tags (list :tags combined-tags)))))
    (let ((next (copy-magpi-intention intention)))
      (if existing
          (setf (magpi-intention-bindings next)
                (mapcar (lambda (item)
                          (if (eq item existing) binding item))
                        (magpi-intention-bindings intention)))
        (setf (magpi-intention-bindings next)
              (append (magpi-intention-bindings intention) (list binding))))
      (setq next
            (magpi-intention--append next
                                    (if existing 'bind-tagged 'bind-added)
                                    :kind kind :reference reference :tags combined-tags))
      (magpi-intention-save next))))
(defun magpi-intention-set-state (intention state &optional reason)
  "Return INTENTION transitioned to STATE through one persisted update."
  (let ((current (magpi-intention-state intention)))
    (unless (memq state magpi-intention-states)
      (user-error "Unknown intention state: %S" state))
    (unless (or (eq state current)
                (memq state
                      (alist-get current magpi-intention-state-transitions)))
      (user-error "Invalid intention transition: %s -> %s" current state))
    (if (eq state current)
        intention
      (let ((next (copy-magpi-intention intention)))
        (setf (magpi-intention-state next) state)
        (setq next (magpi-intention--append next 'state-changed
                                             :from current :to state :reason reason))
        (magpi-intention-save next)))))

(defun magpi-intention-add-action (intention action-id role)
  "Record independent ACTION-ID and acquire its writer lease when needed."
  (unless (eq (magpi-intention-state intention) 'active)
    (user-error "Intention %s is %s" (magpi-intention-id intention)
                (magpi-intention-state intention)))
  (when (and (eq role 'writer) (magpi-intention-writer-lease intention))
    ;; Denial is audit, not a state transition for this intention.
    (let ((next (magpi-intention--append
                 intention 'writer-denied :action-id action-id
                 :held-by (plist-get (magpi-intention-writer-lease intention)
                                      :action-id))))
      (magpi-intention-save next))
    (user-error "Intention already has writer %s"
                (plist-get (magpi-intention-writer-lease intention) :action-id)))
  (let ((next (copy-magpi-intention intention)))
    (unless (member action-id (magpi-intention-action-ids intention))
      (setf (magpi-intention-action-ids next)
            (append (magpi-intention-action-ids intention) (list action-id))))
    (when (eq role 'writer)
      (setf (magpi-intention-writer-lease next)
            (list :action-id action-id :acquired-at (float-time))))
    (setq next (magpi-intention--append next 'action-added
                                         :action-id action-id :role role))
    (magpi-intention-save next)))

(defun magpi-intention-release-writer (intention action-id &optional reason)
  "Return INTENTION with ACTION-ID's writer lease released when it owns it."
  (if (equal action-id (plist-get (magpi-intention-writer-lease intention) :action-id))
      (let ((next (copy-magpi-intention intention)))
        (setf (magpi-intention-writer-lease next) nil)
        (setq next (magpi-intention--append next 'writer-released
                                             :action-id action-id :reason reason))
        (magpi-intention-save next))
    intention))

(defun magpi-intention-git-facts (intention)
  "Return truthful live Git facts for INTENTION; unknown facts stay nil."
  (let* ((path (magpi-intention-worktree-path intention))
         (exists (and path (file-directory-p path)))
         (status (and exists
                      (magpi-git--maybe path "status" "--porcelain")))
         (counts (and exists
                      (magpi-git--maybe
                       path "rev-list" "--left-right" "--count"
                       (format "%s...%s" (magpi-intention-base-ref intention)
                               (magpi-intention-branch intention)))))
         (parts (and counts (split-string counts "[ \t]+" t)))
         (checkout (cond ((null path) 'unstarted)
                         ((not exists) 'missing)
                         ((null status) 'unavailable)
                         ((string-empty-p status) 'clean)
                         (t 'dirty))))
    (list :checkout checkout :dirty (eq checkout 'dirty)
          :behind (and parts (string-to-number (car parts)))
          :ahead (and parts (string-to-number (cadr parts)))
          :exists exists)))

(defun magpi-intention--guard-quiescent (intention operation)
  (unless (eq (magpi-intention-state intention) 'active)
    (user-error "Cannot %s %s intention" operation (magpi-intention-state intention)))
  (when (magpi-intention-writer-lease intention)
    (user-error "Cannot %s while writer %s holds the lease"
                operation
                (plist-get (magpi-intention-writer-lease intention) :action-id))))

(defun magpi-intention-merge-record (intention)
  "Guard, merge INTENTION into its recorded base ref, and persist object ids.

On Git failure the intention stays active, a merge-failed audit is saved, and
the error is re-signalled so React can land in Magit."
  (magpi-intention--guard-quiescent intention "merge")
  (let ((source (magpi-intention-source-root intention))
        (path (magpi-intention-worktree-path intention))
        (branch (magpi-intention-branch intention))
        (base-ref (magpi-intention-base-ref intention)))
    (unless (and path (file-directory-p path) branch)
      (user-error "Start an action before merging this intention"))
    (unless (string-empty-p (magpi-git path "status" "--porcelain"))
      (user-error "Refusing to merge dirty intention worktree"))
    (unless (string-empty-p (magpi-git source "status" "--porcelain"))
      (user-error "Refusing to merge into dirty checkout: %s" source))
    (unless (equal (magpi-git--maybe
                    source "symbolic-ref" "--quiet" "--short" "HEAD")
                   base-ref)
      (user-error "Source checkout is not on intention base %s" base-ref))
    (let ((from-oid (magpi-git source "rev-parse" "HEAD")))
      (condition-case err
          (progn
            (magpi-git source "merge" "--no-ff" branch)
            (let* ((to-oid (magpi-git source "rev-parse" "HEAD"))
                   (next (magpi-intention--append
                          intention 'merged
                          :branch branch :base-ref base-ref
                          :from-oid from-oid :to-oid to-oid)))
              (magpi-intention-set-state next 'merged)))
        (error
         (magpi-intention-save
          (magpi-intention--append
           intention 'merge-failed
           :branch branch :base-ref base-ref :from-oid from-oid
           :reason (error-message-string err)))
         (signal (car err) (cdr err)))))))

(defun magpi-intention-discard-record (intention)
  "Guard, force-remove INTENTION's worktree, and retain its persisted audit."
  (magpi-intention--guard-quiescent intention "discard")
  (let ((path (magpi-intention-worktree-path intention)))
    (when (and path (file-directory-p path))
      (magpi-git (magpi-intention-source-root intention)
                 "worktree" "remove" "--force" path)))
  (let ((next (magpi-intention--append
               intention 'discarded :path (magpi-intention-worktree-path intention))))
    (magpi-intention-set-state next 'discarded)))

(defun magpi-intention-remove-worktree (intention)
  "Remove a terminal INTENTION worktree without changing its disposition."
  (unless (memq (magpi-intention-state intention) '(merged discarded))
    (user-error "Refusing to clean up nonterminal intention"))
  (if (file-directory-p (magpi-intention-worktree-path intention))
      (progn
        (magpi-git (magpi-intention-source-root intention)
                              "worktree" "remove" "--force"
                              (magpi-intention-worktree-path intention))
        (let ((next (magpi-intention--append
                     intention 'worktree-removed
                     :path (magpi-intention-worktree-path intention))))
          (magpi-intention-save next)))
    intention))

(provide 'magpi-intention)
;;; magpi-intention.el ends here
