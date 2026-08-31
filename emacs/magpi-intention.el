;;; magpi-intention.el --- Intention being: why, change, lease, audit -*- lexical-binding: t; -*-

;; An intention is why: durable objective, change, lease, and audit.  Actions reference it.

(require 'cl-lib)
(require 'subr-x)
(require 'seq)
(require 'magpi-store)

(defconst magpi-intention-states '(active merged discarded)
  "Known persisted intention states.  Unknown values are preserved, never active.")

(defconst magpi-intention-state-transitions
  '((active . (merged discarded)))
  "Explicit intention transitions; merged and discarded are terminal.")

(defconst magpi-intention-keys
  '(:version :type :id :objective :bindings :attachments
    :worktree-path :branch :base-ref :base-oid :state :lifecycle
    :action-ids :task-ids :writer-lease :audit :events :receipts
    :source-root :created-at)
  "Keys the intention decoder understands.  Other keys round-trip.")

(cl-defstruct magpi-intention
  id objective bindings worktree-path branch base-ref base-oid state
  writer-lease audit source-root created-at extras)

(defalias 'magpi-git #'magpi-store-git)
(defalias 'magpi-git--maybe #'magpi-store-git-maybe)
(defalias 'magpi-git--repository-root #'magpi-store-toplevel)

(defun magpi-intention--store-directory (root)
  (magpi-store-kind-directory root 'intentions))

(defun magpi-intention--file (root id)
  (magpi-store-file root 'intentions id))

(defun magpi-intention-active-p (intention)
  (and (magpi-intention-p intention)
       (eq (magpi-intention-state intention) 'active)))

(defun magpi-intention--stamp (value)
  (cond
   ((integerp value) value)
   ((numberp value) (floor value))
   (t (magpi-store-unix-time))))

(defun magpi-intention--refs-same-p (left right)
  (or (equal left right)
      (and (stringp left) (stringp right)
           (equal (replace-regexp-in-string "\\`refs/heads/" "" left)
                  (replace-regexp-in-string "\\`refs/heads/" "" right)))))

(defun magpi-intention--plist (intention)
  "Return INTENTION as inert persisted data.  Root is the folder, not a field."
  (magpi-store-plist
   (list :version 1
         :type 'intention
         :id (magpi-intention-id intention)
         :objective (magpi-intention-objective intention)
         :bindings (magpi-intention-bindings intention)
         :worktree-path (or (magpi-store-relative
                             (magpi-intention-source-root intention)
                             (magpi-intention-worktree-path intention))
                            (magpi-intention-worktree-path intention))
         :branch (magpi-intention-branch intention)
         :base-ref (magpi-intention-base-ref intention)
         :base-oid (magpi-intention-base-oid intention)
         :state (magpi-intention-state intention)
         :writer-lease (magpi-intention-writer-lease intention)
         :audit (magpi-intention-audit intention)
         :created-at (magpi-intention-created-at intention))
   (magpi-intention-extras intention)))

(defun magpi-intention--from-plist (data root file)
  "Decode persisted intention DATA living at FILE under ROOT."
  (when (magpi-unreadable-p data)
    (error "%s" (magpi-unreadable-error data)))
  (let* ((version (plist-get data :version))
         (type (plist-get data :type))
         (id (plist-get data :id))
         (state (or (plist-get data :state) (plist-get data :lifecycle)))
         (lease (plist-get data :writer-lease))
         (lease (cond
                 ((and lease (plist-get lease :action-id)) lease)
                 ((and lease (plist-get lease :task-id))
                  (plist-put (copy-sequence lease) :action-id
                             (plist-get lease :task-id)))
                 (t lease))))
    (unless (equal version 1)
      (error "Unsupported Magpi intention version: %s" version))
    (when (and type (not (eq type 'intention)))
      (error "Not an intention record"))
    (unless (and (magpi-store-id-ok id file)
                 (stringp (plist-get data :objective)))
      (error "Invalid Magpi intention record"))
    (make-magpi-intention
     :id id
     :objective (plist-get data :objective)
     :bindings (or (plist-get data :bindings) (plist-get data :attachments))
     :worktree-path (magpi-store-absolute root (plist-get data :worktree-path))
     :branch (plist-get data :branch)
     :base-ref (plist-get data :base-ref)
     :base-oid (plist-get data :base-oid)
     :state state
     :writer-lease lease
     :audit (or (plist-get data :audit)
                (append (plist-get data :events) (plist-get data :receipts)))
     :source-root (file-name-as-directory (expand-file-name root))
     :created-at (and (plist-get data :created-at)
                      (magpi-intention--stamp (plist-get data :created-at)))
     :extras (magpi-store-extras data magpi-intention-keys))))

(defun magpi-intention-save (intention)
  "Atomically persist INTENTION in its repository metadata."
  (let ((root (magpi-intention-source-root intention)))
    (unless root
      (error "Intention %s has no repository" (magpi-intention-id intention)))
    (magpi-store-write (magpi-intention--file root (magpi-intention-id intention))
                       (magpi-intention--plist intention))
    intention))

(defun magpi-intention-load (root id)
  "Load persisted intention ID belonging to ROOT."
  (let* ((file (magpi-intention--file root id))
         (data (magpi-store-read file)))
    (cond
     ((and (magpi-unreadable-p data)
           (equal (magpi-unreadable-error data) "absent"))
      nil)
     ((magpi-unreadable-p data) data)
     (t (magpi-intention--from-plist data root file)))))

(defun magpi-intention-list (root)
  "Return persisted intentions for ROOT.  Unreadable files stay in the list."
  (let (records)
    (dolist (entry (magpi-store-list root 'intentions))
      (let ((file (car entry))
            (data (cdr entry)))
        (push (if (magpi-unreadable-p data)
                  data
                (condition-case err
                    (magpi-intention--from-plist data root file)
                  (error (make-magpi-unreadable
                          :path file :error (error-message-string err)))))
              records)))
    (sort records
          (lambda (a b)
            (> (or (and (magpi-intention-p a) (magpi-intention-created-at a)) 0)
               (or (and (magpi-intention-p b) (magpi-intention-created-at b)) 0))))))

(defun magpi-intention--append (intention type &rest properties)
  "Return a copy of INTENTION with one timestamped audit entry appended."
  (let ((next (copy-magpi-intention intention))
        (entry (append (list :type type :at (magpi-store-unix-time)) properties)))
    (setf (magpi-intention-audit next)
          (append (magpi-intention-audit intention) (list entry)))
    next))

(defun magpi-intention--slug (objective)
  (let ((slug (downcase (replace-regexp-in-string
                         "[^[:alnum:]]+" "-" (or objective "work")))))
    (setq slug (string-trim slug "-+" "-+"))
    (if (string-empty-p slug) "work" (substring slug 0 (min 32 (length slug))))))

(defun magpi-intention-create-record (objective root &optional id)
  "Create and persist a lightweight active intention for OBJECTIVE at ROOT.

Creating an intention only records the authored objective.  Its Git worktree is
created lazily by `magpi-intention-ensure-worktree' when work actually starts."
  (let ((intention (make-magpi-intention
                    :id (or id (magpi-store-new-id))
                    :objective objective :bindings nil
                    :worktree-path nil :branch nil :base-ref nil :base-oid nil
                    :state 'active
                    :writer-lease nil :audit nil
                    :source-root (magpi-git--repository-root root)
                    :created-at (magpi-store-unix-time))))
    (setq intention (magpi-intention--append intention 'created))
    (magpi-intention-save intention)))

(defun magpi-intention--worktree-for-branch (source branch)
  "Return (:path PATH :head OID) for BRANCH in SOURCE, or nil."
  (let ((text (magpi-git--maybe source "worktree" "list" "--porcelain"))
        path head found)
    (when text
      (dolist (line (split-string text "\n"))
        (cond
         ((string-prefix-p "worktree " line)
          (setq path (file-name-as-directory (substring line 9))
                head nil))
         ((string-prefix-p "HEAD " line)
          (setq head (substring line 5)))
         ((and (string-prefix-p "branch " line)
               (magpi-intention--refs-same-p (substring line 7) branch))
          (setq found (list :path path :head head))))))
    found))

(defun magpi-intention--record-worktree (intention path branch base-ref base-oid)
  (let ((next (copy-magpi-intention intention)))
    (setf (magpi-intention-worktree-path next) (file-name-as-directory path)
          (magpi-intention-branch next) branch
          (magpi-intention-base-ref next) base-ref
          (magpi-intention-base-oid next) base-oid)
    (setq next (magpi-intention--append next 'worktree-created
                                        :path path :base-ref base-ref
                                        :base-oid base-oid :branch branch))
    (magpi-intention-save next)))

(defun magpi-intention-ensure-worktree (intention)
  "Create INTENTION's managed worktree only when a task needs it.

Birth resolves a full destination ref to a base oid once and creates the
worktree from that oid.  An interrupted birth on the planned branch is adopted.
Detached HEAD cannot be a merge destination."
  (unless (eq (magpi-intention-state intention) 'active)
    (user-error "Intention %s is %s" (magpi-intention-id intention)
                (magpi-intention-state intention)))
  (let* ((source (magpi-intention-source-root intention))
         (path (magpi-intention-worktree-path intention)))
    (cond
     ((and path (file-directory-p path)) intention)
     (t
      (when (and path (not (file-directory-p path)))
        (when-let ((found (and (magpi-intention-branch intention)
                               (magpi-intention--worktree-for-branch
                                source (magpi-intention-branch intention)))))
          (setf path (plist-get found :path))
          (let ((next (copy-magpi-intention intention)))
            (setf (magpi-intention-worktree-path next) path)
            (setq intention (magpi-intention-save next)))))
      (if (and (magpi-intention-worktree-path intention)
               (file-directory-p (magpi-intention-worktree-path intention)))
          intention
        (let* ((base-ref (magpi-git--maybe source "symbolic-ref" "--quiet" "HEAD"))
               (id (magpi-intention-id intention))
               (branch (or (magpi-intention-branch intention)
                           (format "magpi/%s-%s"
                                   (magpi-intention--slug
                                    (magpi-intention-objective intention))
                                   id)))
               (path (or path
                         (expand-file-name
                          (format ".magpi-worktrees/%s-%s"
                                  (file-name-nondirectory
                                   (directory-file-name source))
                                  id)
                          (file-name-directory (directory-file-name source)))))
               (existing (magpi-intention--worktree-for-branch source branch)))
          (unless base-ref
            (user-error "Intention birth needs a branch, not detached HEAD"))
          (let ((base-oid (magpi-git source "rev-parse" base-ref)))
            (cond
             (existing
              (let ((head (plist-get existing :head)))
                (magpi-intention--record-worktree
                 intention (plist-get existing :path) branch base-ref
                 (and (equal head base-oid) base-oid))))
             (t
              (when (file-exists-p path)
                (user-error "Magpi intention path already exists: %s" path))
              (make-directory (file-name-directory path) t)
              (if (magpi-git--maybe source "show-ref" "--verify" "--quiet"
                                    (concat "refs/heads/" branch))
                  (magpi-git source "worktree" "add" path branch)
                (magpi-git source "worktree" "add" "-b" branch path base-oid))
              (let ((head (magpi-git path "rev-parse" "HEAD")))
                (unless (or (equal head base-oid)
                            (magpi-git--maybe source "show-ref" "--verify"
                                              "--quiet"
                                              (concat "refs/heads/" branch)))
                  (user-error "Worktree HEAD is not birth oid %s" base-oid))
                (magpi-intention--record-worktree
                 intention path branch base-ref
                 (and (equal head base-oid) base-oid))))))))))))

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
  "Acquire ACTION-ID's writer lease when needed.  Membership is Action.intention-id."
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
    (when (eq role 'writer)
      (setf (magpi-intention-writer-lease next)
            (list :action-id action-id :acquired-at (magpi-store-unix-time))))
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
  "Return truthful live Git facts for INTENTION; unknown facts stay nil.

Dirt is the worktree.  Drift is current base-ref versus HEAD.  Work against
base-oid is a separate question; missing origin is not filled from a moving ref."
  (let* ((path (magpi-intention-worktree-path intention))
         (exists (and path (file-directory-p path)))
         (status (and exists (magpi-git--maybe path "status" "--porcelain")))
         (head-ref (and exists (magpi-git--maybe
                                path "symbolic-ref" "--quiet" "--short" "HEAD")))
         (wrong (and exists (magpi-intention-branch intention)
                     (not (magpi-intention--refs-same-p
                           head-ref (magpi-intention-branch intention)))))
         (base-ref (magpi-intention-base-ref intention))
         (counts (and exists base-ref
                      (magpi-git--maybe
                       path "rev-list" "--left-right" "--count"
                       (format "%s...HEAD" base-ref))))
         (parts (and counts (split-string counts "[ \t]+" t)))
         (oid (magpi-intention-base-oid intention))
         (oid-ok (and exists oid (magpi-git--maybe path "cat-file" "-e" oid)))
         (checkout (cond ((null path) 'unstarted)
                         ((not exists) 'missing)
                         ((null status) 'unavailable)
                         (wrong 'wrong-branch)
                         ((string-empty-p status) 'clean)
                         (t 'dirty))))
    (list :checkout checkout :dirty (eq checkout 'dirty)
          :behind (and parts (string-to-number (car parts)))
          :ahead (and parts (string-to-number (cadr parts)))
          :exists exists
          :origin (cond ((null oid) 'unknown)
                        ((not exists) nil)
                        (oid-ok 'present)
                        (t 'missing))
          :work (and oid-ok
                     (string-to-number
                      (or (magpi-git--maybe path "rev-list" "--count"
                                            (format "%s..HEAD" oid))
                          ""))))))

(defun magpi-intention-work-range (intention)
  "Return Magit's work range for INTENTION, or signal unknown origin.

Never falls back to a moving base-ref.  That would relabel drift as work."
  (magpi-store-frozen-range
   (or (magpi-intention-worktree-path intention)
       (user-error "Unknown origin; this intention's work is unavailable"))
   (magpi-intention-base-oid intention)))

(defun magpi-intention--guard-quiescent (intention operation)
  (unless (eq (magpi-intention-state intention) 'active)
    (user-error "Cannot %s %s intention" operation (magpi-intention-state intention)))
  (when (magpi-intention-writer-lease intention)
    (user-error "Cannot %s while writer %s holds the lease"
                operation
                (plist-get (magpi-intention-writer-lease intention) :action-id))))

(defun magpi-intention-merge-record (intention)
  "Guard, merge INTENTION into its recorded base ref, and persist object ids.

A merge-started receipt is saved before Git mutates.  On Git failure the
intention stays active, a merge-failed audit is saved, and the error is
re-signalled so React can land in Magit."
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
    (unless (magpi-intention--refs-same-p
             (magpi-git--maybe source "symbolic-ref" "--quiet" "HEAD")
             base-ref)
      (user-error "Source checkout is not on intention base %s" base-ref))
    (let ((from-oid (magpi-git source "rev-parse" "HEAD")))
      (setq intention
            (magpi-intention-save
             (magpi-intention--append
              intention 'merge-started
              :branch branch :base-ref base-ref :from-oid from-oid)))
      (unless (equal (magpi-git source "rev-parse" "HEAD") from-oid)
        (user-error "Source HEAD moved before merge"))
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
  "Persist discarded, then force-remove leftover worktree if it remains."
  (magpi-intention--guard-quiescent intention "discard")
  (let* ((path (magpi-intention-worktree-path intention))
         (next (magpi-intention-set-state
                (magpi-intention--append intention 'discarded :path path)
                'discarded)))
    (when (and path (file-directory-p path))
      (condition-case err
          (progn
            (magpi-git (magpi-intention-source-root next)
                       "worktree" "remove" "--force" path)
            (setq next (magpi-intention-save
                        (magpi-intention--append next 'worktree-removed :path path))))
        (error
         (magpi-intention-save
          (magpi-intention--append
           next 'worktree-remove-failed :path path
           :reason (error-message-string err)))
         (signal (car err) (cdr err)))))
    next))

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
