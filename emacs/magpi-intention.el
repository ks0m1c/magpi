;;; magpi-intention.el --- Intention being: why, change, lease, audit -*- lexical-binding: t; -*-

;; Copyright (C) 2026 ks0m1c_dharma
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is part of Magpi.

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

(defconst magpi-intention-fields
  '(:id :objective :bindings :target-ref :genesis-oid
    :state :writer-lease :audit :created-at)
  "Coordinates Magpi persists.  Names keep their meaning.")

(defconst magpi-intention-absorbed
  '(:lifecycle :version :type :task-ids :action-ids :source-root
    :worktree-path :branch :base-ref :base-oid)
  "Classified names.  :lifecycle still reads as :state; the rest are not extras.
Old :base-ref / :base-oid load as :target-ref / :genesis-oid.")

(defconst magpi-intention-keys
  (append magpi-intention-fields magpi-intention-absorbed))

(cl-defstruct magpi-intention
  id objective bindings target-ref genesis-oid state
  writer-lease audit source-root created-at extras)

(defalias 'magpi-git #'magpi-store-git)
(defalias 'magpi-git--maybe #'magpi-store-git-maybe)
(defalias 'magpi-git--repository-root #'magpi-store-toplevel)

(defcustom magpi-home-directory (expand-file-name "~/.magpi/")
  "Global Magpi home for change checkouts.

Change trees live at worktrees/<repository-key>/<id>/.
One user option, not a per-repository override.  Must not fall
inside a Git checkout.  Location is a birth hint; live Git wins."
  :type 'directory
  :group 'magpi)

(defun magpi-intention--inside-git-p (directory)
  "Return non-nil when DIRECTORY is inside a Git work tree.

Uncreated DIRECTORY is judged by the nearest existing ancestor."
  (let ((dir (and directory (expand-file-name directory))))
    (while (and dir (not (file-exists-p dir)))
      (let ((parent (file-name-directory (directory-file-name dir))))
        (setq dir (unless (or (null parent) (equal parent dir)) parent))))
    (and dir
         (equal "true"
                (magpi-git--maybe dir "rev-parse" "--is-inside-work-tree")))))

(defun magpi-intention--home ()
  "Return Magpi's global home, or refuse a home inside a Git checkout."
  (let ((home (file-name-as-directory (expand-file-name magpi-home-directory))))
    (when (magpi-intention--inside-git-p home)
      (user-error "Magpi home %s falls inside a Git checkout" home))
    home))

(defun magpi-intention--repository-key (root)
  "Return a collision-safe garden key for ROOT's canonical Git directory.

The digest is of git-common-dir.  The prefix is the directory that holds
.git, not whichever linked checkout invoked Magpi."
  (let* ((common (or (magpi-store-common-dir root)
                     (user-error "No Git directory for %s" root)))
         (digest (substring (sha1 (directory-file-name
                                   (expand-file-name common)))
                            0 12))
         (repo (file-name-directory (directory-file-name common)))
         (name (file-name-nondirectory (directory-file-name repo))))
    (format "%s-%s"
            (if (string-empty-p name) "repo" name)
            digest)))

(defun magpi-intention--garden-parent (root)
  "Return the garden directory that holds ROOT's change checkouts."
  (expand-file-name (magpi-intention--repository-key root)
                    (expand-file-name "worktrees" (magpi-intention--home))))

(defun magpi-intention--preferred-path (root id)
  "Return the garden path Magpi would create for ID in ROOT's repository."
  (unless (magpi-store-id-p id)
    (error "Invalid Magpi id"))
  (file-name-as-directory
   (expand-file-name id (magpi-intention--garden-parent root))))

(define-error 'magpi-blocked "Magpi blocked" 'user-error)

(defun magpi-intention-blocked (surface directory message &optional subject)
  "Signal `magpi-blocked' so porcelain can land Magit on DIRECTORY.

SUBJECT is the Magit log range when SURFACE is `log'."
  (signal 'magpi-blocked
          (append (list message :surface surface :directory directory)
                  (and subject (list :subject subject)))))

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
   (list :id (magpi-intention-id intention)
         :objective (magpi-intention-objective intention)
         :bindings (magpi-intention-bindings intention)
         :target-ref (magpi-intention-target-ref intention)
         :genesis-oid (magpi-intention-genesis-oid intention)
         :state (magpi-intention-state intention)
         :writer-lease (magpi-intention-writer-lease intention)
         :audit (magpi-intention-audit intention)
         :created-at (magpi-intention-created-at intention))
   (magpi-intention-extras intention)))

(defun magpi-intention--from-plist (data root file)
  "Decode persisted intention DATA living at FILE under ROOT.
Folder and id are enough.  The only live alias is :lifecycle → :state."
  (when (magpi-unreadable-p data)
    (error "%s" (magpi-unreadable-error data)))
  (let ((id (plist-get data :id)))
    (unless (magpi-store-id-ok id file)
      (error "Invalid Magpi intention record"))
    (make-magpi-intention
     :id id
     :objective (plist-get data :objective)
     :bindings (plist-get data :bindings)
     :target-ref (or (plist-get data :target-ref) (plist-get data :base-ref))
     :genesis-oid (or (plist-get data :genesis-oid) (plist-get data :base-oid))
     :state (or (plist-get data :state) (plist-get data :lifecycle))
     :writer-lease (plist-get data :writer-lease)
     :audit (plist-get data :audit)
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

(defun magpi-intention-create-record (objective root &optional id)
  "Create and persist a lightweight active intention for OBJECTIVE at ROOT.

Creating an intention only records the authored objective.  Its Git worktree is
created lazily by `magpi-intention-ensure-worktree' when work actually starts."
  (let ((id (or id (magpi-store-new-id))))
    (unless (magpi-store-id-p id)
      (error "Invalid Magpi id"))
    (let ((intention (make-magpi-intention
                      :id id
                      :objective objective :bindings nil
                      :target-ref nil :genesis-oid nil
                      :state 'active
                      :writer-lease nil :audit nil
                      :source-root (magpi-git--repository-root root)
                      :created-at (magpi-store-unix-time))))
      (setq intention (magpi-intention--append intention 'created))
      (magpi-intention-save intention))))

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

(defun magpi-intention-branch (intention)
  "Return INTENTION's derived change branch.  Id determines the name."
  (let ((id (magpi-intention-id intention)))
    (when id
      (unless (magpi-store-id-p id)
        (error "Invalid Magpi id"))
      (format "magpi/%s" id))))

(defun magpi-intention-worktree-path (intention)
  "Return the live checkout of INTENTION's change branch, or nil."
  (let ((root (magpi-intention-source-root intention)))
    (and root
         (plist-get (magpi-intention--worktree-for-branch
                     root (magpi-intention-branch intention))
                    :path))))

(defun magpi-intention--under-garden-p (directory)
  "Return non-nil when DIRECTORY is under this repository's garden."
  (let ((here (and directory
                   (file-directory-p directory)
                   (file-name-as-directory (file-truename directory))))
        garden)
    (when here
      (setq garden (ignore-errors
                     (file-name-as-directory
                      (file-truename (magpi-intention--garden-parent here)))))
      (and garden (string-prefix-p garden here)))))

(defun magpi-intention--change-checkout-p (here primary)
  "Return non-nil when HERE is a Magpi change checkout, not PRIMARY."
  (and here primary
       (not (file-equal-p here primary))
       (or (magpi-intention--under-garden-p here)
           (let ((ref (magpi-git--maybe
                       here "symbolic-ref" "--quiet" "--short" "HEAD")))
             (and ref (string-prefix-p "magpi/" ref))))))

(defun magpi-intention-glance-root (directory)
  "Return the Magpi glance checkout for DIRECTORY.

A Magpi change worktree is not a second Magpi.  Glance opens on the
repository's primary worktree."
  (let* ((directory (and directory
                         (file-name-as-directory (expand-file-name directory))))
         (here (and directory (ignore-errors (magpi-store-toplevel directory))))
         (primary (and here (magpi-store-primary-worktree here))))
    (cond
     ((and here primary (magpi-intention--change-checkout-p here primary))
      primary)
     (here here)
     (t directory))))
(defun magpi-intention--git-path-exists-p (directory name)
  "Return non-nil when Git path NAME exists in DIRECTORY's git-dir."
  (let ((path (and directory
                   (magpi-git--maybe directory "rev-parse" "--git-path" name))))
    (and path
         (file-exists-p (if (file-name-absolute-p path)
                            path
                          (expand-file-name path directory))))))

(defun magpi-intention--merge-in-progress-p (directory)
  (magpi-intention--git-path-exists-p directory "MERGE_HEAD"))

(defun magpi-intention--short-ref (ref)
  (and ref (replace-regexp-in-string "\\`refs/heads/" "" ref)))

(defun magpi-intention--heads-ref (ref)
  "Return REF as refs/heads/… when it is a short branch name."
  (cond
   ((null ref) nil)
   ((string-prefix-p "refs/" ref) ref)
   (t (concat "refs/heads/" ref))))

(defun magpi-intention-destination (intention)
  "Query Git for INTENTION's merge destination.

Return (:ref REF :oid OID :path CHECKOUT-OR-NIL) when the ref exists.
Missing is nil.  A vacant checkout is not a missing branch."
  (let* ((source (magpi-intention-source-root intention))
         (ref (magpi-intention-target-ref intention))
         (root (or (magpi-intention-worktree-path intention) source))
         (oid (and root ref
                   (condition-case nil
                       (magpi-git root "rev-parse" "--verify" ref)
                     (error nil))))
         (held (and oid source
                    (magpi-intention--worktree-for-branch source ref))))
    (and oid (list :ref ref :oid oid :path (plist-get held :path)))))

(defun magpi-intention--integrated-p (directory branch target-ref)
  "Return non-nil when BRANCH is already an ancestor of TARGET-REF in DIRECTORY."
  (and directory branch target-ref
       (magpi-git--maybe directory "merge-base" "--is-ancestor"
                         branch target-ref)))

(defun magpi-intention--fast-forward-p (directory branch target-ref)
  "Return non-nil when TARGET-REF is an ancestor of BRANCH in DIRECTORY."
  (and directory branch target-ref
       (magpi-git--maybe directory "merge-base" "--is-ancestor"
                         target-ref branch)))

(defun magpi-intention--invoking (source invoking)
  "Return INVOKING when it is a checkout of SOURCE's repository."
  (let* ((invoking (file-name-as-directory
                    (expand-file-name (or invoking source))))
         (source-common (magpi-store-common-dir source))
         (invoking-common (magpi-store-common-dir invoking)))
    (unless (and source-common invoking-common
                 (file-equal-p source-common invoking-common))
      (user-error "Invoking checkout is not this intention's repository"))
    invoking))

(defun magpi-intention--attached-head (directory)
  "Return (:ref REF :oid OID) for DIRECTORY's attached HEAD.

Detached HEAD is not an integration destination."
  (let ((ref (magpi-git--maybe directory "symbolic-ref" "--quiet" "HEAD")))
    (unless ref
      (magpi-intention-blocked
       'status directory
       "Intention birth needs a branch, not detached HEAD"))
    (list :ref ref :oid (magpi-git directory "rev-parse" ref))))

(defun magpi-intention--target-for-birth (intention invoking)
  "Return (:ref REF :oid OID) already on INTENTION, or read INVOKING now.

A stored target-ref without genesis-oid stays unknown.
Do not fill genesis from a moving tip."
  (let ((ref (magpi-intention-target-ref intention))
        (oid (magpi-intention-genesis-oid intention)))
    (cond
     ((and ref oid) (list :ref ref :oid oid))
     (ref (list :ref ref :oid nil))
     (t (magpi-intention--attached-head invoking)))))

(defun magpi-intention--record-change (intention target-ref genesis-oid)
  (let ((next (copy-magpi-intention intention)))
    (setf (magpi-intention-target-ref next) target-ref
          (magpi-intention-genesis-oid next) genesis-oid)
    (setq next (magpi-intention--append next 'worktree-created
                                        :target-ref target-ref
                                        :genesis-oid genesis-oid
                                        :branch (magpi-intention-branch intention)))
    (magpi-intention-save next)))

(defun magpi-intention--add-change (source branch path genesis-oid)
  "Add PATH as BRANCH at GENESIS-OID.  An existing branch is checked out, not recreated."
  (when (file-exists-p path)
    (user-error "A checkout already occupies the planned path for %s" branch))
  (make-directory (magpi-intention--garden-parent source) t)
  (if (magpi-git--maybe source "show-ref" "--verify" "--quiet"
                        (concat "refs/heads/" branch))
      (magpi-git source "worktree" "add" path branch)
    (magpi-git source "worktree" "add" "-b" branch path genesis-oid))
  (let ((head (magpi-git path "rev-parse" "HEAD")))
    (unless (or (equal head genesis-oid)
                (magpi-git--maybe source "show-ref" "--verify" "--quiet"
                                  (concat "refs/heads/" branch)))
      (user-error "Worktree HEAD is not genesis oid %s" genesis-oid))
    head))

(defun magpi-intention-ensure-worktree (intention &optional invoking)
  "Create INTENTION's change checkout only when a task needs it.

Birth records the invoking checkout's attached HEAD as target-ref and
freezes genesis-oid, then adds magpi/<id> at the garden path.  An existing
checkout of that branch is adopted; no checkout is switched.  Detached
HEAD cannot be a target."
  (unless (eq (magpi-intention-state intention) 'active)
    (user-error "Intention %s is %s" (magpi-intention-id intention)
                (magpi-intention-state intention)))
  (let* ((source (magpi-intention-source-root intention))
         (invoking (magpi-intention--invoking source invoking))
         (branch (magpi-intention-branch intention))
         (existing (magpi-intention--worktree-for-branch source branch)))
    (cond
     ((and existing (magpi-intention-genesis-oid intention)) intention)
     (t
      (let* ((target (if existing
                         (magpi-intention--attached-head invoking)
                       (magpi-intention--target-for-birth intention invoking)))
             (target-ref (plist-get target :ref))
             (genesis-oid (plist-get target :oid))
             (path (or (plist-get existing :path)
                       (magpi-intention--preferred-path
                        source (magpi-intention-id intention))))
             (head (or (plist-get existing :head)
                       (and genesis-oid
                            (magpi-intention--add-change
                             source branch path genesis-oid)))))
        (unless (or existing genesis-oid)
          (user-error "Unknown genesis; this intention's work is unavailable"))
        (magpi-intention--record-change
         intention target-ref
         (and (equal head genesis-oid) genesis-oid)))))))
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

(defun magpi-intention-add-action (intention action-id role &optional exclusive)
  "Record ACTION-ID on INTENTION.  EXCLUSIVE takes the writer lease.

Writer without EXCLUSIVE may share the intention.  A held lease refuses
every further writer.  Readers never take the lease."
  (unless (eq (magpi-intention-state intention) 'active)
    (user-error "Intention %s is %s" (magpi-intention-id intention)
                (magpi-intention-state intention)))
  (when (and exclusive (not (eq role 'writer)))
    (user-error "Reader cannot take the writer lease"))
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
    (when exclusive
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

Dirt is the worktree.  Drift is current target-ref versus HEAD.  Work against
genesis-oid is a separate question; missing genesis is not filled from a moving ref."
  (let* ((path (magpi-intention-worktree-path intention))
         (exists (and path (file-directory-p path)))
         (status (and exists (magpi-git--maybe path "status" "--porcelain")))
         (head-ref (and exists (magpi-git--maybe
                                path "symbolic-ref" "--quiet" "--short" "HEAD")))
         (wrong (and exists (magpi-intention-branch intention)
                     (not (magpi-intention--refs-same-p
                           head-ref (magpi-intention-branch intention)))))
         (target-ref (magpi-intention-target-ref intention))
         (counts (and exists target-ref
                      (magpi-git--maybe
                       path "rev-list" "--left-right" "--count"
                       (format "%s...HEAD" target-ref))))
         (parts (and counts (split-string counts "[ \t]+" t)))
         (oid (magpi-intention-genesis-oid intention))
         (oid-ok (and exists oid (magpi-git--maybe path "cat-file" "-e" oid)))
         (checkout (cond ((null oid) 'unstarted)
                         ((not exists) 'missing)
                         ((null status) 'unavailable)
                         (wrong 'wrong-branch)
                         ((string-empty-p status) 'clean)
                         (t 'dirty))))
    (list :checkout checkout :dirty (eq checkout 'dirty)
          :behind (and parts (string-to-number (car parts)))
          :ahead (and parts (string-to-number (cadr parts)))
          :exists exists
          :genesis (cond ((null oid) 'unknown)
                         ((not exists) nil)
                         (oid-ok 'present)
                         (t 'missing))
          :work (and oid-ok
                     (string-to-number
                      (or (magpi-git--maybe path "rev-list" "--count"
                                            (format "%s..HEAD" oid))
                          ""))))))

(defun magpi-intention-work-range (intention)
  "Return Magit's work range for INTENTION, or signal unknown genesis.

Never falls back to a moving target-ref.  That would relabel drift as work."
  (magpi-store-frozen-range
   (or (magpi-intention-worktree-path intention)
       (user-error "Unknown genesis; this intention's work is unavailable"))
   (magpi-intention-genesis-oid intention)))

(defun magpi-intention--guard-quiescent (intention operation)
  (unless (eq (magpi-intention-state intention) 'active)
    (user-error "Cannot %s %s intention" operation (magpi-intention-state intention)))
  (when (magpi-intention-writer-lease intention)
    (user-error "Cannot %s while writer %s holds the lease"
                operation
                (plist-get (magpi-intention-writer-lease intention) :action-id))))

(defun magpi-intention-discard-record (intention)
  "Persist discarded, then force-remove leftover worktree if it remains."
  (magpi-intention--guard-quiescent intention "discard")
  (let* ((path (magpi-intention-worktree-path intention))
         (branch (magpi-intention-branch intention))
         (next (magpi-intention-set-state
                (magpi-intention--append intention 'discarded :branch branch)
                'discarded)))
    (when (and path (file-directory-p path))
      (condition-case err
          (progn
            (magpi-git (magpi-intention-source-root next)
                       "worktree" "remove" "--force" path)
            (setq next (magpi-intention-save
                        (magpi-intention--append next 'worktree-removed
                                                 :branch branch))))
        (error
         (magpi-intention-save
          (magpi-intention--append
           next 'worktree-remove-failed :branch branch
           :reason (error-message-string err)))
         (signal (car err) (cdr err)))))
    next))

(defun magpi-intention-remove-worktree (intention)
  "Remove a terminal INTENTION worktree without changing its disposition."
  (unless (memq (magpi-intention-state intention) '(merged discarded))
    (user-error "Refusing to clean up nonterminal intention"))
  (let ((path (magpi-intention-worktree-path intention))
        (branch (magpi-intention-branch intention)))
    (if (and path (file-directory-p path))
        (progn
          (magpi-git (magpi-intention-source-root intention)
                     "worktree" "remove" "--force" path)
          (magpi-intention-save
           (magpi-intention--append intention 'worktree-removed
                                    :branch branch)))
      intention)))

(provide 'magpi-intention)
;;; magpi-intention.el ends here
