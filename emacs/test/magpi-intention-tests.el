;;; magpi-intention-tests.el --- Intention contracts over real Git -*- lexical-binding: t; -*-

;; Git semantics stay real.  Developer config is neutralized.
;; Failed runs retain the repository path as evidence.
;; Intention updates are values: rebind the returned record.

(require 'ert)
(require 'seq)
(require 'magpi-intention)
(require 'magpi-test-repo)
(ert-deftest magpi-intention-persists-one-change-and-guards-its-writer ()
  "Promise: one intention, one change; one writer; disposition is explicit."
  (magpi-test-with-repo (repository)
    (let ((intention
           (magpi-intention-create-record "Shared task set" repository "shared")))
      (should (equal (magpi-intention-id intention) "shared"))
      (should-not (magpi-intention-worktree-path intention))
      (should (eq (plist-get (magpi-intention-git-facts intention) :checkout)
                  'unstarted))

      (setq intention (magpi-intention-ensure-worktree intention))
      (should (file-directory-p (magpi-intention-worktree-path intention)))
      (should (equal (magpi-intention-branch intention) "magpi/shared"))
      (should (eq (magpi-intention-state intention) 'active))
      (should (equal (magpi-intention-objective
                      (magpi-intention-load repository "shared"))
                     "Shared task set"))
      (should (magpi-intention-audit intention))

      (with-temp-file
          (expand-file-name "result" (magpi-intention-worktree-path intention))
        (insert "from related tasks\n"))
      (magpi-test-repo-git
       (magpi-intention-worktree-path intention) "add" "result")
      (magpi-test-repo-git
       (magpi-intention-worktree-path intention) "commit" "-m" "task result")

      (setq intention (magpi-intention-add-action intention "task-1" 'writer t))
      (should-error (magpi-intention-add-action intention "task-2" 'writer))
      (setq intention (magpi-intention-load repository "shared"))
      (should (eq (magpi-intention-state intention) 'active))
      (should (eq (plist-get (car (last (magpi-intention-audit intention)))
                             :type)
                  'writer-denied))
      (should-error (magpi-intention--guard-quiescent intention "open merge"))
      (should (equal (plist-get (magpi-intention-writer-lease intention) :action-id)
                     "task-1"))

      (setq intention
            (magpi-intention-release-writer intention "task-1" "terminal proof"))
      ;; Task end only releases lease; disposition stays explicit.
      (should (eq (magpi-intention-state intention) 'active))
      (let ((facts (magpi-intention-git-facts intention)))
        (should (eq (plist-get facts :checkout) 'clean))
        (should (= (plist-get facts :ahead) 1)))

      (setq intention
            (magpi-intention-set-state
             intention 'merged "operator merged after Git evidence"))
      (should (eq (magpi-intention-state intention) 'merged))
      (should-error (magpi-intention-set-state intention 'active))
      (setq intention (magpi-intention-remove-worktree intention))
      (dolist (entry (magpi-intention-audit intention))
        (should-not (plist-member entry :path)))
      (should-not (file-directory-p
                   (or (magpi-intention-worktree-path intention) "/nonexistent")))
      (should (eq (magpi-intention-state
                   (magpi-intention-load repository "shared"))
                  'merged)))))

(ert-deftest magpi-intention-missing-worktree-does-not-claim-clean-zero-facts ()
  "Promise: missing Git facts stay missing; never rendered as clean/zero."
  (magpi-test-with-repo (repository "magpi-intention-missing-")
    (let* ((intention (make-magpi-intention
                       :id "gone" :objective "Gone"
                       :source-root repository
                       :target-ref "master"
                       :genesis-oid "deadbeefdeadbeefdeadbeefdeadbeefdeadbeef"
                       :state 'discarded))
           (facts (magpi-intention-git-facts intention)))
      (should (eq (plist-get facts :checkout) 'missing))
      (should-not (plist-get facts :ahead)))))

(ert-deftest magpi-intention-writers-share-until-exclusive-lease ()
  "Promise: several writers share; W takes the lease and then refuses writers."
  (magpi-test-with-repo (repository "magpi-intention-share-")
    (let ((intention (magpi-intention-create-record "Share" repository "share")))
      (setq intention (magpi-intention-add-action intention "a1" 'writer))
      (setq intention (magpi-intention-add-action intention "a2" 'writer))
      (should-not (magpi-intention-writer-lease intention))
      (setq intention (magpi-intention-add-action intention "a3" 'writer t))
      (should (equal (plist-get (magpi-intention-writer-lease intention) :action-id)
                     "a3"))
      (should-error (magpi-intention-add-action intention "a4" 'writer))
      (should-error (magpi-intention-add-action intention "a5" 'writer t))
      (setq intention (magpi-intention-add-action intention "a6" 'reader))
      (should (equal (plist-get (magpi-intention-writer-lease intention) :action-id)
                     "a3")))))

(ert-deftest magpi-intention-is-lightweight-and-persists-file-and-chat-tags ()
  "Promise: bindings are references with tags; no worktree until work starts."
  (magpi-test-with-repo (repository "magpi-intention-tags-")
    (with-temp-file (expand-file-name "notes.org" repository)
      (insert "attachment\n"))
    (let ((intention (magpi-intention-create-record "Keep context" repository "tags")))
      (should-not (magpi-intention-worktree-path intention))
      (setq intention
            (magpi-intention-bind intention 'file "notes.org" "notes" '("design")))
      (setq intention
            (magpi-intention-bind intention 'file "notes.org" nil '("scope")))
      (setq intention
            (magpi-intention-bind intention 'chat "pimacs:session-1" "Earlier chat"
                                    '("research" "handoff")))
      (let ((bindings (magpi-intention-bindings
                          (magpi-intention-load repository "tags"))))
        (should (equal bindings
                       '((:kind file :reference "notes.org" :label "notes"
                          :tags ("design" "scope"))
                         (:kind chat :reference "pimacs:session-1"
                          :label "Earlier chat"
                          :tags ("research" "handoff")))))))))


(ert-deftest magpi-intention-discard-force-removes-dirty-worktree ()
  "Promise: discard is explicit force-remove; the record stays as discarded."
  (magpi-test-with-repo (repository "magpi-intention-discard-")
    (let ((intention (magpi-intention-create-record "Throw away" repository "drop"))
          path)
      (setq intention (magpi-intention-ensure-worktree intention))
      (setq path (magpi-intention-worktree-path intention))
      (with-temp-file (expand-file-name "dirty" path)
        (insert "uncommitted\n"))
      (setq intention (magpi-intention-discard-record intention))
      (should (eq (magpi-intention-state intention) 'discarded))
      (should-not (file-directory-p path)))))

(ert-deftest magpi-intention-round-trips-unknown-keys-and-integer-time ()
  "Promise: extra keys survive; created-at is integer unix time."
  (magpi-test-with-repo (repository "magpi-intention-schema-")
    (let* ((intention (magpi-intention-create-record "Keep extras" repository "extra"))
           (file (magpi-intention--file repository "extra"))
           data)
      (should (integerp (magpi-intention-created-at intention)))
      (should-not (plist-member (magpi-intention--plist intention) :version))
      (should-not (plist-member (magpi-intention--plist intention) :type))
      (setq data (plist-put (magpi-intention--plist intention) :future-field "keep"))
      (magpi-store-write file data)
      (setq intention (magpi-intention-load repository "extra"))
      (should (equal (plist-get (magpi-intention-extras intention) :future-field) "keep"))
      (magpi-intention-save intention)
      (should (equal (plist-get (magpi-intention-load repository "extra") :future-field)
                     nil))
      (should (equal (plist-get (magpi-intention-extras
                                 (magpi-intention-load repository "extra"))
                                :future-field)
                     "keep")))))

(ert-deftest magpi-intention-local-files-are-forgiving ()
  "Promise: :lifecycle still means state; classified leftovers are not extras."
  (magpi-test-with-repo (repository "magpi-intention-forgive-")
    (let ((file (magpi-intention--file repository "old")))
      (make-directory (file-name-directory file) t)
      (magpi-store-write
       file
       '(:id "old" :objective "Repair auth" :lifecycle discarded
         :task-ids nil :source-root "/tmp/unused" :version 1))
      (let ((intention (magpi-intention-load repository "old")))
        (should (eq (magpi-intention-state intention) 'discarded))
        (should (equal (magpi-intention-objective intention) "Repair auth"))
        (should-not (plist-get (magpi-intention-extras intention) :lifecycle))
        (magpi-intention-save intention)
        (let ((data (magpi-store-read file)))
          (should (eq (plist-get data :state) 'discarded))
          (should-not (plist-member data :version))
          (should-not (plist-member data :type))
          (should-not (plist-member data :lifecycle))
          (should-not (plist-member data :task-ids))
          (should-not (plist-member data :source-root)))))))

(ert-deftest magpi-intention-disk-omits-action-ids ()
  "Promise: membership is Action.intention-id, not an Intention cache."
  (magpi-test-with-repo (repository "magpi-intention-index-")
    (let ((intention (magpi-intention-create-record "Index" repository "idx"))
          data)
      (setq intention (magpi-intention-add-action intention "act-1" 'writer))
      (setq data (magpi-store-read (magpi-intention--file repository "idx")))
      (should-not (plist-member data :action-ids))
      (should-not (plist-member data :version))
      (should-not (plist-member data :type)))))

(ert-deftest magpi-intention-birth-freezes-genesis-oid-not-moving-ref ()
  "Promise: genesis-oid is birth; advancing the destination does not rewrite it."
  (magpi-test-with-repo (repository "magpi-intention-oid-")
    (let* ((intention (magpi-intention-create-record "Freeze genesis" repository "orig"))
           birth)
      (setq intention (magpi-intention-ensure-worktree intention))
      (setq birth (magpi-intention-genesis-oid intention))
      (should (string-match-p "\\`[0-9a-f]\\{40\\}\\'" birth))
      (should (string-prefix-p "refs/heads/" (magpi-intention-target-ref intention)))
      (with-temp-file (expand-file-name "moved" repository)
        (insert "main moved\n"))
      (magpi-test-repo-git repository "add" "moved")
      (magpi-test-repo-git repository "commit" "-m" "move target")
      (setq intention (magpi-intention-load repository "orig"))
      (should (equal (magpi-intention-genesis-oid intention) birth))
      (should (equal (plist-get (magpi-intention-git-facts intention) :genesis) 'present))
      (should (equal (magpi-intention-work-range intention)
                     (format "%s..HEAD" birth))))))

(ert-deftest magpi-intention-unknown-genesis-is-not-filled-from-target-ref ()
  "Promise: missing genesis-oid stays unknown; Magit work range refuses."
  (magpi-test-with-repo (repository "magpi-intention-unknown-")
    (let ((intention (magpi-intention-create-record "Old" repository "old")))
      (setq intention (magpi-intention-ensure-worktree intention))
      (setf (magpi-intention-genesis-oid intention) nil)
      (magpi-intention-save intention)
      (setq intention (magpi-intention-load repository "old"))
      (should (eq (plist-get (magpi-intention-git-facts intention) :genesis) 'unknown))
      (should-not (plist-get (magpi-intention-git-facts intention) :work))
      (should-error (magpi-intention-work-range intention)))))

(ert-deftest magpi-intention-legacy-origin-keys-load-as-target-and-genesis ()
  "Promise: old :base-ref / :base-oid files load and rewrite as target/genesis."
  (magpi-test-with-repo (repository "magpi-intention-legacy-")
    (let* ((intention (magpi-intention-create-record "Legacy" repository "leg"))
           (file (magpi-intention--file repository "leg"))
           data)
      (setq intention (magpi-intention-ensure-worktree intention))
      (setq data (magpi-store-read file))
      (magpi-store-write file
                         (list :id "leg"
                               :objective "Legacy"
                               :state 'active
                               :base-ref (plist-get data :target-ref)
                               :base-oid (plist-get data :genesis-oid)
                               :created-at (plist-get data :created-at)))
      (setq intention (magpi-intention-load repository "leg"))
      (should (equal (magpi-intention-target-ref intention)
                     (plist-get data :target-ref)))
      (should (equal (magpi-intention-genesis-oid intention)
                     (plist-get data :genesis-oid)))
      (magpi-intention-save intention)
      (setq data (magpi-store-read file))
      (should (plist-member data :target-ref))
      (should (plist-member data :genesis-oid))
      (should-not (plist-member data :base-ref))
      (should-not (plist-member data :base-oid)))))

(ert-deftest magpi-store-frozen-range-is-locator-not-keep-alive ()
  "Promise: frozen oid..HEAD; ancestor check is standalone-only."
  (magpi-test-with-repo (repository "magpi-frozen-range-")
    (let* ((parent (string-trim (magpi-test-repo-git repository "rev-parse" "HEAD")))
           child)
      (with-temp-file (expand-file-name "next" repository)
        (insert "child\n"))
      (magpi-test-repo-git repository "add" "next")
      (magpi-test-repo-git repository "commit" "-m" "child")
      (setq child (string-trim (magpi-test-repo-git repository "rev-parse" "HEAD")))
      (should (equal (magpi-store-frozen-range repository child)
                     (format "%s..HEAD" child)))
      (should (equal (magpi-store-frozen-range repository child t)
                     (format "%s..HEAD" child)))
      (should (magpi-store-oid-ancestor-p repository parent "HEAD"))
      (should (magpi-store-oid-ancestor-p repository child "HEAD"))
      (magpi-test-repo-git repository "reset" "--hard" parent)
      (should (magpi-store-oid-ancestor-p repository parent "HEAD"))
      (should-not (magpi-store-oid-ancestor-p repository child "HEAD"))
      (should-error (magpi-store-frozen-range repository child t))
      (should (equal (magpi-store-frozen-range repository child)
                     (format "%s..HEAD" child)))
      (should-error (magpi-store-frozen-range repository nil))
      (should-error (magpi-store-frozen-range repository
                                             "deadbeefdeadbeefdeadbeefdeadbeefdeadbeef"))
      (should-not (magpi-store-oid-ancestor-p repository child "HEAD"))))

(ert-deftest magpi-intention-refuses-detached-birth ()
  "Promise: detached HEAD is not a merge destination."
  (magpi-test-with-repo (repository "magpi-intention-detach-")
    (let ((intention (magpi-intention-create-record "Detach" repository "det")))
      (magpi-test-repo-git repository "checkout" "--detach" "HEAD")
      (should-error (magpi-intention-ensure-worktree intention)))))

(ert-deftest magpi-intention-adopts-interrupted-worktree-birth ()
  "Promise: a real worktree left by a failed save is adopted on retry."
  (magpi-test-with-repo (repository "magpi-intention-adopt-")
    (let* ((intention (magpi-intention-create-record "Adopt" repository "adp"))
           (first (magpi-intention-ensure-worktree intention))
           (path (magpi-intention-worktree-path first))
           (branch (magpi-intention-branch first))
           (oid (magpi-intention-genesis-oid first))
           (retry (copy-magpi-intention intention)))
      (setf (magpi-intention-target-ref retry) nil
            (magpi-intention-genesis-oid retry) nil)
      (setq retry (magpi-intention-ensure-worktree retry))
      (should (file-equal-p (magpi-intention-worktree-path retry) path))
      (should (equal (magpi-intention-branch retry) branch))
      (should (equal (magpi-intention-genesis-oid retry) oid)))))

(ert-deftest magpi-intention-unreadable-stays-visible ()
  "Promise: corrupt records are not silently dropped."
  (magpi-test-with-repo (repository "magpi-intention-corrupt-")
    (magpi-intention-create-record "Good" repository "good")
    (let ((file (magpi-intention--file repository "bad")))
      (make-directory (file-name-directory file) t)
      (with-temp-file file (insert "(not a plist\n"))
      (let ((records (magpi-intention-list repository)))
        (should (seq-find #'magpi-intention-p records))
        (should (seq-find #'magpi-unreadable-p records))))))

(ert-deftest magpi-intention-birth-uses-the-garden-and-keeps-invoking-head ()
  "Promise: birth adds magpi/<id> under the home; no checkout is switched."
  (magpi-test-with-repo (repository "magpi-intention-wt-garden-")
    (let* ((intention (magpi-intention-create-record "Stay outside" repository "inside"))
           (before (magpi-test-repo-git repository "symbolic-ref" "--quiet" "HEAD"))
           data)
      (setq intention (magpi-intention-ensure-worktree intention))
      (should (file-equal-p (magpi-intention-worktree-path intention)
                            (magpi-intention--preferred-path repository "inside")))
      (should (equal (magpi-intention-branch intention) "magpi/inside"))
      (should (equal (string-trim before)
                     (string-trim (magpi-test-repo-git repository
                                                      "symbolic-ref" "--quiet" "HEAD"))))
      (should-not (file-directory-p (expand-file-name ".magpi-worktrees" repository)))
      (setq data (magpi-store-read (magpi-intention--file repository "inside")))
      (should-not (plist-member data :worktree-path))
      (should-not (plist-member data :branch))
      (dolist (entry (plist-get data :audit))
        (should-not (plist-member entry :path)))
      (should (magpi-intention-target-ref
               (magpi-intention-load repository "inside"))))))
(ert-deftest magpi-intention-garden-is-global-and-stable ()
  "Promise: preferred path follows git-common-dir, not the invoking checkout."
  (magpi-test-with-repo (repository "magpi-intention-garden-")
    (let* ((home (make-temp-file "magpi-test-home-" t))
           (link-parent (make-temp-file "magpi-test-link-" t))
           (linked (expand-file-name "linked" link-parent))
           (magpi-home-directory home))
      (unwind-protect
          (progn
            (magpi-test-repo-git repository "worktree" "add" linked)
            (let ((from-main (magpi-intention--preferred-path repository "why"))
                  (from-link (magpi-intention--preferred-path linked "why"))
                  (key (magpi-intention--repository-key repository)))
              (should (equal from-main from-link))
              (should (equal key (magpi-intention--repository-key linked)))
              (should (equal from-main
                             (file-name-as-directory
                              (expand-file-name
                               "why"
                               (expand-file-name
                                key
                                (expand-file-name "worktrees" home))))))
              (should-not (string-prefix-p
                           (file-truename repository)
                           (file-truename from-main)))))
        (ignore-errors (magpi-test-repo-git repository "worktree" "remove" linked))
        (ignore-errors (delete-directory home t))
        (ignore-errors (delete-directory link-parent t))))))

(ert-deftest magpi-intention-home-refuses-a-git-checkout ()
  "Promise: a Magpi home inside any Git checkout is refused."
  (magpi-test-with-repo (repository "magpi-intention-home-git-")
    (let ((magpi-home-directory (expand-file-name "garden" repository)))
      (should-error (magpi-intention--home) :type 'user-error)
      (should-error (magpi-intention--preferred-path repository "why")
                    :type 'user-error))))

(ert-deftest magpi-intention-refuses-unsafe-ids-before-derived-paths ()
  "Promise: branch and garden paths require a path-safe id."
  (magpi-test-with-repo (repository "magpi-intention-unsafe-id-")
    (should-not (magpi-intention-branch (make-magpi-intention :id nil)))
    (should (equal (magpi-intention-branch (make-magpi-intention :id "why"))
                   "magpi/why"))
    (should (magpi-store-id-p "why"))
    (dolist (id '("" "." ".." "../escape" "foo/bar" "foo\\bar" "~" "~root"))
      (should-not (magpi-store-id-p id))
      (should-error (magpi-intention--preferred-path repository id))
      (should-error (magpi-intention-branch (make-magpi-intention :id id)))
      (should-error (magpi-intention-create-record "Why" repository id)))))

(ert-deftest magpi-intention-birth-from-any-checkout-uses-the-same-garden ()
  "Promise: invoking checkout only supplies target-ref; garden follows the repository."
  (magpi-test-with-repo (repository "magpi-intention-birth-link-")
    (let* ((link-parent (make-temp-file "magpi-test-link-" t))
           (linked (expand-file-name "linked" link-parent))
           intention)
      (unwind-protect
          (progn
            (magpi-test-repo-git repository "worktree" "add" "-b" "other" linked)
            (setq intention (magpi-intention-create-record "Peer" linked "peer"))
            (setq intention (magpi-intention-ensure-worktree intention linked))
            (should (file-equal-p (magpi-intention-worktree-path intention)
                                  (magpi-intention--preferred-path repository "peer")))
            (should (string-match-p "refs/heads/other\\'"
                                    (magpi-intention-target-ref intention)))
            (should (equal "refs/heads/master"
                           (string-trim (magpi-test-repo-git repository
                                                            "symbolic-ref" "--quiet" "HEAD"))))
            (should (equal "refs/heads/other"
                           (string-trim (magpi-test-repo-git linked
                                                            "symbolic-ref" "--quiet" "HEAD")))))
        (ignore-errors (magpi-test-repo-git repository "worktree" "remove" linked))
        (ignore-errors (delete-directory link-parent t))))))
(ert-deftest magpi-intention-glance-root-routes-change-worktree-to-primary ()
  "Promise: glance from a Magpi change checkout is the repository Magpi."
  (magpi-test-with-repo (repository "magpi-intention-glance-root-")
    (let* ((intention (magpi-intention-create-record "Stay outside" repository "inside"))
           work)
      (setq intention (magpi-intention-ensure-worktree intention)
            work (magpi-intention-worktree-path intention))
      (should (file-equal-p (magpi-intention-glance-root repository) repository))
      (should (file-equal-p (magpi-intention-glance-root work) repository)))))

(ert-deftest magpi-intention-glance-root-keeps-unrelated-linked-checkout ()
  "Promise: a non-Magpi linked worktree is not a nested Magpi."
  (magpi-test-with-repo (repository "magpi-intention-glance-peer-")
    (let* ((link-parent (make-temp-file "magpi-test-link-" t))
           (linked (expand-file-name "linked" link-parent)))
      (unwind-protect
          (progn
            (magpi-test-repo-git repository "worktree" "add" "-b" "other" linked)
            (should (file-equal-p (magpi-intention-glance-root linked) linked)))
        (ignore-errors (magpi-test-repo-git repository "worktree" "remove" linked))
        (ignore-errors (delete-directory link-parent t))))))

(provide 'magpi-intention-tests)
;;; magpi-intention-tests.el ends here
