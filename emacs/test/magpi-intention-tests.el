;;; magpi-intention-tests.el --- Intention contracts over real Git -*- lexical-binding: t; -*-

;; Git semantics stay real.  Developer config is neutralized.
;; Failed runs retain the repository path as evidence.
;; Intention updates are values: rebind the returned record.

(require 'ert)
(require 'seq)
(require 'magpi-intention)
(require 'magpi-test-repo)
(ert-deftest magpi-intention-persists-one-change-and-guards-its-writer ()
  "Promise: one intention, one change; one writer; merge is explicit."
  (magpi-test-with-repo (repository)
    (let ((intention
           (magpi-intention-create-record "Shared task set" repository "shared")))
      (should (equal (magpi-intention-id intention) "shared"))
      (should-not (magpi-intention-worktree-path intention))
      (should (eq (plist-get (magpi-intention-git-facts intention) :checkout)
                  'unstarted))

      (setq intention (magpi-intention-ensure-worktree intention))
      (should (file-directory-p (magpi-intention-worktree-path intention)))
      (should (string-match-p "magpi/shared-task-set-shared"
                              (magpi-intention-branch intention)))
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

      (setq intention (magpi-intention-add-action intention "task-1" 'writer))
      (should-error (magpi-intention-add-action intention "task-2" 'writer))
      (setq intention (magpi-intention-load repository "shared"))
      (should (eq (magpi-intention-state intention) 'active))
      (should-not (member "task-2" (magpi-intention-action-ids intention)))
      (should (eq (plist-get (car (last (magpi-intention-audit intention)))
                             :type)
                  'writer-denied))
      (should-error (magpi-intention-merge-record intention))
      (should (equal (plist-get (magpi-intention-writer-lease intention) :action-id)
                     "task-1"))

      (setq intention
            (magpi-intention-release-writer intention "task-1" "terminal proof"))
      ;; Task end only releases lease; disposition stays explicit.
      (should (eq (magpi-intention-state intention) 'active))
      (let ((facts (magpi-intention-git-facts intention)))
        (should (eq (plist-get facts :checkout) 'clean))
        (should (= (plist-get facts :ahead) 1)))

      (setq intention (magpi-intention-merge-record intention))
      (should (eq (magpi-intention-state intention) 'merged))
      (should-error (magpi-intention-set-state intention 'active))
      (should (file-exists-p (expand-file-name "result" repository)))
      (setq intention (magpi-intention-remove-worktree intention))
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
                       :worktree-path (expand-file-name "absent" repository)
                       :branch "magpi/gone" :base-ref "master"
                       :state 'discarded))
           (facts (magpi-intention-git-facts intention)))
      (should (eq (plist-get facts :checkout) 'missing))
      (should-not (plist-get facts :ahead)))))

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


(ert-deftest magpi-intention-merge-records-object-ids ()
  "Promise: a completed merge audit names Git objects, not just branch names."
  (magpi-test-with-repo (repository "magpi-intention-merge-oids-")
    (let ((intention (magpi-intention-create-record "Ship oids" repository "oids")))
      (setq intention (magpi-intention-ensure-worktree intention))
      (with-temp-file (expand-file-name "result" (magpi-intention-worktree-path intention))
        (insert "from action\n"))
      (magpi-test-repo-git (magpi-intention-worktree-path intention) "add" "result")
      (magpi-test-repo-git (magpi-intention-worktree-path intention) "commit" "-m" "action")
      (setq intention (magpi-intention-merge-record intention))
      (should (eq (magpi-intention-state intention) 'merged))
      (let ((merged (seq-find (lambda (entry) (eq (plist-get entry :type) 'merged))
                              (magpi-intention-audit intention))))
        (should (string-match-p "\\`[0-9a-f]\\{40\\}\\'" (plist-get merged :from-oid)))
        (should (string-match-p "\\`[0-9a-f]\\{40\\}\\'" (plist-get merged :to-oid)))
        (should-not (equal (plist-get merged :from-oid) (plist-get merged :to-oid)))))))

(ert-deftest magpi-intention-failed-merge-stays-active-and-audits ()
  "Promise: a conflicted merge is not success; Magpi keeps the recovery path."
  (magpi-test-with-repo (repository "magpi-intention-merge-fail-")
    (let ((intention (magpi-intention-create-record "Conflict" repository "fail")))
      (setq intention (magpi-intention-ensure-worktree intention))
      (with-temp-file (expand-file-name "README" (magpi-intention-worktree-path intention))
        (insert "worktree\n"))
      (magpi-test-repo-git (magpi-intention-worktree-path intention) "add" "README")
      (magpi-test-repo-git (magpi-intention-worktree-path intention) "commit" "-m" "worktree")
      (with-temp-file (expand-file-name "README" repository)
        (insert "source\n"))
      (magpi-test-repo-git repository "add" "README")
      (magpi-test-repo-git repository "commit" "-m" "source")
      (should-error (magpi-intention-merge-record intention))
      (setq intention (magpi-intention-load repository "fail"))
      (should (eq (magpi-intention-state intention) 'active))
      (should (eq (plist-get (car (last (magpi-intention-audit intention))) :type)
                  'merge-failed))
      (should (file-exists-p (expand-file-name ".git/MERGE_HEAD" repository))))))

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

(provide 'magpi-intention-tests)
;;; magpi-intention-tests.el ends here
