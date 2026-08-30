;;; magpi-action-tests.el --- Tests for functional Magpi action domain -*- lexical-binding: t; -*-

(require 'ert)
(require 'magpi-action)
(require 'magpi-launch)

(defun magpi-test-action (&optional prompt)
  (make-magpi-action
   :id "a1" :prompt (or prompt "Make intent visible")
   :launch (magpi-launch-build default-directory nil 'writer '(:kind none))
   :observation (magpi-observation-initial)))

(ert-deftest magpi-action-reduce-is-functional ()
  (let* ((action (magpi-test-action))
         (next (magpi-action-reduce
                action '(:type activity-started :activity "edit"))))
    (should-not (eq action next))
    (should-not (eq (magpi-action-observation action)
                    (magpi-action-observation next)))
    (should (eq (magpi-observation-activity-state
                 (magpi-action-observation action))
                'starting))
    (should (eq (magpi-observation-activity-state
                 (magpi-action-observation next)) 'running))
    (should (equal (magpi-observation-activity
                    (magpi-action-observation next)) "edit"))))

(ert-deftest magpi-action-reduce-records-semantic-observations ()
  (let* ((action (magpi-test-action))
         (title (magpi-action-reduce
                 action '(:type title-observed :title "Derived task")))
         (response (magpi-action-reduce
                    title '(:type response-observed
                             :text "Changed implementation.\nTesting now."))))
    (should (equal (magpi-action-prompt action) "Make intent visible"))
    (should (equal (magpi-action-prompt response) "Make intent visible"))
    (should (equal (magpi-observation-display-title
                    (magpi-action-observation response))
                   "Derived task"))
    (should (equal (magpi-observation-last-response
                    (magpi-action-observation response))
                   "Changed implementation. Testing now."))))

(ert-deftest magpi-action-reduce-deduplicates-normalized-file-evidence ()
  (cl-letf (((symbol-function 'file-truename)
             (lambda (&rest _) (error "Reducer must not access the filesystem")))
            ((symbol-function 'file-in-directory-p)
             (lambda (&rest _) (error "Reducer must not access the filesystem"))))
    (let* ((action (magpi-test-action))
           (next (seq-reduce
                  (lambda (value path)
                    (magpi-action-reduce value
                                          (list :type 'file-observed :path path)))
                  '("lib/auth.ex" "lib/auth.ex")
                  action)))
      (should-not (magpi-observation-observed-files
                   (magpi-action-observation action)))
      (should (equal (magpi-observation-observed-files
                      (magpi-action-observation next))
                     '("lib/auth.ex"))))))

(ert-deftest magpi-action-reduce-normalizes-empty-observations ()
  (let* ((action (magpi-test-action))
         (title (magpi-action-reduce
                 action '(:type title-observed :title "Derived task")))
         (blank-title (magpi-action-reduce
                       title '(:type title-observed :title "   ")))
         (blank-response (magpi-action-reduce
                          blank-title '(:type response-observed :text " \n\t "))))
    (should (equal (magpi-observation-display-title
                    (magpi-action-observation blank-title))
                   "Derived task"))
    (should-not (magpi-observation-last-response
                 (magpi-action-observation blank-response)))))

(ert-deftest magpi-action-reduce-keeps-problem-and-connection-observations ()
  (let* ((action (magpi-test-action))
         (problem (magpi-action-reduce
                   action '(:type problem-observed :problem "extension error")))
         (disconnected (magpi-action-reduce problem '(:type disconnected))))
    (should (equal (magpi-observation-problem
                    (magpi-action-observation problem))
                   "extension error"))
    (should (eq (magpi-observation-connection-state
                 (magpi-action-observation disconnected))
                'disconnected))
    (should (eq (magpi-observation-activity-state
                 (magpi-action-observation disconnected))
                'unknown))))

(ert-deftest magpi-adapter-observations-never-change-authored-data ()
  (let* ((launch (magpi-launch-build "/tmp/" 'low 'writer '(:kind none)))
         (action (make-magpi-action
                   :id "a1" :prompt "Keep this prompt" :launch launch
                   :observation (magpi-observation-initial))))
    (dolist (event '((:type activity-started :activity "edit")
                     (:type activity-ended :idle t)
                     (:type file-observed :path "lib/auth.ex")
                     (:type response-observed :text "Done")
                     (:type title-observed :title "Derived")
                     (:type model-observed :model "anthropic/claude-sonnet")
                     (:type problem-observed :problem "extension error")
                     (:type disconnected)))
      (setq action (magpi-action-reduce action event))
      (should (equal (magpi-action-prompt action) "Keep this prompt"))
      (should (eq (magpi-action-launch action) launch)))))

(ert-deftest magpi-ask-events-preserve-structured-nested-state ()
  (let* ((action (magpi-test-action))
         (parent (magpi-action-reduce
                  action
                  '(:type ask-requested
                    :ask (:id "approval-1" :requester "planner" :question "Apply the migration?"
                               :affected-paths ("lib/schema.ex" "priv/migrate.ex")))))
         (nested (magpi-action-reduce
                  parent
                  '(:type ask-requested
                    :ask (:id "approval-2" :parent-id "approval-1"
                               :requester "reviewer" :question "Allow child validation?"
                               :state pending :affected-paths ("test/schema_test.ex")))))
         (resolved (magpi-action-reduce
                    nested
                    '(:type ask-updated
                      :ask (:id "approval-1" :state approved))))
         (asks (magpi-observation-asks
                     (magpi-action-observation resolved))))
    (should-not (magpi-observation-asks (magpi-action-observation action)))
    (should (equal (mapcar #'magpi-ask-id asks)
                   '("approval-1" "approval-2")))
    (should (eq (magpi-ask-state (car asks)) 'approved))
    (should (equal (magpi-ask-parent-id (cadr asks)) "approval-1"))
    (should (equal (magpi-ask-affected-paths (cadr asks))
                   '("test/schema_test.ex")))
    (should (eq resolved
                (magpi-action-reduce resolved
                                      '(:type ask-updated
                                        :ask (:id "approval-1" :state approved)))))))

(ert-deftest magpi-action-reduce-ignores-unknown-event-types ()
  (let* ((action (magpi-test-action))
         (again (magpi-action-reduce
                 action '(:type usage-observed :usage (:input 1)))))
    (should (eq again action))))

(ert-deftest magpi-action-reduce-is-identity-for-restated-facts ()
  (let* ((action (magpi-action-reduce
                   (magpi-test-action)
                   '(:type model-observed :model "openai/gpt-4.1")))
         (again (magpi-action-reduce
                 action
                 '(:type model-observed :model "openai/gpt-4.1")))
         (blank (magpi-action-reduce
                 again '(:type title-observed :title "   "))))
    (should (eq again action))
    (should (eq blank action))
    (should (equal (magpi-observation-running-model
                    (magpi-action-observation action))
                   "openai/gpt-4.1"))))

(ert-deftest magpi-prompt-observation-is-a-title-fallback ()
  (let* ((action (magpi-test-action))
         (prompt (magpi-action-reduce
                  action '(:type prompt-observed :prompt "First task")))
         (named (magpi-action-reduce
                 prompt '(:type title-observed :title "Named chat")))
         (later-prompt (magpi-action-reduce
                       named '(:type prompt-observed :prompt "Follow-up"))))
    (should (equal (magpi-observation-display-title
                   (magpi-action-observation prompt)) "First task"))
    (should (equal (magpi-observation-display-title
                   (magpi-action-observation later-prompt)) "Named chat"))))

(provide 'magpi-action-tests)
;;; magpi-action-tests.el ends here
