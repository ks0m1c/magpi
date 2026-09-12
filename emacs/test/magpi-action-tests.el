;;; magpi-action-tests.el --- Tests for functional Magpi action domain -*- lexical-binding: t; -*-

(require 'ert)
(require 'magpi-action)
(require 'magpi-launch)
(require 'magpi-test-repo)

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
                 action '(:type unicorn-observed :usage (:input 1)))))
    (should (eq again action))))

(ert-deftest magpi-action-reduce-records-usage ()
  (let* ((action (magpi-test-action))
         (usage '(:input 12400 :output 3100 :cost 0.042 :context 9000
                  :window 200000))
         (observed (magpi-action-reduce
                    action (list :type 'usage-observed :usage usage)))
         (again (magpi-action-reduce
                 observed (list :type 'usage-observed :usage usage))))
    (should (equal (magpi-observation-usage
                    (magpi-action-observation observed))
                   usage))
    (should (eq again observed))))

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
    (should (equal (magpi-observation-last-prompt
                   (magpi-action-observation prompt)) "First task"))
    (should (equal (magpi-observation-display-title
                   (magpi-action-observation later-prompt)) "Named chat"))
    (should (equal (magpi-observation-last-prompt
                   (magpi-action-observation later-prompt)) "Follow-up"))))

(ert-deftest magpi-observation-auspice-first-match ()
  "Promise: auspice is a first-match projection of Observation, not a field."
  (should (eq (magpi-observation-auspice nil) 'cold))
  (should (eq (magpi-observation-auspice
               (make-magpi-observation :connection-state 'disconnected))
              'blood))
  (should (eq (magpi-observation-auspice
               (make-magpi-observation :activity-state 'unknown
                                       :connection-state 'connected))
              'blood))
  (should (eq (magpi-observation-auspice
               (make-magpi-observation :problem "extension error"
                                       :activity-state 'starting
                                       :connection-state 'connected))
              'blood))
  (should (eq (magpi-observation-auspice
               (make-magpi-observation :activity-state 'starting
                                       :connection-state 'connected))
              'lift))
  (should (eq (magpi-observation-auspice
               (make-magpi-observation :activity-state 'running
                                       :connection-state 'connected))
              'aloft))
  (should (eq (magpi-observation-auspice
               (make-magpi-observation :activity-state 'idle
                                       :connection-state 'connected))
              'rest))
  (should (eq (magpi-observation-auspice (make-magpi-observation)) 'cold))
  (should (eq (magpi-observation-auspice (magpi-observation-initial)) 'lift)))

(ert-deftest magpi-observation-auspice-ask-requested-does-not-recode ()
  (let* ((action (magpi-test-action))
         (asked (magpi-action-reduce
                 action
                 '(:type ask-requested
                   :ask (:id "approval-1" :question "Apply?"))))
         (running (magpi-action-reduce
                   asked '(:type activity-started :activity "edit")))
         (asked-running (magpi-action-reduce
                         running
                         '(:type ask-requested
                           :ask (:id "approval-2" :question "Continue?")))))
    (should (eq (magpi-observation-auspice (magpi-action-observation action))
                'lift))
    (should (eq (magpi-observation-auspice (magpi-action-observation asked))
                'lift))
    (should (eq (magpi-observation-auspice (magpi-action-observation running))
                'aloft))
    (should (eq (magpi-observation-auspice
                 (magpi-action-observation asked-running))
                'aloft))))

(ert-deftest magpi-action-reduce-telemetry-does-not-birth-lift ()
  "Promise: model-only telemetry on a cold Action is not starting/connected."
  (let* ((action (make-magpi-action :id "cold" :chat-ref "cold"))
         (modeled (magpi-action-reduce
                   action '(:type model-observed :model "openai/gpt-4.1")))
         (ignored (magpi-action-reduce
                   action '(:type unicorn-observed :usage (:input 1)))))
    (should-not (magpi-action-observation action))
    (should (eq ignored action))
    (should-not (magpi-action-observation ignored))
    (let ((observation (magpi-action-observation modeled)))
      (should (equal (magpi-observation-running-model observation)
                     "openai/gpt-4.1"))
      (should-not (magpi-observation-activity-state observation))
      (should-not (magpi-observation-connection-state observation))
      (should (eq (magpi-observation-auspice observation) 'cold)))))

(ert-deftest magpi-action-reduce-reconnected-is-the-connection-fact ()
  "Promise: disconnect plus running stays blood until reconnected."
  (let* ((action (magpi-action-reduce (magpi-test-action) '(:type disconnected)))
         (running (magpi-action-reduce
                   action '(:type activity-started :activity "thinking")))
         (live (magpi-action-reduce running '(:type reconnected)))
         (cold (magpi-action-reduce
                (make-magpi-action :id "cold" :chat-ref "cold")
                '(:type reconnected))))
    (should (eq (magpi-observation-auspice (magpi-action-observation running))
                'blood))
    (should (eq (magpi-observation-connection-state
                 (magpi-action-observation running))
                'disconnected))
    (should (eq (magpi-observation-activity-state
                 (magpi-action-observation running))
                'running))
    (should (eq (magpi-observation-connection-state
                 (magpi-action-observation live))
                'connected))
    (should (eq (magpi-observation-activity-state
                 (magpi-action-observation live))
                'running))
    (should (eq (magpi-observation-auspice (magpi-action-observation live))
                'aloft))
    (should (eq (magpi-observation-activity-state
                 (magpi-action-observation cold))
                'starting))
    (should (eq (magpi-observation-connection-state
                 (magpi-action-observation cold))
                'connected))
    (should (eq (magpi-observation-auspice (magpi-action-observation cold))
                'lift))))

(ert-deftest magpi-action-disk-omits-observation-and-keeps-chat-ref-monotonic ()
  "Promise: Action files store pointers, not theatre; chat-ref only advances."
  (magpi-test-with-repo (repository "magpi-action-persist-")
    (let* ((action (make-magpi-action
                    :id "act1"
                    :intention-id "intent-1"
                    :source-root repository
                    :created-at 100
                    :chat-ref "act1"
                    :observation (magpi-observation-initial)
                    :prompt "RAM only"
                    :extras '(:future-field "keep")))
           loaded)
      (should (eq action (magpi-action-save action)))
      (setq loaded (magpi-action-load repository "act1"))
      (should (equal (magpi-action-intention-id loaded) "intent-1"))
      (should-not (magpi-action-observation loaded))
      (should (eq (magpi-observation-auspice (magpi-action-observation loaded))
                  'cold))
      (should-not (magpi-action-prompt loaded))
      (should (equal (magpi-action-chat-ref loaded) "act1"))
      (should (equal (plist-get (magpi-action-extras loaded) :future-field) "keep"))
      (let ((data (magpi-store-read (magpi-store-file repository 'actions "act1"))))
        (should-not (plist-member data :version))
        (should-not (plist-member data :type))
        (should-not (plist-member data :observation)))
      (should (eq loaded (magpi-action-set-chat-ref loaded "act1")))
      (should-error (magpi-action-set-chat-ref loaded "pimacs:other")))))

(ert-deftest magpi-action-save-does-not-invent-identity ()
  "Promise: save never setfs chat-ref or created-at."
  (magpi-test-with-repo (repository "magpi-action-save-pure-")
    (let ((action (make-magpi-action
                   :id "x" :source-root repository
                   :created-at 1 :chat-ref "x")))
      (should (eq action (magpi-action-save action)))
      (should (equal (magpi-action-chat-ref action) "x"))
      (should (equal (magpi-action-created-at action) 1)))
    (should-error (magpi-action-save
                   (make-magpi-action :id "y" :source-root repository
                                      :created-at 1)))
    (should-error (magpi-action-save
                   (make-magpi-action :id "z" :source-root repository
                                      :chat-ref "z")))))

(ert-deftest magpi-action-standalone-stores-spawn-oid-not-live-head ()
  "Promise: standalone watermark is birth HEAD; no last-known tip."
  (magpi-test-with-repo (repository "magpi-action-standalone-")
    (let* ((oid (string-trim (magpi-test-repo-git repository "rev-parse" "HEAD")))
           (action (make-magpi-action
                    :id "solo"
                    :source-root repository
                    :spawn-oid oid
                    :created-at 1
                    :chat-ref "solo"
                    :title "Standalone")))
      (magpi-action-save action)
      (setq action (magpi-action-load repository "solo"))
      (should (equal (magpi-action-spawn-oid action) oid))
      (should (equal (magpi-action-title action) "Standalone"))
      (should-not (magpi-action-intention-id action))
      (should-not (plist-member (magpi-action--plist action) :intention-id))
      (should-not (plist-member (magpi-action--plist action) :version))
      (should-not (plist-member (magpi-action--plist action) :observation)))))

(provide 'magpi-action-tests)
;;; magpi-action-tests.el ends here
