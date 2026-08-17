;;; magpi-core-tests.el --- Tests for functional Magpi core -*- lexical-binding: t; -*-

(require 'ert)
(require 'magpi-core)

(defun magpi-test-attempt (&optional intent)
  (make-magpi-attempt
   :id "a1" :intent (or intent "Make intent visible")
   :launch (magpi-launch-build default-directory "Default" 'writer '(:kind none))
   :observation (magpi-observation-initial)))

(ert-deftest magpi-attempt-reduce-is-functional ()
  (let* ((attempt (magpi-test-attempt))
         (next (magpi-attempt-reduce
                attempt '(:type activity-started :activity "edit"))))
    (should-not (eq attempt next))
    (should-not (eq (magpi-attempt-observation attempt)
                    (magpi-attempt-observation next)))
    (should (eq (magpi-observation-activity-state
                 (magpi-attempt-observation attempt))
                'starting))
    (should (eq (magpi-observation-activity-state
                 (magpi-attempt-observation next)) 'running))
    (should (equal (magpi-observation-activity
                    (magpi-attempt-observation next)) "edit"))))

(ert-deftest magpi-attempt-reduce-records-semantic-observations ()
  (let* ((attempt (magpi-test-attempt))
         (title (magpi-attempt-reduce
                 attempt '(:type title-observed :title "Derived task")))
         (response (magpi-attempt-reduce
                    title '(:type response-observed
                             :text "Changed implementation.\nTesting now."))))
    (should (equal (magpi-attempt-intent attempt) "Make intent visible"))
    (should (equal (magpi-attempt-intent response) "Make intent visible"))
    (should (equal (magpi-observation-display-title
                    (magpi-attempt-observation response))
                   "Derived task"))
    (should (equal (magpi-observation-last-response
                    (magpi-attempt-observation response))
                   "Changed implementation. Testing now."))))

(ert-deftest magpi-attempt-reduce-deduplicates-normalized-file-evidence ()
  (cl-letf (((symbol-function 'file-truename)
             (lambda (&rest _) (error "Reducer must not access the filesystem")))
            ((symbol-function 'file-in-directory-p)
             (lambda (&rest _) (error "Reducer must not access the filesystem"))))
    (let* ((attempt (magpi-test-attempt))
           (next (seq-reduce
                  (lambda (value path)
                    (magpi-attempt-reduce value
                                          (list :type 'file-observed :path path)))
                  '("lib/auth.ex" "lib/auth.ex")
                  attempt)))
      (should-not (magpi-observation-observed-files
                   (magpi-attempt-observation attempt)))
      (should (equal (magpi-observation-observed-files
                     (magpi-attempt-observation next))
                     '("lib/auth.ex"))))))

(ert-deftest magpi-attempt-reduce-normalizes-empty-observations ()
  (let* ((attempt (magpi-test-attempt))
         (title (magpi-attempt-reduce
                 attempt '(:type title-observed :title "Derived task")))
         (blank-title (magpi-attempt-reduce
                       title '(:type title-observed :title "   ")))
         (blank-response (magpi-attempt-reduce
                          blank-title '(:type response-observed :text " \n\t "))))
    (should (equal (magpi-observation-display-title
                  (magpi-attempt-observation blank-title))
                   "Derived task"))
    (should-not (magpi-observation-last-response
                 (magpi-attempt-observation blank-response)))))

(ert-deftest magpi-attempt-reduce-keeps-problem-and-connection-observations ()
  (let* ((attempt (magpi-test-attempt))
         (problem (magpi-attempt-reduce
                   attempt '(:type problem-observed :problem "extension error")))
         (disconnected (magpi-attempt-reduce problem '(:type disconnected))))
    (should (equal (magpi-observation-problem
                    (magpi-attempt-observation problem))
                   "extension error"))
    (should (eq (magpi-observation-connection-state
                 (magpi-attempt-observation disconnected))
                'disconnected))
    (should (eq (magpi-observation-activity-state
                 (magpi-attempt-observation disconnected))
                'unknown))))

(ert-deftest magpi-backend-observations-never-change-authored-data ()
  (let* ((launch (magpi-launch-build "/tmp/" "Quick" 'writer '(:kind none)))
         (attempt (make-magpi-attempt
                   :id "a1" :intent "Keep this intent" :launch launch
                   :observation (magpi-observation-initial))))
    (dolist (event '((:type activity-started :activity "edit")
                     (:type activity-ended :idle t)
                     (:type file-observed :path "lib/auth.ex")
                     (:type response-observed :text "Done")
                     (:type title-observed :title "Derived")
                     (:type model-observed :model "anthropic/claude-sonnet")
                     (:type usage-observed
                            :usage (:input 10 :output 5 :total 15 :cost 0.01))
                     (:type problem-observed :problem "extension error")
                     (:type disconnected)))
      (setq attempt (magpi-attempt-reduce attempt event))
      (should (equal (magpi-attempt-intent attempt) "Keep this intent"))
      (should (eq (magpi-attempt-launch attempt) launch)))))

(ert-deftest magpi-attempt-reduce-records-usage-observations ()
  (let* ((usage '(:input 50000 :output 10000 :cache-read 40000
                  :cache-write 5000 :total 105000 :cost 0.45
                  :context-tokens 60000 :context-window 200000
                  :context-percent 30.0))
         (attempt (magpi-attempt-reduce
                   (magpi-test-attempt)
                   (list :type 'usage-observed :usage usage)))
         (again (magpi-attempt-reduce
                 attempt
                 (list :type 'usage-observed :usage usage)))
         (nil-usage (magpi-attempt-reduce
                     again '(:type usage-observed :usage nil))))
    (should (equal (magpi-observation-usage
                    (magpi-attempt-observation attempt))
                   usage))
    (should (eq again attempt))
    (should (eq nil-usage attempt))))

(ert-deftest magpi-attempt-reduce-is-identity-for-restated-facts ()
  (let* ((attempt (magpi-attempt-reduce
                   (magpi-test-attempt)
                   '(:type model-observed :model "openai/gpt-4.1")))
         (again (magpi-attempt-reduce
                 attempt
                 '(:type model-observed :model "openai/gpt-4.1")))
         (blank (magpi-attempt-reduce
                 again '(:type title-observed :title "   "))))
    (should (eq again attempt))
    (should (eq blank attempt))
    (should (equal (magpi-observation-running-model
                    (magpi-attempt-observation attempt))
                   "openai/gpt-4.1"))))

(provide 'magpi-core-tests)
;;; magpi-core-tests.el ends here
