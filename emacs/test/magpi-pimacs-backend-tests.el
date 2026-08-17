;;; magpi-pimacs-backend-tests.el --- Tests for the Lean Cut backend -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)

;; Keep the adapter testable without loading the full Pimacs UI or a real Pi
;; process.  The production adapter is exercised unchanged against this small
;; compatibility surface.
(unless (require 'pimacs nil t)
  (defvar pimacs-flags nil)
  (defvar pimacs-chat-mode-map (make-sparse-keymap))
  (defvar magpi-pimacs-test--chat nil)
  (defvar magpi-pimacs-test--flags nil)
  (defvar magpi-pimacs-test--name nil)
  (defvar magpi-pimacs-test--listener nil)
  (defvar magpi-pimacs-test--sent nil)
  (defvar magpi-pimacs-test--terminated nil)
  (defvar magpi-pimacs-test--commands nil)
  (defvar-local pimacs--project-key nil)
  (defun pimacs-chat (name _root)
    (setq magpi-pimacs-test--name name)
    (setq magpi-pimacs-test--flags pimacs-flags)
    (setq magpi-pimacs-test--chat (get-buffer-create " *magpi-pimacs-test*"))
    (switch-to-buffer magpi-pimacs-test--chat)
    (setq-local pimacs--project-key "test-project"))
  (defun pimacs--current-agent () nil)
  (defun pimacs--set-event-listener (_name _id listener)
    (setq magpi-pimacs-test--listener listener))
  (defun pimacs--agent-add-cleanup (_agent _cleanup) nil)
  (defun pimacs-send-prompt (message mode)
    (setq magpi-pimacs-test--sent
          (list message mode (current-buffer) magpi-pimacs-test--listener)))
  (defun pimacs--kill-agent (&optional cleanup)
    (setq magpi-pimacs-test--terminated cleanup))
  (defun pimacs--send-command (type args &optional callback)
    (push (list type args callback) magpi-pimacs-test--commands))
  (defun pimacs--response-success-p (response)
    (plist-get response :success))
  (provide 'pimacs))

(require 'magpi-pimacs-backend)

(defun magpi-pimacs-test-attempt (&optional profile id)
  (make-magpi-attempt
   :id (or id "attempt-1234") :intent "Agent"
   :launch (magpi-launch-build "/tmp/"
                                    (or profile "Default") 'writer
                                    '(:kind none))))

(defun magpi-pimacs-test--callback (type)
  (nth 2 (cl-find-if (lambda (command) (equal (car command) type))
                     magpi-pimacs-test--commands)))

(defmacro magpi-pimacs-test-with-backend (&rest body)
  `(let ((pimacs-flags '("--global"))
         (magpi-pimacs-test--chat nil)
         (magpi-pimacs-test--flags nil)
         (magpi-pimacs-test--name nil)
         (magpi-pimacs-test--listener nil)
         (magpi-pimacs-test--sent nil)
         (magpi-pimacs-test--terminated nil)
         (magpi-pimacs-test--commands nil))
     ,@body))

(ert-deftest magpi-pimacs-backend-subscribes-before-sending-initial-prompt ()
  (magpi-pimacs-test-with-backend
   (let* ((backend (make-magpi-pimacs-backend))
          (attempt (magpi-pimacs-test-attempt "Quick" "attempt-123456789abc"))
          (handle (magpi-backend-spawn backend attempt #'ignore)))
     (should (equal magpi-pimacs-test--flags
                    '("--global" "--thinking" "low")))
     (should (equal magpi-pimacs-test--name "Agent · attempt-123456789abc"))
     (should magpi-pimacs-test--listener)
     (should-not magpi-pimacs-test--sent)
     (magpi-backend-send-initial backend handle attempt)
     (should (equal magpi-pimacs-test--sent
                    (list "Agent" nil
                          magpi-pimacs-test--chat magpi-pimacs-test--listener)))
     (let ((first magpi-pimacs-test--sent))
       (magpi-backend-send-initial backend handle attempt)
       (should (eq magpi-pimacs-test--sent first)))
     (should (equal (magpi-pimacs-handle-id handle) "attempt-123456789abc")))))

(ert-deftest magpi-pimacs-backend-compiles-explicit-requested-model ()
  (magpi-pimacs-test-with-backend
   (let* ((attempt (magpi-pimacs-test-attempt "Quick"))
          (spec (magpi-attempt-launch attempt)))
     (setf (magpi-launch-spec-requested-model spec) "openai/gpt-4.1"
           (magpi-launch-spec-thinking spec) 'high)
     (magpi-backend-spawn (make-magpi-pimacs-backend) attempt #'ignore)
     (should (equal magpi-pimacs-test--flags
                    '("--global" "--provider" "openai" "--model" "gpt-4.1"
                      "--thinking" "high"))))))

(ert-deftest magpi-pimacs-backend-rejects-unknown-authority ()
  (magpi-pimacs-test-with-backend
   (let* ((attempt (magpi-pimacs-test-attempt))
          (spec (magpi-attempt-launch attempt)))
     (setf (magpi-launch-spec-authority spec) 'unknown)
     (should-error (magpi-backend-spawn (make-magpi-pimacs-backend) attempt #'ignore)))))

(ert-deftest magpi-pimacs-backend-normalizes-raw-events-at-the-boundary ()
  (magpi-pimacs-test-with-backend
   (let (received)
     (let ((backend (make-magpi-pimacs-backend)))
       (magpi-backend-spawn backend (magpi-pimacs-test-attempt)
                            (lambda (event) (push event received))))
     (funcall magpi-pimacs-test--listener
              '(:type "tool_execution_start" :toolName "edit"
                :args (:path "./lib/auth.ex")))
     (should (equal (nreverse received)
                    '((:type activity-started :activity "edit")
                      (:type file-observed :path "lib/auth.ex"))))
     (should (equal
              (magpi-pimacs--normalize-events
               '(:type "message_end"
                 :message (:role "assistant"
                           :content ((:type "text" :text "Done.")))))
              '((:type response-observed :text "Done."))))
     (should (equal (magpi-pimacs--normalize-events
                     '(:type "agent_settled"))
                    '((:type activity-ended :idle t))))
     (should (equal (magpi-pimacs--normalize-events
                     '(:type "extension_error"))
                    '((:type problem-observed :problem "extension error")))))))

(ert-deftest magpi-pimacs-backend-normalizes-authoritative-model-changes ()
  (should (equal
           (magpi-pimacs--normalize-events
            '(:type "model_change" :provider "anthropic" :modelId "claude-sonnet"))
           '((:type model-observed :model "anthropic/claude-sonnet")))))

(ert-deftest magpi-pimacs-backend-normalizes-session-stats-usage ()
  (should-not (magpi-pimacs--normalize-usage nil))
  (should-not (magpi-pimacs--normalize-usage '(:tokens json-null)))
  (should (equal
           (magpi-pimacs--normalize-usage
            '(:tokens (:input 50000 :output 10000 :cacheRead 40000
                       :cacheWrite 5000 :total 105000)
              :cost 0.45
              :contextUsage (:tokens 60000 :contextWindow 200000
                             :percent 30.0)))
           '(:input 50000 :output 10000 :cache-read 40000
             :cache-write 5000 :total 105000 :cost 0.45
             :context-tokens 60000 :context-window 200000
             :context-percent 30.0)))
  (should (equal
           (magpi-pimacs--normalize-usage
            '(:tokens (:input 1 :output 2 :cacheRead 0 :cacheWrite 0 :total 3)
              :cost 0
              :contextUsage (:tokens json-null :contextWindow 200000
                             :percent json-null)))
           '(:input 1 :output 2 :cache-read 0 :cache-write 0 :total 3
             :cost 0 :context-tokens nil :context-window 200000
             :context-percent nil))))

(ert-deftest magpi-pimacs-backend-reconciles-title-and-running-model-state ()
  (magpi-pimacs-test-with-backend
   (let* ((backend (make-magpi-pimacs-backend))
          received
          (_handle (magpi-backend-spawn backend (magpi-pimacs-test-attempt)
                                        (lambda (event) (setq received event)))))
     (funcall (magpi-pimacs-test--callback "get_state")
              '(:success t :data (:sessionName "Derived task title"
                                  :model (:provider "openai" :id "gpt-4.1"))))
     (should (equal received
                    '(:type model-observed :model "openai/gpt-4.1"))))))

(ert-deftest magpi-pimacs-backend-ignores-null-or-incomplete-models ()
  (should-not (magpi-pimacs--model-identifier 'json-null))
  (should-not (magpi-pimacs--model-identifier '(:provider "openai")))
  (should (equal (magpi-pimacs--model-identifier
                  '(:provider "openai" :modelId "gpt-4.1"))
                 "openai/gpt-4.1")))

(ert-deftest magpi-pimacs-backend-reconcile-reissues-get-state ()
  (magpi-pimacs-test-with-backend
   (let* ((backend (make-magpi-pimacs-backend))
          (handle (magpi-backend-spawn backend (magpi-pimacs-test-attempt)
                                       #'ignore)))
     (setq magpi-pimacs-test--commands nil)
     (magpi-backend-reconcile backend handle #'ignore)
     (should (equal (mapcar #'car magpi-pimacs-test--commands)
                    '("get_session_stats" "get_state"))))))

(ert-deftest magpi-pimacs-backend-requests-usage-after-agent-settled ()
  (magpi-pimacs-test-with-backend
   (let* ((backend (make-magpi-pimacs-backend))
          received
          (_handle (magpi-backend-spawn backend (magpi-pimacs-test-attempt)
                                        (lambda (event)
                                          (push event received)))))
     (setq magpi-pimacs-test--commands nil
           received nil)
     (funcall magpi-pimacs-test--listener '(:type "agent_settled"))
     (should (equal (caar magpi-pimacs-test--commands) "get_session_stats"))
     (funcall (magpi-pimacs-test--callback "get_session_stats")
              '(:success t
                :data (:tokens (:input 10 :output 5 :cacheRead 0
                                :cacheWrite 0 :total 15)
                        :cost 0.01)))
     (should (equal received
                    '((:type usage-observed
                       :usage (:input 10 :output 5 :cache-read 0
                               :cache-write 0 :total 15 :cost 0.01
                               :context-tokens nil :context-window nil
                               :context-percent nil))
                      (:type activity-ended :idle t)))))))

(provide 'magpi-pimacs-backend-tests)
;;; magpi-pimacs-backend-tests.el ends here
