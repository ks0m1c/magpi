;;; magpi-pimacs-backend-tests.el --- Tests for the Pimacs adapter -*- lexical-binding: t; -*-

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
  (defun pimacs--current-agent () t)
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

(defun magpi-pimacs-test-action (&optional thinking id)
  (make-magpi-action
   :id (or id "action-1234") :prompt "Agent"
   :launch (magpi-launch-build "/tmp/" thinking 'writer '(:kind none))))

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
          (action (magpi-pimacs-test-action 'low "action-123456789abc"))
          (handle (magpi-backend-spawn backend action #'ignore)))
     (should (equal magpi-pimacs-test--flags
                    '("--global" "--thinking" "low")))
     (should (equal magpi-pimacs-test--name "Agent · action-123456789abc"))
     (should magpi-pimacs-test--listener)
     (should-not magpi-pimacs-test--sent)
     (magpi-backend-send-initial backend handle action)
     (should (equal magpi-pimacs-test--sent
                    (list "Agent" nil
                          magpi-pimacs-test--chat magpi-pimacs-test--listener)))
     (let ((first magpi-pimacs-test--sent))
       (magpi-backend-send-initial backend handle action)
       (should (eq magpi-pimacs-test--sent first)))
     (should (equal (magpi-pimacs-handle-id handle) "action-123456789abc")))))

(ert-deftest magpi-pimacs-backend-ungrouped-session-names-are-unique ()
  (let ((first (magpi-pimacs-test-action nil "action-one"))
        (second (magpi-pimacs-test-action nil "action-two")))
    (should-not (equal (magpi-pimacs--session-name first)
                       (magpi-pimacs--session-name second)))
    (should (string-match-p "action-one$"
                          (magpi-pimacs--session-name first)))
    (should (string-match-p "action-two$"
                          (magpi-pimacs--session-name second)))))
(ert-deftest magpi-pimacs-backend-titles-empty-message-from-context ()
  (magpi-pimacs-test-with-backend
   (let* ((action (make-magpi-action
                    :id "action-1234"
                    :launch (magpi-launch-build
                             "/tmp/" 'medium 'writer
                             '(:kind point :file "lib/auth.ex" :line 12))))
          (_handle (magpi-backend-spawn (make-magpi-pimacs-backend) action #'ignore)))
     (should (equal magpi-pimacs-test--name "lib/auth.ex:12 · action-1234")))))
(ert-deftest magpi-pimacs-backend-compiles-explicit-requested-model ()
  (magpi-pimacs-test-with-backend
   (let* ((action (magpi-pimacs-test-action 'low))
          (spec (magpi-action-launch action)))
     (setf (magpi-launch-spec-requested-model spec) "openai/gpt-4.1"
           (magpi-launch-spec-thinking spec) 'high)
     (magpi-backend-spawn (make-magpi-pimacs-backend) action #'ignore)
     (should (equal magpi-pimacs-test--flags
                    '("--global" "--provider" "openai" "--model" "gpt-4.1"
                      "--thinking" "high"))))))

(ert-deftest magpi-pimacs-backend-rejects-unknown-role ()
  (magpi-pimacs-test-with-backend
   (let* ((action (magpi-pimacs-test-action))
          (spec (magpi-action-launch action)))
     (setf (magpi-launch-spec-role spec) 'unknown)
     (should-error (magpi-backend-spawn (make-magpi-pimacs-backend) action #'ignore)))))

(ert-deftest magpi-pimacs-backend-normalizes-raw-events-at-the-boundary ()
  (magpi-pimacs-test-with-backend
   (let (received)
     (let ((backend (make-magpi-pimacs-backend)))
       (magpi-backend-spawn backend (magpi-pimacs-test-action)
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

(ert-deftest magpi-pimacs-backend-normalizes-first-user-prompt-for-title-fallback ()
  (should (equal
           (magpi-pimacs--normalize-events
            '(:type "message_end"
              :message (:role "user" :content "First task")))
           '((:type prompt-observed :prompt "First task")))))

(ert-deftest magpi-pimacs-backend-normalizes-authoritative-model-changes ()
  (should (equal
           (magpi-pimacs--normalize-events
            '(:type "model_change" :provider "anthropic" :modelId "claude-sonnet"))
           '((:type model-observed :model "anthropic/claude-sonnet")))))

(ert-deftest magpi-pimacs-backend-spawn-does-not-pull-snapshots ()
  (magpi-pimacs-test-with-backend
   (magpi-backend-spawn (make-magpi-pimacs-backend)
                       (magpi-pimacs-test-action) #'ignore)
   (should-not (magpi-pimacs-test--callback "get_state"))
   (should-not (magpi-pimacs-test--callback "get_session_stats"))
   (should-not (magpi-pimacs-test--callback "get_available_models"))))

(ert-deftest magpi-pimacs-backend-fill-catalog-from-a-handle ()
  (magpi-pimacs-test-with-backend
   (let* ((backend (make-magpi-pimacs-backend))
          (handle (magpi-backend-spawn backend (magpi-pimacs-test-action)
                                       #'ignore))
          (magpi-launch--catalog (make-hash-table :test #'equal)))
     (setq magpi-pimacs-test--commands nil)
     (should-not (magpi-pimacs-fill-catalog "/tmp/" handle))
     (should (equal (caar magpi-pimacs-test--commands) "get_available_models"))
     (funcall (magpi-pimacs-test--callback "get_available_models")
              '(:success t
                :data (:models [(:provider "anthropic" :id "claude-sonnet")
                                (:provider "openai" :id "gpt-4.1")])))
     (should (equal (magpi-launch-cached-models "/tmp/")
                    '("anthropic/claude-sonnet" "openai/gpt-4.1"))))))

(ert-deftest magpi-pimacs-backend-fill-catalog-issues-nothing-without-a-session ()
  (magpi-pimacs-test-with-backend
   (let ((magpi-pimacs-models-store-file
          (expand-file-name "missing-models-store.json" temporary-file-directory))
         (magpi-launch--catalog (make-hash-table :test #'equal)))
     (should-not (magpi-pimacs-fill-catalog "/tmp/no-session/"))
     (should-not magpi-pimacs-test--commands))))

(ert-deftest magpi-pimacs-backend-fill-catalog-reads-disk-when-cold ()
  (magpi-pimacs-test-with-backend
   (let* ((store (make-temp-file "magpi-models-store" nil ".json"))
          (magpi-pimacs-models-store-file store)
          (magpi-launch--catalog (make-hash-table :test #'equal)))
     (unwind-protect
         (progn
           (with-temp-file store
             (insert (concat
                      "{\"xai\":{\"models\":[{\"id\":\"grok-4.5\"},"
                      "{\"id\":\"grok-4.6\"}]},"
                      "\"openai-codex\":{\"models\":[{\"id\":\"gpt-5.6-sol\"}]}}")))
           (should (equal (magpi-pimacs-fill-catalog "/tmp/no-session/")
                          '("xai/grok-4.5" "xai/grok-4.6"
                            "openai-codex/gpt-5.6-sol")))
           (should-not magpi-pimacs-test--commands))
       (when (file-exists-p store) (delete-file store))))))

(ert-deftest magpi-pimacs-backend-disk-models-ignores-malformed-store ()
  (let ((store (make-temp-file "magpi-models-bad" nil ".json")))
    (unwind-protect
        (progn
          (with-temp-file store (insert "not-json"))
          (should-not (magpi-pimacs--disk-models store)))
      (when (file-exists-p store) (delete-file store)))))

(ert-deftest magpi-pimacs-backend-reconciles-title-and-running-model-state ()
  (magpi-pimacs-test-with-backend
   (let* ((backend (make-magpi-pimacs-backend))
          received
          (handle (magpi-backend-spawn backend (magpi-pimacs-test-action)
                                       (lambda (event) (setq received event)))))
     (setq magpi-pimacs-test--commands nil)
     (magpi-backend-reconcile backend handle
                              (lambda (event) (setq received event)))
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
                 "openai/gpt-4.1"))
  (should (equal (magpi-pimacs--normalize-models
                  '(:models [(:provider "openai" :id "gpt-4.1")
                             (:provider "broken")]))
                 '("openai/gpt-4.1")))
  (should-not (magpi-pimacs--normalize-models '(:models []))))

(ert-deftest magpi-pimacs-backend-reconcile-reissues-get-state ()
  (magpi-pimacs-test-with-backend
   (let* ((backend (make-magpi-pimacs-backend))
          (handle (magpi-backend-spawn backend (magpi-pimacs-test-action)
                                       #'ignore)))
     (setq magpi-pimacs-test--commands nil)
     (magpi-backend-reconcile backend handle #'ignore)
     (should (equal (mapcar #'car magpi-pimacs-test--commands)
                    '("get_state"))))))

(ert-deftest magpi-pimacs-backend-settled-does-not-request-usage ()
  (magpi-pimacs-test-with-backend
   (let* ((backend (make-magpi-pimacs-backend))
          received
          (_handle (magpi-backend-spawn backend (magpi-pimacs-test-action)
                                        (lambda (event)
                                          (push event received)))))
     (setq magpi-pimacs-test--commands nil
           received nil)
     (funcall magpi-pimacs-test--listener '(:type "agent_settled"))
     (should-not magpi-pimacs-test--commands)
     (should (equal received '((:type activity-ended :idle t)))))))

(ert-deftest magpi-pimacs-backend-registers-catalog-filler ()
  (should (eq magpi-launch-catalog-refresh-function
              #'magpi-pimacs-fill-catalog)))

(ert-deftest magpi-pimacs-intention-objective-never-becomes-chat-message ()
  (magpi-pimacs-test-with-backend
   (let* ((backend (make-magpi-pimacs-backend))
          (action (make-magpi-action
                   :id "task-1"
                   :title "Private intention objective"
                   :prompt "Implement token validation" :intention-id "intent-1"
                   :launch (magpi-launch-build "/tmp/" 'medium 'writer
                                               '(:kind none))))
          (handle (magpi-backend-spawn backend action #'ignore)))
     (should (equal magpi-pimacs-test--name "Private intention objective"))
     (magpi-backend-send-initial backend handle action)
     (should (equal (car magpi-pimacs-test--sent) "Implement token validation"))
     (should-not (string-match-p "Private intention objective"
                                 (car magpi-pimacs-test--sent))))))

(provide 'magpi-pimacs-backend-tests)
;;; magpi-pimacs-backend-tests.el ends here
