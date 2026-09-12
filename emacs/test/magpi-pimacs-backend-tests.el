;;; magpi-pimacs-backend-tests.el --- Tests for the Pimacs adapter -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)

;; Keep the adapter testable without loading the full Pimacs UI or a real Pi
;; process.  The production adapter is exercised unchanged against this small
;; compatibility surface.
(unless (require 'pimacs nil t)
  (defvar pimacs-flags nil)
  (defvar pimacs-chat-mode-map (make-sparse-keymap))
  (defvar pimacs--agents (make-hash-table :test 'equal))
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
    (setq-local pimacs--project-key "test-project")
    (puthash "test-project" t pimacs--agents))
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
     (when (boundp 'pimacs--agents)
       (clrhash pimacs--agents))
     ,@body))

(ert-deftest magpi-pimacs-backend-subscribes-before-sending-initial-prompt ()
  (magpi-pimacs-test-with-backend
   (let* ((backend (make-magpi-pimacs-backend))
          (action (magpi-pimacs-test-action 'low "action-123456789abc"))
          (handle (magpi-backend-spawn backend action #'ignore)))
     (should (equal magpi-pimacs-test--flags
                    '("--global" "--session-id" "action-123456789abc"
                      "--thinking" "low")))
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

(ert-deftest magpi-pimacs-backend-session-names-include-the-action-id ()
  (let ((first (magpi-pimacs-test-action nil "action-one"))
        (second (magpi-pimacs-test-action nil "action-two"))
        (grouped (make-magpi-action
                  :id "task-1" :title "Shared objective" :intention-id "intent-1"
                  :launch (magpi-launch-build "/tmp/" nil 'writer '(:kind none)))))
    (should-not (equal (magpi-pimacs--session-name first)
                       (magpi-pimacs--session-name second)))
    (should (string-match-p "action-one$" (magpi-pimacs--session-name first)))
    (should (string-match-p "action-two$" (magpi-pimacs--session-name second)))
    (should (string-match-p "task-1$" (magpi-pimacs--session-name grouped)))))
(ert-deftest magpi-pimacs-backend-titles-empty-message-from-context ()
  (magpi-pimacs-test-with-backend
   (let* ((action (make-magpi-action
                    :id "action-1234"
                    :launch (magpi-launch-build
                             "/tmp/" 'medium 'writer
                             '(:kind point :file "lib/auth.ex" :line 12))))
          (_handle (magpi-backend-spawn (make-magpi-pimacs-backend) action #'ignore)))
     (should (equal magpi-pimacs-test--name "lib/auth.ex:12 · action-1234")))))

(ert-deftest magpi-pimacs-backend-sends-launch-source-when-prompt-is-nil ()
  (magpi-pimacs-test-with-backend
   (let* ((backend (make-magpi-pimacs-backend))
          (action (make-magpi-action
                    :id "action-1234"
                    :launch (magpi-launch-build
                             "/tmp/" 'medium 'writer
                             '(:kind point :file "lib/auth.ex" :line 12
                               :text "refresh()"))))
          (handle (magpi-backend-spawn backend action #'ignore)))
     (should-not magpi-pimacs-test--sent)
     (magpi-backend-send-initial backend handle action)
     (should (string-match-p "lib/auth.ex:12" (car magpi-pimacs-test--sent)))
     (should (string-match-p "refresh()" (car magpi-pimacs-test--sent))))))

(ert-deftest magpi-pimacs-backend-nil-prompt-without-source-sends-nothing ()
  (magpi-pimacs-test-with-backend
   (let* ((backend (make-magpi-pimacs-backend))
          (action (make-magpi-action
                    :id "action-1234"
                    :launch (magpi-launch-build
                             "/tmp/" 'medium 'writer '(:kind none))))
          (handle (magpi-backend-spawn backend action #'ignore)))
     (magpi-backend-send-initial backend handle action)
     (should-not magpi-pimacs-test--sent))))
(ert-deftest magpi-pimacs-backend-compiles-explicit-requested-model ()
  (magpi-pimacs-test-with-backend
   (let* ((action (magpi-pimacs-test-action 'low))
          (spec (magpi-action-launch action)))
     (setf (magpi-launch-spec-requested-model spec) "openai/gpt-4.1"
           (magpi-launch-spec-thinking spec) 'high)
     (magpi-backend-spawn (make-magpi-pimacs-backend) action #'ignore)
     (should (equal magpi-pimacs-test--flags
                    '("--global" "--session-id" "action-1234"
                      "--provider" "openai" "--model" "gpt-4.1"
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
   (let (refreshed)
     (cl-letf (((symbol-function 'pimacs-refresh-session)
                (lambda (&optional _) (setq refreshed t))))
       (magpi-backend-spawn (make-magpi-pimacs-backend)
                            (magpi-pimacs-test-action) #'ignore)
       (should-not refreshed)
       (should-not (magpi-pimacs-test--callback "get_state"))
       (should-not (magpi-pimacs-test--callback "get_session_stats"))
       (should-not (magpi-pimacs-test--callback "get_available_models"))))))

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
                    '("get_session_stats" "get_state"))))))

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

(ert-deftest magpi-pimacs-backend-normalizes-session-stats ()
  (should-not (magpi-pimacs--normalize-usage nil))
  (should-not (magpi-pimacs--normalize-usage '(:tokens json-null)))
  (should (equal (magpi-pimacs--normalize-usage
                  '(:tokens (:input 12 :output 3 :total 15)
                    :cost 0.01
                    :contextUsage (:tokens 40 :contextWindow 200000)))
                 '(:input 12 :output 3 :total 15 :cost 0.01
                   :context 40 :window 200000))))

(ert-deftest magpi-pimacs-backend-reconciles-usage ()
  (magpi-pimacs-test-with-backend
   (let* ((backend (make-magpi-pimacs-backend))
          received
          (handle (magpi-backend-spawn backend (magpi-pimacs-test-action)
                                       #'ignore)))
     (setq magpi-pimacs-test--commands nil)
     (magpi-backend-reconcile backend handle
                              (lambda (event) (setq received event)))
     (funcall (magpi-pimacs-test--callback "get_session_stats")
              '(:success t
                :data (:tokens (:input 12400 :output 3100 :total 15500)
                       :cost 0.042
                       :contextUsage (:tokens 9000 :contextWindow 200000))))
     (should (equal received
                    '(:type usage-observed
                      :usage (:input 12400 :output 3100 :total 15500
                              :cost 0.042 :context 9000 :window 200000)))))))
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
     (should (equal magpi-pimacs-test--name "Private intention objective · task-1"))
     (magpi-backend-send-initial backend handle action)
     (should (equal (car magpi-pimacs-test--sent) "Implement token validation"))
     (should-not (string-match-p "Private intention objective"
                                 (car magpi-pimacs-test--sent))))))

(ert-deftest magpi-pimacs-backend-session-ref-is-the-action-id ()
  (magpi-pimacs-test-with-backend
   (let* ((backend (make-magpi-pimacs-backend))
          (handle (magpi-backend-spawn backend (magpi-pimacs-test-action)
                                       #'ignore)))
     (should (equal (magpi-backend-session-ref backend handle) "action-1234")))))

(ert-deftest magpi-pimacs-backend-live-p-is-the-agent-process ()
  (magpi-pimacs-test-with-backend
   (let* ((backend (make-magpi-pimacs-backend))
          (handle (magpi-backend-spawn backend (magpi-pimacs-test-action)
                                       #'ignore))
          (key (magpi-pimacs-handle-key handle))
          (chat (magpi-pimacs-handle-chat-buffer handle)))
     (should (equal key "test-project"))
     (cl-letf (((symbol-function 'process-live-p)
                (lambda (proc) (eq proc 'alive-agent))))
       (puthash key 'alive-agent pimacs--agents)
       (should (magpi-backend-live-p backend handle))
       (puthash key 'dead-agent pimacs--agents)
       (should-not (magpi-backend-live-p backend handle))
       (remhash key pimacs--agents)
       (should-not (magpi-backend-live-p backend handle))
       (should (buffer-live-p chat))))))

(ert-deftest magpi-pimacs-backend-session-id-replaces-an-earlier-flag ()
  (magpi-pimacs-test-with-backend
   (let ((pimacs-flags '("--session-id" "other" "--global")))
     (magpi-backend-spawn (make-magpi-pimacs-backend)
                          (magpi-pimacs-test-action) #'ignore)
     (should (equal magpi-pimacs-test--flags
                    '("--global" "--session-id" "action-1234"))))))


(ert-deftest magpi-pimacs-backend-spawn-drops-parent-pi-session ()
  (magpi-pimacs-test-with-backend
   (let ((process-environment
          (append '("PI_SESSION_ID=parent-session"
                    "PI_SESSION_FILE=/tmp/parent.jsonl")
                  process-environment))
         seen-id seen-file)
     (cl-letf (((symbol-function 'pimacs-chat)
                (let ((orig (symbol-function 'pimacs-chat)))
                  (lambda (&rest args)
                    (setq seen-id (getenv "PI_SESSION_ID")
                          seen-file (getenv "PI_SESSION_FILE"))
                    (apply orig args)))))
       (magpi-backend-spawn (make-magpi-pimacs-backend)
                            (magpi-pimacs-test-action) #'ignore))
     (should-not seen-id)
     (should-not seen-file))))
(ert-deftest magpi-pimacs-backend-visit-rebinds-a-killed-chat ()
  (magpi-pimacs-test-with-backend
   (let* ((backend (make-magpi-pimacs-backend))
          (handle (magpi-backend-spawn backend (magpi-pimacs-test-action)
                                       #'ignore))
          (old (magpi-pimacs-handle-chat-buffer handle)))
     (kill-buffer old)
     (should-not (buffer-live-p old))
     (unwind-protect
         (progn
           (magpi-backend-visit backend handle)
           (should (buffer-live-p (magpi-pimacs-handle-chat-buffer handle)))
           (should (equal magpi-pimacs-test--name "Agent · action-1234")))
       (magpi-pimacs--history-watch-stop handle)))))

(ert-deftest magpi-pimacs-backend-visit-pulls-session-history ()
  (magpi-pimacs-test-with-backend
   (let* ((backend (make-magpi-pimacs-backend))
          (handle (magpi-backend-spawn backend (magpi-pimacs-test-action)
                                       #'ignore))
          refreshed)
     (unwind-protect
         (cl-letf (((symbol-function 'pimacs-refresh-session)
                    (lambda (&optional callback)
                      (setq refreshed (current-buffer))
                      (when callback (funcall callback)))))
           (magpi-backend-visit backend handle)
           (should (eq refreshed (magpi-pimacs-handle-chat-buffer handle)))
           (should-not (magpi-pimacs-handle-awaiting-entries handle)))
       (magpi-pimacs--history-watch-stop handle)))))

(ert-deftest magpi-pimacs-backend-visit-skips-history-when-already-shown ()
  (magpi-pimacs-test-with-backend
   (let* ((backend (make-magpi-pimacs-backend))
          (handle (magpi-backend-spawn backend (magpi-pimacs-test-action)
                                       #'ignore))
          refreshed)
     (cl-letf (((symbol-function 'magpi-pimacs--history-rendered-p)
                (lambda (&optional _) t))
               ((symbol-function 'pimacs-refresh-session)
                (lambda (&optional _) (setq refreshed t))))
       (magpi-backend-visit backend handle)
       (should-not refreshed)))))

(ert-deftest magpi-pimacs-backend-history-fill-is-loading-not-a-turn ()
  (magpi-pimacs-test-with-backend
   (let* (events
          (backend (make-magpi-pimacs-backend))
          (handle (magpi-backend-spawn backend (magpi-pimacs-test-action)
                                       (lambda (event) (push event events)))))
     (unwind-protect
         (cl-letf (((symbol-function 'pimacs-refresh-session)
                    (lambda (&optional _) nil)))
           (magpi-backend-visit backend handle)
           (should (magpi-pimacs-handle-awaiting-entries handle))
           (should (equal (magpi-backend-history-pending backend handle)
                          "loading"))
           (should (equal (car events) '(:type history-pending)))
           (setf (magpi-pimacs-handle-awaiting-entries handle) nil)
           (with-current-buffer (magpi-pimacs-handle-chat-buffer handle)
             (setq-local pimacs--history-render-pending '((a b c))))
           (should (equal (magpi-backend-history-pending backend handle)
                          "loading 3")))
       (magpi-pimacs--history-watch-stop handle)))))
(ert-deftest magpi-pimacs-session-label-prefers-meaningful-name ()
  (should (equal (magpi-pimacs--session-label "Repair auth" "ignored first"
                                              "later user")
                 "Repair auth"))
  (should (equal (magpi-pimacs--session-label "New chat · deadbeef" "First user"
                                              "Later user")
                 "First user"))
  (should (equal (magpi-pimacs--session-label "New task · deadbeef" "First user")
                 "First user"))
  (should (equal (magpi-pimacs--session-label "◯ · deadbeef" nil "Later user")
                 "Later user"))
  (should-not (magpi-pimacs--session-label "New chat · deadbeef" "   ")))

(ert-deftest magpi-pimacs-read-session-file-peeks-user-then-last-assistant ()
  (let* ((dir (make-temp-file "magpi-session-dir" t))
         (file (expand-file-name "2026-01-01T00-00-00-000Z_sess-1.jsonl" dir)))
    (unwind-protect
        (progn
          (with-temp-file file
            (insert
             (concat
              "{\"type\":\"session\",\"id\":\"sess-1\",\"cwd\":\"/tmp/proj\"}\n"
              "{\"type\":\"session_info\",\"name\":\"New chat · sess-1\"}\n"
              "{\"type\":\"message\",\"message\":{\"role\":\"assistant\","
              "\"content\":[{\"type\":\"text\",\"text\":\"Nope\"}]}}\n"
              "{\"type\":\"message\",\"message\":{\"role\":\"user\","
              "\"content\":[{\"type\":\"text\",\"text\":\"Real task\"}]}}\n"
              "{\"type\":\"message\",\"message\":{\"role\":\"assistant\","
              "\"content\":[{\"type\":\"text\",\"text\":\"Last activity\"}]}}\n")))
          (let ((session (magpi-pimacs--read-session-file file)))
            (should (equal (plist-get session :id) "sess-1"))
            (should (equal (plist-get session :first-user) "Real task"))
            (should (equal (plist-get session :last-user) "Real task"))
            (should (equal (plist-get session :last-activity) "Last activity"))
            (should (equal (magpi-pimacs--session-label
                            (plist-get session :name)
                            (plist-get session :first-user)
                            (plist-get session :last-user))
                           "Real task"))))
      (when (file-directory-p dir) (delete-directory dir t)))))

(ert-deftest magpi-pimacs-chat-candidates-read-disk-without-spawn ()
  (let* ((store (make-temp-file "magpi-sessions" t))
         (root (file-name-as-directory (make-temp-file "magpi-root" t)))
         (project (expand-file-name (magpi-pimacs--encode-cwd root) store))
         (id "hist-1")
         (file (expand-file-name (format "2026-01-01T00-00-00-000Z_%s.jsonl" id)
                                 project))
         (pimacs-flags nil)
         (process-environment
          (cons (format "PI_CODING_AGENT_SESSION_DIR=%s" store)
                process-environment))
         spawned)
    (unwind-protect
        (progn
          (make-directory project t)
          (with-temp-file file
            (insert
             (format
              (concat
               "{\"type\":\"session\",\"id\":\"%s\",\"cwd\":\"%s\"}\n"
               "{\"type\":\"session_info\",\"name\":\"New chat · %s\"}\n"
               "{\"type\":\"message\",\"message\":{\"role\":\"user\","
               "\"content\":[{\"type\":\"text\",\"text\":\"Historical why\"}]}}\n")
              id (directory-file-name root) id)))
          (cl-letf (((symbol-function 'pimacs-chat)
                     (lambda (&rest _) (setq spawned t))))
            (let ((candidates (magpi-backend-chat-candidates
                               (make-magpi-pimacs-backend) root)))
              (should-not spawned)
              (should (equal candidates
                             (list (list :reference "pimacs:hist-1"
                                         :label "Historical why")))))))
      (when (file-directory-p store) (delete-directory store t))
      (when (file-directory-p root) (delete-directory root t)))))

(ert-deftest magpi-pimacs-chat-candidates-order-by-last-activity ()
  (let* ((store (make-temp-file "magpi-sessions" t))
         (root (file-name-as-directory (make-temp-file "magpi-root" t)))
         (project (expand-file-name (magpi-pimacs--encode-cwd root) store))
         (old (expand-file-name "2026-01-01T00-00-00-000Z_old-1.jsonl" project))
         (new (expand-file-name "2026-01-02T00-00-00-000Z_new-1.jsonl" project))
         (process-environment
          (cons (format "PI_CODING_AGENT_SESSION_DIR=%s" store)
                process-environment)))
    (unwind-protect
        (progn
          (make-directory project t)
          (cl-labels ((write-session (file id text)
                        (with-temp-file file
                          (insert (format
                                   (concat
                                    "{\"type\":\"session\",\"id\":\"%s\",\"cwd\":\"%s\"}\n"
                                    "{\"type\":\"session_info\",\"name\":\"New chat · %s\"}\n"
                                    "{\"type\":\"message\",\"message\":{\"role\":\"assistant\","
                                    "\"content\":[{\"type\":\"text\",\"text\":\"%s\"}]}}\n")
                                   id (directory-file-name root) id text)))))
            (write-session old "old-1" "Older last")
            (write-session new "new-1" "Newer last")
            (set-file-times old (encode-time 0 0 0 1 1 2020 t))
            (set-file-times new (encode-time 0 0 0 1 1 2021 t))
            (let ((candidates (magpi-backend-chat-candidates
                               (make-magpi-pimacs-backend) root)))
              (should (equal candidates
                             (list (list :reference "pimacs:new-1"
                                         :label "Newer last")
                                   (list :reference "pimacs:old-1"
                                         :label "Older last")))))))
      (when (file-directory-p store) (delete-directory store t))
      (when (file-directory-p root) (delete-directory root t)))))

(ert-deftest magpi-pimacs-chat-candidates-live-wins-over-disk ()
  (let* ((store (make-temp-file "magpi-sessions" t))
         (root (file-name-as-directory (make-temp-file "magpi-root" t)))
         (project (expand-file-name (magpi-pimacs--encode-cwd root) store))
         (id "live-1")
         (file (expand-file-name (format "2026-01-01T00-00-00-000Z_%s.jsonl" id)
                                 project))
         (process-environment
          (cons (format "PI_CODING_AGENT_SESSION_DIR=%s" store)
                process-environment))
         (live (get-buffer-create " *magpi-live-cand*")))
    (unwind-protect
        (progn
          (make-directory project t)
          (with-temp-file file
            (insert (format
                     "{\"type\":\"session\",\"id\":\"%s\",\"cwd\":\"%s\"}\n"
                     id (directory-file-name root)))
            (insert "{\"type\":\"session_info\",\"name\":\"Disk title\"}\n"))
          (with-current-buffer live
            (setq-local default-directory root)
            (setq-local pimacs--header-line-state
                        (list :sessionName "Live title"
                              :sessionStats (list :sessionId id)))
            (let ((major-mode 'pimacs-chat-mode)
                  (buffer-list-fn (symbol-function 'buffer-list)))
              (cl-letf (((symbol-function 'derived-mode-p)
                         (lambda (&rest _) (eq major-mode 'pimacs-chat-mode)))
                        ((symbol-function 'buffer-list)
                         (lambda () (cons live (funcall buffer-list-fn)))))
                (let ((candidates (magpi-backend-chat-candidates
                                   (make-magpi-pimacs-backend) root)))
                  (should (equal (car candidates)
                                 (list :reference "pimacs:live-1"
                                       :label "Live title")))
                  (should (= (length candidates) 1))))))
      (when (buffer-live-p live) (kill-buffer live))
      (when (file-directory-p store) (delete-directory store t))
      (when (file-directory-p root) (delete-directory root t))))))
(ert-deftest magpi-pimacs-backend-translates-approval-to-ask-without-invented-proof ()
  (let ((ask (magpi-pimacs--normalize-ask
              '(:id "q1" :question "Apply?" :affected-paths ("lib/auth.ex")))))
    (should (equal (plist-get ask :id) "q1"))
    (should (equal (plist-get ask :question) "Apply?"))
    (should (equal (plist-get ask :affected-paths) '("lib/auth.ex")))
    (should-not (plist-member ask :proposed-effect))
    (should-not (plist-member ask :consequence)))
  (should (equal (plist-get (car (magpi-pimacs--normalize-events
                                  '(:type "approval_requested"
                                    :id "q1" :question "Apply?")))
                            :type)
                 'ask-requested)))

(ert-deftest magpi-pimacs-backend-cannot-answer-structured-asks ()
  "Promise: Magpi does not invent a Pi approval RPC the adapter does not have."
  (let ((backend (make-magpi-pimacs-backend)))
    (should-not (magpi-backend-ask-supported-p backend))
    (should-error (magpi-backend-respond-ask backend 'handle "q1" 'approved)
                  :type 'user-error)))

(provide 'magpi-pimacs-backend-tests)
;;; magpi-pimacs-backend-tests.el ends here
