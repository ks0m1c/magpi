;;; magpi-pimacs-backend.el --- Pimacs adapter for Magpi -*- lexical-binding: t; -*-

;; This is the only Magpi module allowed to use Pimacs private APIs.  It is an
;; ephemeral Lean Cut adapter; its public surface is magpi-backend.el.

(require 'cl-lib)
(require 'pimacs)
(require 'magpi-backend)
(require 'magpi-core)
(require 'magpi-launch)

(cl-defstruct magpi-pimacs-backend)

(cl-defstruct magpi-pimacs-handle
  id chat-buffer cleanup initial-title listener initial-sent)

(defvar magpi-backend (make-magpi-pimacs-backend)
  "The Lean Cut backend.

Later backends replace this value while retaining the `magpi-backend-*'
contract.")

(defun magpi-pimacs--session-name (attempt)
  "Compile ATTEMPT into an injective Pimacs session identity."
  (format "%s · %s"
          (truncate-string-to-width (or (magpi-attempt-intent attempt) "◯")
                                    42 nil nil "…")
          (magpi-attempt-id attempt)))

(defun magpi-pimacs--model-identifier (model)
  "Return MODEL's backend-neutral provider/model identifier, or nil."
  (when (listp model)
    (let ((provider (plist-get model :provider))
          (id (or (plist-get model :id) (plist-get model :modelId))))
      (when (and (stringp provider) (stringp id)
                 (not (string-empty-p provider))
                 (not (string-empty-p id)))
        (format "%s/%s" provider id)))))

(defun magpi-pimacs--model-components (identifier)
  "Decode the canonical semantic provider/model IDENTIFIER."
  (when (and (stringp identifier)
             (string-match "\\`\\([^/]+\\)/\\(.+\\)\\'" identifier))
    (list :provider (match-string 1 identifier) :id (match-string 2 identifier))))

(defun magpi-pimacs--flags (spec)
  "Compile frozen SPEC policy into explicit Pimacs transport arguments."
  (let ((model (magpi-launch-spec-requested-model spec)))
    (append
     (when model
       (if-let ((components (magpi-pimacs--model-components model)))
           (list "--provider" (plist-get components :provider)
                 "--model" (plist-get components :id))
         (user-error "Unknown Pimacs model identifier: %s" model)))
     (when-let ((thinking (magpi-launch-spec-thinking spec)))
       (list "--thinking" (symbol-name thinking)))
     (pcase (magpi-launch-spec-authority spec)
       ('read-only '("--tools" "read,grep,find,ls"))
       ('writer nil)
       (_ (user-error "Unknown Magpi authority: %S"
                      (magpi-launch-spec-authority spec)))))))

(defun magpi-pimacs--initial-prompt (attempt)
  "Compile ATTEMPT intent and frozen source context into one initial prompt.

Authority and model are transport flags, not prompt text.  Restating them here
would give the model a second encoding of the same policy and look like a new
instruction on any accidental re-send."
  (when-let ((intent (magpi-attempt-intent attempt)))
    (let ((context (magpi-launch-spec-context (magpi-attempt-launch attempt))))
      (if (eq (plist-get context :kind) 'none)
          intent
        (concat
         intent
         (format "\n\nContext captured at dispatch:\n- %s%s%s"
                 (or (plist-get context :file) "buffer")
                 (if-let ((line (plist-get context :line)))
                     (format ":%d" line)
                   "")
                 (if-let ((text (plist-get context :text)))
                     (format "\n\n%s" text)
                   "")))))))

(defun magpi-pimacs--content-text (content)
  "Extract assistant text from Pimacs's transport CONTENT."
  (cond
   ((stringp content) content)
   ((vectorp content) (magpi-pimacs--content-text (append content nil)))
   ((listp content)
    (mapconcat
     (lambda (item)
       (if (equal (plist-get item :type) "text")
           (or (plist-get item :text) "")
         ""))
     content ""))
   (t "")))

(defun magpi-pimacs--normalize-file-path (path)
  "Return PATH as a lexical project-relative path, or nil when it escapes root."
  (when (stringp path)
    (let ((relative (file-relative-name (expand-file-name path) default-directory)))
      (unless (or (equal relative ".")
                  (string-prefix-p "../" relative)
                  (equal relative ".."))
        relative))))

(defun magpi-pimacs--normalize-events (event)
  "Translate raw Pimacs EVENT into zero or more semantic event plists."
  (pcase (plist-get event :type)
    ((or "agent_start" "turn_start")
     (list '(:type activity-started :activity "thinking")))
    ;; Pi settling says only that it is idle right now; it is not completion.
    ("agent_settled"
     (list '(:type activity-ended :idle t)))
    ("tool_execution_start"
     (let* ((tool (or (plist-get event :toolName) "tool"))
            (args (plist-get event :args))
            (path (magpi-pimacs--normalize-file-path
                   (plist-get args :path))))
       (append (list (list :type 'activity-started :activity tool))
               (when (and path (member tool '("edit" "write")))
                 (list (list :type 'file-observed :path path))))))
    ("tool_execution_end"
     (list '(:type activity-ended)))
    ("message_end"
     (let ((message (plist-get event :message)))
       (when (equal (plist-get message :role) "assistant")
         (list (list :type 'response-observed
                     :text (magpi-pimacs--content-text
                            (plist-get message :content)))))))
    ("extension_ui_request"
     (when (equal (plist-get event :method) "setTitle")
       (list (list :type 'title-observed :title (plist-get event :title)))))
    ((or "model_change" "model_changed")
     (let* ((model (or (plist-get event :model) event))
            (identifier (magpi-pimacs--model-identifier model)))
       (when identifier
         (list (list :type 'model-observed :model identifier)))))
    ("extension_error"
     (list '(:type problem-observed :problem "extension error")))
    ("auto_retry_start"
     (list '(:type activity-started :activity "retrying")))
    ("auto_retry_end"
     (list '(:type activity-started :activity "thinking")))))

(defun magpi-pimacs--json-number (value)
  "Return VALUE when it is a finite number; treat json-null as absent."
  (and (numberp value) value))

(defun magpi-pimacs--normalize-usage (data)
  "Translate get_session_stats DATA into a backend-neutral usage plist.

Returns nil when DATA carries no usable token, cost, or context facts."
  (when (listp data)
    (let* ((tokens (plist-get data :tokens))
           (context (plist-get data :contextUsage))
           (tokens (and (listp tokens) tokens))
           (context (and (listp context) context))
           (usage
            (list :input (magpi-pimacs--json-number
                          (and tokens (plist-get tokens :input)))
                  :output (magpi-pimacs--json-number
                           (and tokens (plist-get tokens :output)))
                  :cache-read (magpi-pimacs--json-number
                               (and tokens (plist-get tokens :cacheRead)))
                  :cache-write (magpi-pimacs--json-number
                                (and tokens (plist-get tokens :cacheWrite)))
                  :total (magpi-pimacs--json-number
                          (and tokens (plist-get tokens :total)))
                  :cost (magpi-pimacs--json-number (plist-get data :cost))
                  :context-tokens (magpi-pimacs--json-number
                                   (and context (plist-get context :tokens)))
                  :context-window (magpi-pimacs--json-number
                                   (and context
                                        (plist-get context :contextWindow)))
                  :context-percent (magpi-pimacs--json-number
                                    (and context
                                         (plist-get context :percent))))))
      (when (cl-some #'numberp
                     (list (plist-get usage :input)
                           (plist-get usage :output)
                           (plist-get usage :cache-read)
                           (plist-get usage :cache-write)
                           (plist-get usage :total)
                           (plist-get usage :cost)
                           (plist-get usage :context-tokens)
                           (plist-get usage :context-window)
                           (plist-get usage :context-percent)))
        usage))))

(defun magpi-pimacs--request-usage (listener)
  "Reconcile session token usage into a usage-observed event."
  (pimacs--send-command
   "get_session_stats" '()
   (lambda (response)
     (when (pimacs--response-success-p response)
       (when-let ((usage (magpi-pimacs--normalize-usage
                          (plist-get response :data))))
         (funcall listener (list :type 'usage-observed :usage usage)))))))

(defun magpi-pimacs--request-state (handle listener)
  "Reconcile Pimacs state into title and running-model observations."
  (pimacs--send-command
   "get_state" '()
   (lambda (response)
     (when (pimacs--response-success-p response)
       (let ((data (plist-get response :data)))
         (when-let ((title (plist-get data :sessionName)))
           (unless (equal title (magpi-pimacs-handle-initial-title handle))
             (funcall listener (list :type 'title-observed :title title))))
         (when-let ((model (magpi-pimacs--model-identifier
                            (plist-get data :model))))
           (funcall listener (list :type 'model-observed :model model))))))))

(defun magpi-pimacs--request-observations (handle listener)
  "Snapshot transport-only facts: title, model, and session usage."
  (magpi-pimacs--request-state handle listener)
  (magpi-pimacs--request-usage listener))

(cl-defmethod magpi-backend-spawn ((_backend magpi-pimacs-backend) attempt listener)
  (let* ((spec (magpi-attempt-launch attempt))
         (session-name (magpi-pimacs--session-name attempt))
         (pimacs-flags (append pimacs-flags (magpi-pimacs--flags spec))))
    (pimacs-chat session-name (magpi-launch-spec-root spec))
    (let* ((chat (current-buffer))
           (handle (make-magpi-pimacs-handle
                    :id (magpi-attempt-id attempt)
                    :chat-buffer chat
                    :initial-title session-name
                    :listener listener)))
      (with-current-buffer chat
        (pimacs--set-event-listener
         t (magpi-pimacs-handle-id handle)
         (lambda (event)
           (dolist (normalized (magpi-pimacs--normalize-events event))
             (funcall listener normalized))
           ;; Session totals only advance after a settle; pull them then so the
           ;; status buffer stays current without requiring an explicit `g'.
           (when (equal (plist-get event :type) "agent_settled")
             (magpi-pimacs--request-usage listener))))
        (magpi-pimacs--request-observations handle listener)
        (when-let ((agent (pimacs--current-agent)))
          (let ((cleanup (lambda ()
                           (funcall listener '(:type disconnected)))))
            (setf (magpi-pimacs-handle-cleanup handle) cleanup)
            (pimacs--agent-add-cleanup agent cleanup))))
      handle)))

(cl-defmethod magpi-backend-send-initial ((_backend magpi-pimacs-backend) handle attempt)
  (unless (magpi-pimacs-handle-initial-sent handle)
    (let ((chat (magpi-pimacs-handle-chat-buffer handle)))
      (unless (buffer-live-p chat)
        (user-error "Attempt %s has no live chat buffer"
                    (magpi-pimacs-handle-id handle)))
      ;; Record the attempt before delivery so a transport error cannot restate
      ;; the same instruction.  Retry is a new attempt, never a second send.
      (setf (magpi-pimacs-handle-initial-sent handle) t)
      (with-current-buffer chat
        (when-let ((prompt (magpi-pimacs--initial-prompt attempt)))
          (pimacs-send-prompt prompt nil)))))
  handle)

(cl-defmethod magpi-backend-visit ((_backend magpi-pimacs-backend) handle)
  (let ((chat (magpi-pimacs-handle-chat-buffer handle)))
    (if (buffer-live-p chat)
        (pop-to-buffer chat)
      (user-error "Attempt %s has no live chat buffer"
                  (magpi-pimacs-handle-id handle)))))

(cl-defmethod magpi-backend-send ((_backend magpi-pimacs-backend) handle message
                                  &optional mode)
  (let ((chat (magpi-pimacs-handle-chat-buffer handle)))
    (unless (buffer-live-p chat)
      (user-error "Attempt %s has no live chat buffer"
                  (magpi-pimacs-handle-id handle)))
    (with-current-buffer chat
      (pimacs-send-prompt message mode))))

(cl-defmethod magpi-backend-terminate ((_backend magpi-pimacs-backend) handle)
  (let ((chat (magpi-pimacs-handle-chat-buffer handle)))
    (when (buffer-live-p chat)
      (with-current-buffer chat
        (pimacs--kill-agent (magpi-pimacs-handle-cleanup handle))))))

(cl-defmethod magpi-backend-reconcile ((_backend magpi-pimacs-backend) handle listener)
  (let ((chat (magpi-pimacs-handle-chat-buffer handle))
        (listener (or listener (magpi-pimacs-handle-listener handle))))
    (when (and listener (buffer-live-p chat))
      (with-current-buffer chat
        (magpi-pimacs--request-observations handle listener)))))

(provide 'magpi-pimacs-backend)
;;; magpi-pimacs-backend.el ends here
