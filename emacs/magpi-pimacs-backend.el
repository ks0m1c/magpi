;;; magpi-pimacs-backend.el --- Pimacs adapter for Magpi -*- lexical-binding: t; -*-

;; One job: compile Magpi semantics to Pimacs and normalize Pimacs facts
;; back.  This is the only Magpi module allowed to use Pimacs private APIs.

(require 'cl-lib)
(require 'json)
(require 'seq)
(require 'pimacs)
(require 'magpi-backend)
(require 'magpi-action)
(require 'magpi-launch)

(cl-defstruct magpi-pimacs-backend)

(cl-defstruct magpi-pimacs-handle
  id chat-buffer cleanup initial-title listener initial-sent key root)
(defcustom magpi-pimacs-models-store-file
  (expand-file-name "~/.pi/agent/models-store.json")
  "Pi on-disk model catalog used when no live session can answer.

Cold launch reads this file so the spawn Model field is not stuck on
Inherit before the first agent exists.  A live `get_available_models'
reply still wins and refreshes the cache."
  :type 'file
  :group 'magpi)

(defvar magpi-backend (make-magpi-pimacs-backend)
  "The active Magpi adapter.

Later adapters replace this value while retaining the `magpi-backend-*'
contract verbs.")

(defun magpi-pimacs--session-name (action)
  "Return a Pimacs session name unique to ACTION's id.

Pimacs keys agents by md5(root + name) and starts a process only when that
key has no agent.  A shared name reuses a process and drops `--session-id'."
  (let* ((launch (magpi-action-launch action))
         (context (and launch (magpi-launch-spec-context launch)))
         (title (or (magpi-action-title action)
                    (magpi-action-prompt action)
                    (magpi-launch-context-title context)
                    "New task")))
    (format "%s · %s"
            (truncate-string-to-width title 42 nil nil "…")
            (magpi-action-id action))))

(defun magpi-pimacs--without-option (flags option)
  "Return FLAGS without OPTION and its following value."
  (let (out)
    (while flags
      (if (equal (car flags) option)
          (setq flags (cdr (cdr flags)))
        (push (pop flags) out)))
    (nreverse out)))

(defun magpi-pimacs--listen (handle listener)
  (pimacs--set-event-listener
   t (magpi-pimacs-handle-id handle)
   (lambda (event)
     (dolist (normalized (magpi-pimacs--normalize-events event))
       (funcall listener normalized)))))

(defun magpi-pimacs--model-identifier (model)
  "Return MODEL's adapter-neutral provider/model identifier, or nil."
  (when (listp model)
    (let ((provider (plist-get model :provider))
          (id (or (plist-get model :id) (plist-get model :modelId))))
      (when (and (stringp provider) (stringp id)
                 (not (string-empty-p provider))
                 (not (string-empty-p id)))
        (format "%s/%s" provider id)))))

(defun magpi-pimacs--normalize-models (data)
  "Return canonical model ids from a get_available_models DATA payload."
  (when (listp data)
    (let ((models (plist-get data :models)))
      (when (vectorp models)
        (setq models (append models nil)))
      (when (listp models)
        (delq nil (mapcar #'magpi-pimacs--model-identifier models))))))

(defun magpi-pimacs--read-models-store (file)
  "Parse FILE as an alist-keyed models store, or signal."
  (if (fboundp 'json-parse-file)
      (json-parse-file file :object-type 'alist :array-type 'list)
    (let ((json-object-type 'alist)
          (json-array-type 'list)
          (json-key-type 'string))
      (json-read-file file))))

(defun magpi-pimacs--alist-get (key alist)
  "Return KEY from ALIST whether KEY was parsed as a string or symbol."
  (or (alist-get key alist nil nil #'equal)
      (and (stringp key) (alist-get (intern key) alist))))

(defun magpi-pimacs--disk-models (&optional file)
  "Return canonical provider/model ids from Pi's on-disk models store.

FILE defaults to `magpi-pimacs-models-store-file'.  Returns nil when the
file is missing, unreadable, or malformed.  This is a cold launch
catalog only; it does not start an agent."
  (let ((file (expand-file-name (or file magpi-pimacs-models-store-file))))
    (when (file-readable-p file)
      (condition-case nil
          (let* ((data (magpi-pimacs--read-models-store file))
                 models)
            (dolist (provider-entry data)
              (let* ((provider (car provider-entry))
                     (provider (if (symbolp provider)
                                   (symbol-name provider)
                                 provider))
                     (meta (cdr provider-entry))
                     (entries (magpi-pimacs--alist-get "models" meta)))
                (when (and (stringp provider) (listp entries))
                  (dolist (model entries)
                    (let ((id (magpi-pimacs--alist-get "id" model)))
                      (when (and (stringp id) (not (string-empty-p id)))
                        (push (format "%s/%s" provider id) models)))))))
            (nreverse (seq-uniq models)))
        (error nil)))))

(defun magpi-pimacs--live-chat-at-root (root)
  "Return a live Pimacs chat buffer at ROOT, or nil."
  (when root
    (let ((root (file-truename (file-name-as-directory root))))
      (seq-find
       (lambda (buffer)
         (with-current-buffer buffer
           (and (derived-mode-p 'pimacs-chat-mode)
                (ignore-errors
                  (equal (file-truename
                          (file-name-as-directory default-directory))
                         root)))))
       (buffer-list)))))

(cl-defmethod magpi-backend-chat-candidates ((_backend magpi-pimacs-backend) root)
  "Return active Pimacs chats at ROOT as durable session references."
  (let ((root (file-truename (file-name-as-directory root))))
    (delq
     nil
     (mapcar
      (lambda (buffer)
        (with-current-buffer buffer
          (when (and (derived-mode-p 'pimacs-chat-mode)
                     (ignore-errors
                       (equal (file-truename
                               (file-name-as-directory default-directory)) root)))
            (let* ((state pimacs--header-line-state)
                   (session (plist-get state :sessionStats))
                   (id (plist-get session :sessionId))
                   (title (or (plist-get state :sessionName) (buffer-name buffer))))
              (list :reference (concat "pimacs:" (or id (buffer-name buffer)))
                    :label title)))))
      (buffer-list)))))
(defun magpi-pimacs--catalog-chat (handle root)
  "Return the chat buffer to query for a model catalog."
  (or (and handle (magpi-pimacs-handle-p handle)
           (magpi-pimacs-handle-chat-buffer handle))
      (magpi-pimacs--live-chat-at-root root)))

(defun magpi-pimacs-fill-catalog (root &optional handle)
  "Fill the launch catalog from a live session or the on-disk store.

Never waits.  A live RPC stores the catalog when the reply arrives.
A disk read stores it immediately.  Returns the cached models, if any."
  (let ((chat (magpi-pimacs--catalog-chat handle root))
        issued)
    (when (buffer-live-p chat)
      (with-current-buffer chat
        (when (ignore-errors (pimacs--current-agent))
          (condition-case nil
              (progn
                (pimacs--send-command
                 "get_available_models" '()
                 (lambda (response)
                   (when (pimacs--response-success-p response)
                     (when-let ((models (magpi-pimacs--normalize-models
                                         (plist-get response :data))))
                       (magpi-launch-store-catalog root models)))))
                (setq issued t))
            (error nil)))))
    (unless issued
      (when-let ((models (magpi-pimacs--disk-models)))
        (magpi-launch-store-catalog root models)))
    (magpi-launch-cached-models root)))

(setq magpi-launch-catalog-refresh-function #'magpi-pimacs-fill-catalog)

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
     (pcase (magpi-launch-spec-role spec)
       ('reader '("--tools" "read,grep,find,ls"))
       ('writer nil)
       (_ (user-error "Unknown Magpi role: %S"
                      (magpi-launch-spec-role spec)))))))

(defun magpi-pimacs--initial-prompt (action)
  "Compile ATTEMPT's task prompt and frozen source context.

Authority, model, and intention identity are not prompt text."
  (when-let ((prompt (magpi-action-prompt action)))
    (let ((context (magpi-launch-spec-context (magpi-action-launch action))))
      (if (eq (plist-get context :kind) 'none)
          prompt
        (concat
         prompt
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

(defun magpi-pimacs--normalize-ask (payload)
  "Translate a transport approval/ask PAYLOAD into a Magpi `:ask' plist."
  (let ((data (or (plist-get payload :approval) (plist-get payload :ask) payload)))
    (list :id (plist-get data :id)
          :parent-id (plist-get data :parent-id)
          :requester (plist-get data :requester)
          :question (or (plist-get data :question) (plist-get data :ask))
          :detail (plist-get data :detail)
          :state (plist-get data :state)
          :affected-paths (plist-get data :affected-paths))))

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
       (cond
        ((equal (plist-get message :role) "assistant")
         (list (list :type 'response-observed
                     :text (magpi-pimacs--content-text
                            (plist-get message :content)))))
        ((equal (plist-get message :role) "user")
         (list (list :type 'prompt-observed
                     :prompt (magpi-pimacs--content-text
                              (plist-get message :content))))))))
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
     (list '(:type activity-started :activity "thinking")))
    ;; Pi may say "approval"; Magpi only receives ask-* events.
    ((or "approval_requested" "ask_requested")
     (list (list :type 'ask-requested :ask (magpi-pimacs--normalize-ask event))))
    ((or "approval_updated" "ask_updated")
     (list (list :type 'ask-updated :ask (magpi-pimacs--normalize-ask event))))
    ((or "approval_resolved" "ask_resolved")
     (list (list :type 'ask-resolved :ask (magpi-pimacs--normalize-ask event))))))

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

(defun magpi-pimacs--without-parent-session (env)
  "Return ENV without a parent Pi session identity.
A Magpi-spawned agent must not inherit the operator's Pi session."
  (seq-remove
   (lambda (entry)
     (string-match-p
      "\\`PI_\\(SESSION_ID\\|SESSION_FILE\\|SUBAGENT_PARENT_SESSION\\|CODING_AGENT\\)="
      entry))
   env))

(cl-defmethod magpi-backend-spawn ((_backend magpi-pimacs-backend) action listener)
  (let* ((spec (magpi-action-launch action))
         (root (magpi-launch-spec-root spec))
         (session-name (magpi-pimacs--session-name action))
         (process-environment (magpi-pimacs--without-parent-session process-environment))
         (pimacs-flags (append (magpi-pimacs--without-option pimacs-flags "--session-id")
                               (list "--session-id" (magpi-action-id action))
                               (magpi-pimacs--flags spec))))
    (pimacs-chat session-name root)
    (let* ((chat (current-buffer))
           (handle (make-magpi-pimacs-handle
                    :id (magpi-action-id action)
                    :chat-buffer chat
                    :initial-title session-name
                    :listener listener
                    :root root
                    :key (buffer-local-value 'pimacs--project-key chat))))
      (with-current-buffer chat
        (magpi-pimacs--listen handle listener)
        (when-let ((agent (pimacs--current-agent)))
          (let ((cleanup (lambda ()
                           (funcall listener '(:type disconnected)))))
            (setf (magpi-pimacs-handle-cleanup handle) cleanup)
            (pimacs--agent-add-cleanup agent cleanup))))
      handle)))

(cl-defmethod magpi-backend-send-initial ((_backend magpi-pimacs-backend) handle action)
  (unless (magpi-pimacs-handle-initial-sent handle)
    (let ((chat (magpi-pimacs-handle-chat-buffer handle)))
      (unless (buffer-live-p chat)
        (user-error "Attempt %s has no live chat buffer"
                    (magpi-pimacs-handle-id handle)))
      ;; Record the action before delivery so a transport error cannot restate
      ;; the same instruction.  Retry is a new action, never a second send.
      (setf (magpi-pimacs-handle-initial-sent handle) t)
      (with-current-buffer chat
        (when-let ((prompt (magpi-pimacs--initial-prompt action)))
          (pimacs-send-prompt prompt nil)))))
  handle)

(cl-defmethod magpi-backend-visit ((_backend magpi-pimacs-backend) handle)
  (let ((chat (magpi-pimacs-handle-chat-buffer handle)))
    (unless (buffer-live-p chat)
      (let ((name (magpi-pimacs-handle-initial-title handle))
            (root (magpi-pimacs-handle-root handle)))
        (unless (and name root)
          (user-error "Attempt %s has no live chat buffer"
                      (magpi-pimacs-handle-id handle)))
        ;; Agent may still be live.  pimacs-chat reuses it and rebuilds the UI;
        ;; flags do not apply.
        (pimacs-chat name root)
        (setq chat (current-buffer))
        (setf (magpi-pimacs-handle-chat-buffer handle) chat
              (magpi-pimacs-handle-key handle)
              (buffer-local-value 'pimacs--project-key chat))
        (when-let ((listener (magpi-pimacs-handle-listener handle)))
          (with-current-buffer chat
            (magpi-pimacs--listen handle listener)))))
    (pop-to-buffer chat)))

(cl-defmethod magpi-backend-visit-root ((_backend magpi-pimacs-backend) root)
  "Visit the live Pimacs chat associated with project ROOT."
  (if-let ((chat (magpi-pimacs--live-chat-at-root root)))
      (pop-to-buffer chat)
    (user-error "No active Pimacs chat for project %s"
               (file-name-nondirectory
                (directory-file-name (expand-file-name root))))))

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
        (magpi-pimacs--request-state handle listener)))))

(cl-defmethod magpi-backend-session-ref ((_backend magpi-pimacs-backend) handle)
  (and (magpi-pimacs-handle-p handle)
       (magpi-pimacs-handle-id handle)))

(cl-defmethod magpi-backend-live-p ((_backend magpi-pimacs-backend) handle)
  (when (magpi-pimacs-handle-p handle)
    (let* ((key (magpi-pimacs-handle-key handle))
           (agent (and key (boundp 'pimacs--agents)
                       (gethash key pimacs--agents))))
      (and agent (process-live-p agent)))))

(provide 'magpi-pimacs-backend)
;;; magpi-pimacs-backend.el ends here
