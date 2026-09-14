;;; magpi-pimacs-backend.el --- Pimacs adapter for Magpi -*- lexical-binding: t; -*-

;; Copyright (C) 2026 ks0m1c_dharma
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is part of Magpi.

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
  id chat-buffer cleanup initial-title listener initial-sent key root
  awaiting-entries history-timer)
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

(defun magpi-pimacs--flag-value (option)
  "Return the value following OPTION in `pimacs-flags', or nil."
  (let ((flags (and (boundp 'pimacs-flags) pimacs-flags))
        value)
    (while flags
      (if (equal (car flags) option)
          (setq value (cadr flags)
                flags nil)
        (setq flags (cdr flags))))
    value))

(defun magpi-pimacs--session-store-root ()
  "Return the Pi session store without starting an agent.

Order: `--session-dir' from `pimacs-flags', then
`PI_CODING_AGENT_SESSION_DIR', then Pi's default."
  (or (magpi-pimacs--flag-value "--session-dir")
      (let ((env (getenv "PI_CODING_AGENT_SESSION_DIR")))
        (and (stringp env) (not (string-empty-p env)) env))
      (expand-file-name "~/.pi/agent/sessions")))

(defun magpi-pimacs--encode-cwd (root)
  "Encode ROOT the way Pi names project session folders."
  (let ((path (replace-regexp-in-string
               "\\`/+" ""
               (directory-file-name (expand-file-name root)))))
    (concat "--"
            (replace-regexp-in-string "/" "-" path t t)
            "--")))

(defun magpi-pimacs--project-session-dir (root)
  (expand-file-name (magpi-pimacs--encode-cwd root)
                    (magpi-pimacs--session-store-root)))

(defun magpi-pimacs--generated-session-name-p (name)
  "Return non-nil when NAME is a transport placeholder, not a real title."
  (and (stringp name)
       (or (member name '("New chat" "New task" "◯"))
           (string-match-p "\\`\\(New chat\\|New task\\|◯\\) · " name))))

(defun magpi-pimacs--session-label (name first-user &optional last-user)
  "Choose a scannable label from NAME, then FIRST-USER, then LAST-USER.

Generated transport names yield.  Last assistant never becomes the title."
  (let* ((name (and (stringp name) (not (string-empty-p (string-trim name)))
                    (string-trim name)))
         (first (magpi--one-line first-user 120))
         (last (magpi--one-line last-user 120)))
    (or (and name (not (magpi-pimacs--generated-session-name-p name)) name)
        first
        last)))

(defun magpi-pimacs--message-text (message)
  "Return MESSAGE's text collapsed to one line, or nil when blank."
  (when (listp message)
    (magpi--one-line (magpi-pimacs--content-text
                      (plist-get message :content))
                     200)))

(defconst magpi-pimacs--session-peek-bytes 65536
  "Bytes peeked from a session JSONL head and tail.  Not a transcript load.")

(defun magpi-pimacs--insert-session-window (filename start)
  "Insert FILENAME from START to EOF.  Drop a partial first line when START>0."
  (let ((size (file-attribute-size (file-attributes filename))))
    (insert-file-contents filename nil start size)
    (when (> start 0)
      (goto-char (point-min))
      (forward-line 1)
      (delete-region (point-min) (point)))))

(defun magpi-pimacs--scan-session-jsonl (on-json)
  "Call ON-JSON with each JSON object in the current buffer."
  (goto-char (point-min))
  (while (not (eobp))
    (let ((line (buffer-substring-no-properties
                 (line-beginning-position) (line-end-position))))
      (unless (string-empty-p line)
        (condition-case nil
            (funcall on-json
                     (json-parse-string line :object-type 'plist
                                        :array-type 'list))
          (error nil))))
    (forward-line 1)))

(defun magpi-pimacs--absorb-session-json (acc json)
  "Update ACC with identity, last user, and last assistant from one JSONL object."
  (pcase (plist-get json :type)
    ("session"
     (plist-put acc :id (plist-get json :id))
     (plist-put acc :cwd (plist-get json :cwd)))
    ("session_info"
     (unless (plist-get acc :name)
       (plist-put acc :name (plist-get json :name))))
    ("message"
     (let* ((message (plist-get json :message))
            (role (and (listp message) (plist-get message :role)))
            (text (magpi-pimacs--message-text message)))
       (when text
         (cond
          ((equal role "user")
           (plist-put acc :last-user text)
           (unless (plist-get acc :first-user)
             (plist-put acc :first-user text)))
          ((equal role "assistant")
           (plist-put acc :last-activity text)))))))
  acc)

(defun magpi-pimacs--read-session-file (filename)
  "Parse a primary Pi session JSONL into identity and a last-activity peek.

Does not call Pi.  Tail peek is last assistant text.  Magpi never
loads the transcript here.  Malformed files are skipped."
  (when (and (stringp filename) (file-readable-p filename))
    (condition-case nil
        (let* ((size (file-attribute-size (file-attributes filename)))
               (peek magpi-pimacs--session-peek-bytes)
               (basename (file-name-base filename))
               (acc (list :id nil :cwd nil :name nil
                          :first-user nil :last-user nil :last-activity nil)))
          (with-temp-buffer
            (insert-file-contents filename nil 0 (min size peek))
            (magpi-pimacs--scan-session-jsonl
             (lambda (json) (magpi-pimacs--absorb-session-json acc json))))
          (when (> size peek)
            (with-temp-buffer
              (magpi-pimacs--insert-session-window filename (- size peek))
              (magpi-pimacs--scan-session-jsonl
               (lambda (json) (magpi-pimacs--absorb-session-json acc json)))))
          (let ((id (plist-get acc :id)))
            (when (and (stringp id) (not (string-empty-p id))
                       (or (equal id basename)
                           (string-suffix-p (concat "_" id) basename)))
              acc)))
      (error nil))))

(defun magpi-pimacs--disk-chat-candidates (root)
  "Return historical session candidates for ROOT from the on-disk store.

Recent last-activity (file mtime) first.  Labels peek the user task; last
assistant is `:last'.  Pi is not started."
  (let* ((root (file-truename (file-name-as-directory root)))
         (dir (magpi-pimacs--project-session-dir root))
         candidates)
    (when (file-directory-p dir)
      (dolist (file (directory-files dir t "\\.jsonl\\'"))
        (when-let ((session (magpi-pimacs--read-session-file file)))
          (let* ((cwd (plist-get session :cwd))
                 (cwd (and (stringp cwd)
                           (file-truename (file-name-as-directory cwd)))))
            (when (or (null cwd) (equal cwd root))
              (when-let ((label (magpi-pimacs--session-label
                                 (plist-get session :name)
                                 (plist-get session :first-user)
                                 (or (plist-get session :last-user)
                                     (plist-get session :last-activity)))))
                (let ((last (plist-get session :last-activity)))
                  (push (cons (or (file-attribute-modification-time
                                   (file-attributes file))
                                  '(0 0))
                              (nconc
                               (list :reference (concat "pimacs:" (plist-get session :id))
                                     :label label)
                               (when (and (stringp last)
                                          (not (string-empty-p last))
                                          (not (equal last label)))
                                 (list :last last))))
                        candidates))))))))
    (mapcar #'cdr
            (sort candidates
                  (lambda (a b)
                    (time-less-p (car b) (car a)))))))

(defun magpi-pimacs--live-chat-candidates (root)
  "Return live Pimacs chat buffers at ROOT as session references."
  (let ((root (file-truename (file-name-as-directory root))))
    (delq
     nil
     (mapcar
      (lambda (buffer)
        (with-current-buffer buffer
          (when (and (derived-mode-p 'pimacs-chat-mode)
                     (ignore-errors
                       (equal (file-truename
                               (file-name-as-directory default-directory))
                              root)))
            (let* ((state (and (boundp 'pimacs--header-line-state)
                               pimacs--header-line-state))
                   (session (and state (plist-get state :sessionStats)))
                   (id (or (and session (plist-get session :sessionId))
                           (buffer-name buffer)))
                   (name (and state (plist-get state :sessionName)))
                   (label (or (magpi-pimacs--session-label name nil)
                              (buffer-name buffer))))
              (list :reference (concat "pimacs:" id)
                    :label label)))))
      (buffer-list)))))

(defun magpi-pimacs--dedupe-chat-candidates (candidates)
  "Deduplicate CANDIDATES by :reference; earlier entries win."
  (let ((seen (make-hash-table :test #'equal))
        out)
    (dolist (candidate candidates)
      (let ((ref (plist-get candidate :reference)))
        (unless (or (null ref) (gethash ref seen))
          (puthash ref t seen)
          (push candidate out))))
    (nreverse out)))

(cl-defmethod magpi-backend-chat-candidates ((_backend magpi-pimacs-backend) root)
  "Return live and historical chats for ROOT as (:reference :label).

Live wins on the same reference.  Disk sessions are read cold; Pi is not started."
  (magpi-pimacs--dedupe-chat-candidates
   (append (magpi-pimacs--live-chat-candidates root)
           (magpi-pimacs--disk-chat-candidates root))))

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
  "Compile ACTION's task prompt and frozen launch-local source.

Authority, model, intention identity, and `@' bindings are not prompt text.
A nil task prompt still delivers captured point/region source."
  (magpi-launch-compose-first-message
   (magpi-action-prompt action)
   (when-let ((launch (magpi-action-launch action)))
     (magpi-launch-spec-context launch))))

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

(defun magpi-pimacs--number (value)
  "Return VALUE when it is a number.  json-null and strings stay silent."
  (and (numberp value) value))

(defun magpi-pimacs--usage-pair (key value)
  (when-let ((n (magpi-pimacs--number value)))
    (list key n)))

(defun magpi-pimacs--normalize-usage (data)
  "Return Magpi usage plist from Pimacs session-stats DATA, or nil."
  (when (listp data)
    (let* ((tokens (plist-get data :tokens))
           (context (plist-get data :contextUsage))
           (usage
            (append
             (and (listp tokens)
                  (append
                   (magpi-pimacs--usage-pair :input (plist-get tokens :input))
                   (magpi-pimacs--usage-pair :output (plist-get tokens :output))
                   (magpi-pimacs--usage-pair :total (plist-get tokens :total))))
             (magpi-pimacs--usage-pair :cost (plist-get data :cost))
             (and (listp context)
                  (append
                   (magpi-pimacs--usage-pair :context (plist-get context :tokens))
                   (magpi-pimacs--usage-pair :window
                                            (plist-get context :contextWindow)))))))
      (when usage usage))))

(defun magpi-pimacs--request-usage (listener)
  "Reconcile Pimacs session stats into a usage observation."
  (pimacs--send-command
   "get_session_stats" '()
   (lambda (response)
     (when (pimacs--response-success-p response)
       (when-let ((usage (magpi-pimacs--normalize-usage
                          (plist-get response :data))))
         (funcall listener (list :type 'usage-observed :usage usage)))))))

(defun magpi-pimacs--without-parent-session (env)
  "Return ENV without a parent Pi session identity.
A Magpi-spawned agent must not inherit the operator's Pi session."
  (seq-remove
   (lambda (entry)
     (string-match-p
      "\\`PI_\\(SESSION_ID\\|SESSION_FILE\\|SUBAGENT_PARENT_SESSION\\|CODING_AGENT\\)="
      entry))
   env))

(defun magpi-pimacs--visible-buffer-name (name)
  "Return NAME without a leading-space hide, or NAME."
  (if (and (stringp name) (string-prefix-p " " name))
      (substring name 1)
    name))

(defun magpi-pimacs--as-project-buffer (buffer)
  "Make BUFFER a visible project buffer.  Kill still terminates the agent."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (let ((visible (magpi-pimacs--visible-buffer-name (buffer-name))))
        (unless (string= visible (buffer-name))
          (rename-buffer visible t)))
      (setq-local doom-real-buffer-p t)))
  buffer)

(defun magpi-pimacs--ensure-chat (name root)
  "Return Pimacs chat NAME at ROOT without displaying it.

`pimacs-chat' visits via `pop-to-buffer'.  Spawn is birth, not visit."
  (let (chat)
    (cl-letf (((symbol-function 'pop-to-buffer)
               (lambda (buffer &rest _)
                 (setq chat buffer)
                 buffer)))
      (pimacs-chat name root))
    (magpi-pimacs--as-project-buffer (or chat (current-buffer)))))

(cl-defmethod magpi-backend-spawn ((_backend magpi-pimacs-backend) action listener)
  (let* ((spec (magpi-action-launch action))
         (root (magpi-launch-spec-root spec))
         (session-name (magpi-pimacs--session-name action))
         (process-environment (magpi-pimacs--without-parent-session process-environment))
         (pimacs-flags (append (magpi-pimacs--without-option pimacs-flags "--session-id")
                               (list "--session-id" (magpi-action-id action))
                               (magpi-pimacs--flags spec)))
         (chat (magpi-pimacs--ensure-chat session-name root))
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
    handle))

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

(defun magpi-pimacs--history-rendered-p (&optional chat)
  "Return non-nil when CHAT already shows session messages.

Session name chrome (`info') is not history.  Missing Pimacs section
machinery means the transcript is not shown."
  (let ((chat (or chat (current-buffer))))
    (when (buffer-live-p chat)
      (with-current-buffer chat
        (when (and (fboundp 'pimacs-section-children)
                   (fboundp 'pimacs-section-type)
                   (boundp 'pimacs-section--root-section)
                   pimacs-section--root-section)
          (seq-some
           (lambda (section)
             (memq (pimacs-section-type section)
                   '(user assistant tool compact custom model thinking-level)))
           (pimacs-section-children pimacs-section--root-section)))))))

(defun magpi-pimacs--chat-history-pending (chat)
  "Return pending history count, t if unknown, or nil."
  (when (buffer-live-p chat)
    (with-current-buffer chat
      (cond
       ((and (boundp 'pimacs--history-render-pending)
             pimacs--history-render-pending)
        (if (fboundp 'pimacs--history-pending-entry-count)
            (pimacs--history-pending-entry-count)
          (apply #'+ (mapcar #'length pimacs--history-render-pending))))
       ((and (boundp 'pimacs--history-loading-section)
             pimacs--history-loading-section)
        t)))))

(defun magpi-pimacs--history-busy-p (handle)
  (or (magpi-pimacs-handle-awaiting-entries handle)
      (magpi-pimacs--chat-history-pending
       (magpi-pimacs-handle-chat-buffer handle))))

(defun magpi-pimacs--paint-history (handle)
  (when-let ((listener (magpi-pimacs-handle-listener handle)))
    (funcall listener '(:type history-pending))))

(defun magpi-pimacs--history-watch-stop (handle)
  (when-let ((timer (and (magpi-pimacs-handle-p handle)
                         (magpi-pimacs-handle-history-timer handle))))
    (when (timerp timer) (cancel-timer timer)))
  (when (magpi-pimacs-handle-p handle)
    (setf (magpi-pimacs-handle-history-timer handle) nil)))

(defun magpi-pimacs--history-watch-tick (handle)
  (if (and (magpi-pimacs-handle-p handle)
           (buffer-live-p (magpi-pimacs-handle-chat-buffer handle))
           (magpi-pimacs--history-busy-p handle))
      (magpi-pimacs--paint-history handle)
    (magpi-pimacs--history-watch-stop handle)
    (magpi-pimacs--paint-history handle)))

(defun magpi-pimacs--history-watch-start (handle)
  (magpi-pimacs--history-watch-stop handle)
  (magpi-pimacs--paint-history handle)
  (when (magpi-pimacs--history-busy-p handle)
    (setf (magpi-pimacs-handle-history-timer handle)
          (run-with-idle-timer 0.35 t #'magpi-pimacs--history-watch-tick handle))))

(defun magpi-pimacs--history-arrived (handle)
  (when (magpi-pimacs-handle-p handle)
    (setf (magpi-pimacs-handle-awaiting-entries handle) nil)
    (magpi-pimacs--history-watch-start handle)))

(defun magpi-pimacs--history-pending-label (handle)
  "Return a glance loading mark while HANDLE's chat is filling, or nil."
  (cond
   ((not (magpi-pimacs-handle-p handle)) nil)
   ((magpi-pimacs-handle-awaiting-entries handle) "loading")
   (t
    (let ((pending (magpi-pimacs--chat-history-pending
                    (magpi-pimacs-handle-chat-buffer handle))))
      (cond
       ((and (numberp pending) (> pending 0))
        (format "loading %d" pending))
       (pending "loading"))))))

(cl-defmethod magpi-backend-history-pending ((_backend magpi-pimacs-backend) handle)
  (magpi-pimacs--history-pending-label handle))

(defun magpi-pimacs--hydrate-history (chat handle)
  "Ask Pimacs to snapshot session entries into CHAT when the UI is empty.

`get_entries' is not a model turn: no tokens.  Pimacs paints last activity
first, then lazily fills earlier history.  Magpi never parses JSONL.
Glance may show loading until the porcelain is still."
  (when (and (magpi-pimacs-handle-p handle)
             (buffer-live-p chat)
             (fboundp 'pimacs-refresh-session)
             (not (magpi-pimacs--history-rendered-p chat)))
    (with-current-buffer chat
      (when (ignore-errors (pimacs--current-agent))
        (setf (magpi-pimacs-handle-awaiting-entries handle) t)
        (magpi-pimacs--history-watch-start handle)
        (condition-case nil
            (pimacs-refresh-session
             (lambda () (magpi-pimacs--history-arrived handle)))
          (error (magpi-pimacs--history-arrived handle)))))))
(cl-defmethod magpi-backend-visit ((_backend magpi-pimacs-backend) handle)
  (let ((chat (magpi-pimacs-handle-chat-buffer handle)))
    (unless (buffer-live-p chat)
      (let ((name (magpi-pimacs-handle-initial-title handle))
            (root (magpi-pimacs-handle-root handle)))
        (unless (and name root)
          (user-error "Attempt %s has no live chat buffer"
                      (magpi-pimacs-handle-id handle)))
        ;; Agent may still be live.  Ensure rebuilds the UI; flags do not apply.
        (setq chat (magpi-pimacs--ensure-chat name root))
        (setf (magpi-pimacs-handle-chat-buffer handle) chat
              (magpi-pimacs-handle-key handle)
              (buffer-local-value 'pimacs--project-key chat))
        (when-let ((listener (magpi-pimacs-handle-listener handle)))
          (with-current-buffer chat
            (magpi-pimacs--listen handle listener)))))
    (magpi-pimacs--as-project-buffer chat)
    (magpi-pimacs--hydrate-history chat handle)
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
  (magpi-pimacs--history-watch-stop handle)
  (let ((chat (magpi-pimacs-handle-chat-buffer handle)))
    (when (buffer-live-p chat)
      (with-current-buffer chat
        (pimacs--kill-agent (magpi-pimacs-handle-cleanup handle))))))

(cl-defmethod magpi-backend-reconcile ((_backend magpi-pimacs-backend) handle listener)
  (let ((chat (magpi-pimacs-handle-chat-buffer handle))
        (listener (or listener (magpi-pimacs-handle-listener handle))))
    (when (and listener (buffer-live-p chat))
      (with-current-buffer chat
        (magpi-pimacs--request-state handle listener)
        (magpi-pimacs--request-usage listener)))))

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
