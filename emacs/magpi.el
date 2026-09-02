;;; magpi.el --- Magpi porcelain: intention, action, bind, react -*- lexical-binding: t; -*-
;; Version: 0.1.0
;; Keywords: tools git
;; Package-Requires: ((emacs "29.1") (magit "3.0") (transient "0.4") (pimacs "0.6"))

;; One job: registry and effects for Intention and Action.
;; Bind and React are powers; glance is status; depth is Magit; adapter is Pimacs.
;; Effects ask semantic section selectors, not glance taxonomy (facet TYPE.VALUE).

;;; Commentary:

;; Magpi is a tiny intention-first Emacs porcelain for deliberate Pi work.
;; It does not replace Magit, Transient, or Pimacs: glance is Magit-mode,
;; launch is Transient, chat is Pimacs, and Git remains Git.

;;; Code:
(require 'project)
(require 'subr-x)
(require 'magpi-action)
(require 'magpi-launch)
(require 'magpi-intention)
(require 'magpi-backend)
(require 'magpi-pimacs-backend)
(require 'magpi-transient)
(require 'transient nil t)

;; Glance UI is optional until status opens or a section verb needs it.
;; Do not autoload magpi-facet*: effects use semantic selectors only.
(autoload 'magpi-status-open "magpi-status" nil nil)
(autoload 'magpi-section-intention-id "magpi-status" nil nil)
(autoload 'magpi-section-action-id "magpi-status" nil nil)
(autoload 'magpi-section-ask-id "magpi-status" nil nil)
(autoload 'magpi-section-path "magpi-status" nil nil)
(autoload 'magpi-section-root "magpi-status" nil nil)
(autoload 'magpi-section-bind-surface "magpi-status" nil nil)

(declare-function magit-status "magit")
(declare-function magit-diff-range "magit-diff")
(declare-function magit-log-range "magit-log")
(declare-function magit-commit-create "magit-commit")
(declare-function magit-refresh-buffer "magit-mode")
(declare-function magpi-react--transient nil)
(defun magpi--status-ready-p ()
  "Return non-nil when glance/status is already loaded.
Autoloads exist for open/selectors, but global verbs must not force Magit
load merely to discover root or absence of intention."
  (featurep 'magpi-status))

(defun magpi--section-intention-id (&optional section)
  (and (magpi--status-ready-p) (magpi-section-intention-id section)))

(defun magpi--section-action-id (&optional section)
  (and (magpi--status-ready-p) (magpi-section-action-id section)))

(defun magpi--section-ask-id (&optional section)
  (and (magpi--status-ready-p) (magpi-section-ask-id section)))

(defun magpi--section-path (&optional section)
  (and (magpi--status-ready-p) (magpi-section-path section)))

(defun magpi--section-root (&optional section)
  (and (magpi--status-ready-p) (magpi-section-root section)))

(defun magpi--section-bind-surface (&optional section)
  "Bind surface without forcing status/Magit load outside glance."
  (if (magpi--status-ready-p)
      (magpi-section-bind-surface section)
    'root))

(defvar magpi--actions (make-hash-table :test #'equal)
  "Latest immutable action value by action ID.")

(defvar magpi--handles (make-hash-table :test #'equal)
  "Opaque adapter handles by action ID; never part of the domain record.")

(defvar magpi--intentions (make-hash-table :test #'equal)
  "Latest persisted intention record by intention ID.")

(defvar-local magpi-intention-metadata nil
  "Intention metadata handed to Magit: id, objective, lease, audit, chat refs.")

(defvar magpi--pending-commit-metadata (make-hash-table :test #'equal)
  "Intention metadata awaiting creation of a Magit commit-message buffer.")

(defun magpi--install-commit-metadata ()
  "Transfer pending intention metadata into the current commit-message buffer."
  (when-let ((metadata (gethash (file-truename default-directory)
                                magpi--pending-commit-metadata)))
    (setq-local magpi-intention-metadata metadata)
    (remhash (file-truename default-directory) magpi--pending-commit-metadata)))

(add-hook 'git-commit-setup-hook #'magpi--install-commit-metadata)

(defvar magpi--refresh-timer nil)

(defun magpi--live-chats ()
  "Return live Pimacs chat buffers, last-seen first.

Hidden buffers are skipped.  Does not spawn, hydrate, or consult disk."
  (seq-filter
   (lambda (buffer)
     (and (buffer-live-p buffer)
          (not (string-prefix-p " " (buffer-name buffer)))
          (with-current-buffer buffer
            (derived-mode-p 'pimacs-chat-mode))))
   (buffer-list)))

(defun magpi-last-seen-action-ids ()
  "Return action ids for live chats, last-seen first.

Unseen actions are omitted; status appends them in display order."
  (let ((chat->id (make-hash-table :test #'eq))
        ids)
    (maphash
     (lambda (id handle)
       (when (and (fboundp 'magpi-pimacs-handle-p)
                  (magpi-pimacs-handle-p handle))
         (let ((chat (magpi-pimacs-handle-chat-buffer handle)))
           (when (buffer-live-p chat)
             (puthash chat id chat->id)))))
     magpi--handles)
    (dolist (chat (magpi--live-chats))
      (when-let ((id (gethash chat chat->id)))
        (push id ids)))
    (nreverse ids)))

(defvar-keymap magpi-command-map
  :doc "Global Magpi prefix: status, bind, spawn, and discard.  Extra I/R/M stay out."
  "m" #'magpi-status
  "s" #'magpi-spawn
  "i" #'magpi-intention-create
  "@" #'magpi-bind
  "k" #'magpi-discard)
(keymap-global-set "C-c m" magpi-command-map)

(defun magpi--root ()
  (magpi-launch-current-root))

(defun magpi--intentions-for-root (root)
  "Reload active intentions for ROOT; terminal records stay in the audit store."
  (let ((intentions (seq-filter #'magpi-intention-active-p
                                (magpi-intention-list root))))
    (dolist (intention intentions)
      (puthash (magpi-intention-id intention) intention magpi--intentions))
    intentions))

(defun magpi--lookup-action (id)
  "Return Action ID from RAM, loading that id from the current root if needed.

Miss is not birth.  Unreadable files stay out of the table."
  (or (gethash id magpi--actions)
      (let ((root (magpi--root)))
        (when (and root (magpi-store-common-dir root))
          (let ((loaded (magpi-action-load root id)))
            (when (magpi-action-p loaded)
              (puthash id loaded magpi--actions)
              loaded))))))

(defun magpi--action (id)
  "Return Action ID or reject a stale selection."
  (or (magpi--lookup-action id)
      (user-error "This action is no longer available")))

(defun magpi--intention (id)
  "Return intention ID.  Disk is coordinates when the store exists."
  (or (when-let ((root (magpi--root)))
        (when (magpi-store-common-dir root)
          (let ((loaded (magpi-intention-load root id)))
            (when (magpi-intention-p loaded)
              (puthash id loaded magpi--intentions)
              loaded))))
      (gethash id magpi--intentions)
      (user-error "Unknown Magpi intention: %s" id)))

(defun magpi-intention-create (intent)
  "Create or reuse a lightweight persisted intention for INTENT."
  (interactive (list (read-string "Intention: ")))
  (let* ((intent (or (magpi-normalize-objective intent)
                     (user-error "An intention needs a description")))
         (root (magpi--root))
         (existing (seq-find
                   (lambda (candidate)
                     (and (eq (magpi-intention-state candidate) 'active)
                          (equal (magpi-intention-objective candidate) intent)))
                   (magpi--intentions-for-root root))))
    (let ((result
           (if existing
               (progn
                 (puthash (magpi-intention-id existing) existing magpi--intentions)
                 (message "Using existing Magpi intention %s"
                          (magpi-intention-id existing))
                 existing)
             (let ((intention (magpi-intention-create-record intent root)))
               (puthash (magpi-intention-id intention) intention magpi--intentions)
               (message "Magpi intention %s created; press @ to add context"
                        (magpi-intention-id intention))
               intention))))
      (when (derived-mode-p 'magpi-status-mode)
        (magit-refresh-buffer)
        (goto-char (point-min))
        (when (re-search-forward (regexp-quote (magpi-intention-objective result)) nil t)
          (beginning-of-line)))
      result)))

(defun magpi--read-intention-id (&optional prompt)
  "Read one active intention ID, displaying its authored objective."
  (let ((intentions (magpi--intentions-for-root (magpi--root))))
    (unless intentions
      (user-error "Create an intention first with C-c m i"))
    (let ((choices (mapcar
                    (lambda (intention)
                      (cons (format "%s · %s"
                                    (magpi-intention-objective intention)
                                    (magpi-intention-id intention))
                            (magpi-intention-id intention)))
                    intentions)))
      (cdr (assoc (completing-read (or prompt "Intention: ") choices nil t)
                  choices)))))

(defun magpi--read-bind-tags ()
  "Read optional comma-separated bind tags."
  (let ((tags (read-string "Tags (comma-separated, optional): ")))
    (seq-filter (lambda (tag) (not (string-empty-p tag)))
                (mapcar #'string-trim (split-string tags "," t)))))

(defun magpi--chat-bind-candidates ()
  "Return chat references available in this Emacs session."
  (let (candidates)
    (maphash
     (lambda (id action)
       (push (list :reference (concat "magpi:" id)
                   :label (or (magpi-action-title action)
                              (magpi-action-prompt action) "Magpi chat"))
             candidates))
     magpi--actions)
    (append candidates
            (magpi-backend-chat-candidates magpi-backend (magpi--root)))))

(defun magpi--chat-candidate-for-action (action candidates)
  "Return the candidate plist matching ACTION, or nil.

Matches `pimacs:<action-id>' or `pimacs:<chat-ref>'.  Does not touch
Observation or auspice."
  (let* ((id (magpi-action-id action))
         (chat-ref (magpi-action-chat-ref action))
         (refs (delq nil
                     (list (and id (concat "pimacs:" id))
                           (and chat-ref (concat "pimacs:" chat-ref))))))
    (seq-find (lambda (candidate)
                (member (plist-get candidate :reference) refs))
              candidates)))

(defun magpi--chat-candidate-label-for-action (action candidates)
  "Return the historical session label for ACTION from CANDIDATES, or nil."
  (plist-get (magpi--chat-candidate-for-action action candidates) :label))

(defun magpi--read-chat-bind ()
  "Read a past-chat reference from known chats or an explicit session reference."
  (let* ((candidates (magpi--chat-bind-candidates))
         (choices (mapcar (lambda (chat)
                            (cons (format "%s · %s" (plist-get chat :label)
                                          (plist-get chat :reference))
                                  chat))
                          candidates))
         (other "Other chat or session reference…")
         (choice (completing-read "Chat: " (append (mapcar #'car choices) (list other))
                                  nil t)))
    (if (equal choice other)
        (list :reference (read-string "Chat or session reference: ")
              :label (read-string "Chat label (optional): "))
      (cdr (assoc choice choices)))))

(defun magpi--bind-kind-choices (surface)
  "Return bind kinds valid for SURFACE."
  (pcase surface
    ((or 'intention 'root) '("File" "Past chat" "Point" "Region"))
    ('action '("File" "Past chat" "Point" "Region"))
    (_ '("File" "Past chat"))))

(defun magpi--bind-point-or-region (kind)
  "Capture KIND (`point' or `region') as a bind reference plist."
  (unless (magpi-launch-source-buffer-p)
    (user-error "Bind %s needs a source file, not porcelain" kind))
  (if (eq kind 'region)
      (progn
        (unless (use-region-p)
          (user-error "No active region"))
        (list :kind 'region
              :reference (format "%s:%s-%s"
                                 (file-name-nondirectory buffer-file-name)
                                 (line-number-at-pos (region-beginning))
                                 (line-number-at-pos (region-end)))
              :label (format "%s:%s" (file-name-nondirectory buffer-file-name)
                             (line-number-at-pos (region-beginning)))
              :file buffer-file-name
              :text (buffer-substring-no-properties (region-beginning) (region-end))))
    (list :kind 'point
          :reference (format "%s:%s"
                             (file-name-nondirectory buffer-file-name)
                             (line-number-at-pos))
          :label (format "%s:%s" (file-name-nondirectory buffer-file-name)
                         (line-number-at-pos))
          :file buffer-file-name
          :text (string-trim
                 (buffer-substring-no-properties
                  (line-beginning-position) (line-end-position))))))

(defun magpi--bind-to-intention (intention kind &optional tags)
  "Persist KIND context on INTENTION.  Bound context is never a prompt."
  (pcase kind
    ('file
     (let* ((root (magpi-intention-source-root intention))
            (file (expand-file-name
                   (read-file-name "Bind file: " default-directory buffer-file-name t))))
       (unless (file-regular-p file)
         (user-error "Bind needs a readable file: %s" file))
       (magpi-intention-bind
        intention 'file
        (if (file-in-directory-p file root)
            (file-relative-name file root)
          file)
        (file-name-nondirectory file) tags)))
    ('chat
     (let ((chat (magpi--read-chat-bind)))
       (magpi-intention-bind intention 'chat
                             (plist-get chat :reference)
                             (plist-get chat :label) tags)))
    ((or 'point 'region)
     (let ((captured (magpi--bind-point-or-region kind)))
       (magpi-intention-bind intention kind
                             (plist-get captured :reference)
                             (plist-get captured :label) tags)))
    (_ (user-error "Unknown bind kind: %S" kind))))

(defun magpi--bind-ephemeral (kind)
  "Bind KIND as a look-here gesture without durable storage."
  (pcase kind
    ('file
     (find-file (read-file-name "Look at file: " default-directory buffer-file-name t))
     (message "Bound glance only — not durable context"))
    ('chat
     (let ((chat (magpi--read-chat-bind)))
       (message "Look at chat %s (not durable)" (plist-get chat :reference))))
    ((or 'point 'region)
     (let ((captured (magpi--bind-point-or-region kind)))
       (message "Look at %s (not durable)" (plist-get captured :reference))))
    (_ (user-error "Unknown bind kind: %S" kind))))

;;;###autoload
(defun magpi-bind (&optional section)
  "Bind context to the Magit section at point.

Bind is a power, not a field.  The section's lineage decides who holds it:
intention (durable why-reference), action (look-here), else choose intention.
Kinds: file, chat, point, region.  Bound context is never silently a prompt."
  (interactive)
  (let* ((surface (magpi--section-bind-surface section))
         (kind-label (completing-read
                      "Bind: " (magpi--bind-kind-choices surface) nil t))
         (kind (pcase kind-label
                 ("File" 'file) ("Past chat" 'chat)
                 ("Point" 'point) ("Region" 'region)
                 (_ (user-error "Unknown bind kind: %s" kind-label))))
         (tags (and (memq surface '(intention root))
                    (magpi--read-bind-tags))))
    (pcase surface
      ('intention
       (let* ((id (magpi--section-intention-id section))
              (intention (magpi--intention id)))
         (setq intention (magpi--bind-to-intention intention kind tags))
         (puthash id intention magpi--intentions)
         (magpi--schedule-refresh)
         (message "Bound on %s" (magpi-intention-objective intention))))
      ('action
       (magpi--bind-ephemeral kind))
      ('root
       (let* ((id (magpi--read-intention-id "Bind on intention: "))
              (intention (magpi--intention id)))
         (setq intention (magpi--bind-to-intention intention kind tags))
         (puthash id intention magpi--intentions)
         (magpi--schedule-refresh)
         (message "Bound on %s" (magpi-intention-objective intention))))
      (_
       (user-error "Nothing to bind at point; open status or stand on a surface")))))

(defun magpi-intention-merge (intention-id)
  "Merge persisted INTENTION-ID into its recorded base branch."
  (interactive
   (progn
     (magpi--intentions-for-root (magpi--root))
     (list (completing-read
            "Merge intention: "
            (let (choices)
              (maphash (lambda (id intention)
                         (push (cons (format "%s · %s"
                                             (magpi-intention-objective intention) id)
                                     id)
                               choices))
                       magpi--intentions)
              choices)
            nil t))))
  (let ((intention (magpi--intention intention-id)))
    (setq intention (magpi-intention-merge-record intention))
    (puthash intention-id intention magpi--intentions)
    (message "Merged Magpi intention %s" (magpi-intention-objective intention))
    intention))

(defun magpi--capture-bind (kind)
  "Freeze one Bind moment KIND into the launch specification.

The same power as `@': point/region are source evidence; none means do not bind.
Porcelain buffers are never source.  Spawn calls this; `@' persists or glances."
  (pcase kind
    ('none (list :kind 'none))
    ((or 'region 'point)
     (unless (magpi-launch-source-buffer-p)
       (user-error "Magpi bind %s needs a source file, not porcelain"
                   kind))
     (if (eq kind 'region)
         (progn
           (unless (use-region-p)
             (user-error "No active region"))
           (list :kind 'region
                 :file buffer-file-name
                 :line (line-number-at-pos (region-beginning))
                 :text (buffer-substring-no-properties
                        (region-beginning) (region-end))))
       (list :kind 'point
             :file buffer-file-name
             :line (line-number-at-pos)
             :text (string-trim
                    (buffer-substring-no-properties
                     (line-beginning-position) (line-end-position))))))
    (_ (user-error "Unknown Magpi bind kind: %S" kind))))

(defun magpi--handle-event (id event)
  "Reduce EVENT into the latest action for immutable ID, then refresh.

The listener captures only ID.  This is the one mutation point for the action
registry; reduction itself is functional.  Restated facts keep the same action
object and do not schedule another paint.
History fill is porcelain: it repaints glance without minting Observation."
  (when-let ((action (gethash id magpi--actions)))
    (if (eq (plist-get event :type) 'history-pending)
        (magpi--schedule-refresh)
      (let ((next (magpi-action-reduce action event)))
        (unless (eq next action)
          (puthash id next magpi--actions)
          ;; Disconnection is uncertainty, not terminal proof.  A persisted writer
          ;; lease remains held until an explicit, attributable release.
          (magpi--schedule-refresh))))))

(defun magpi--record-launch-problem (action error-data)
  "Return ACTION marked as disconnected after launch ERROR-DATA."
  (magpi-action-reduce
   (magpi-action-reduce action
                         (list :type 'problem-observed
                               :problem (error-message-string error-data)))
   '(:type disconnected)))

(defun magpi--live-handle (id)
  "Return the live adapter handle for action ID, or nil.

Liveness is the agent process, not a chat buffer."
  (when-let ((handle (gethash id magpi--handles)))
    (when (magpi-backend-live-p magpi-backend handle)
      handle)))

(defun magpi--known-process-state (id)
  "Return known adapter process state for action ID: `live', `dead', or `absent'."
  (let ((handle (gethash id magpi--handles)))
    (cond ((null handle) 'absent)
          ((magpi-backend-live-p magpi-backend handle) 'live)
          (t 'dead))))

(defun magpi--attach (id action &optional resume)
  "Spawn ACTION for ID and store the handle.  RESUME skips send-initial.

Does not rewrite ACTION except to record an attributable launch failure."
  (let (handle spawn-tried)
    (condition-case error-data
        (progn
          (setq spawn-tried t
                handle (magpi-backend-spawn
                        magpi-backend action
                        (lambda (event) (magpi--handle-event id event))))
          (puthash id handle magpi--handles)
          (unless resume
            (magpi-backend-send-initial magpi-backend handle action))
          (when-let ((launch (magpi-action-launch action)))
            (magpi-launch-refresh-catalog (magpi-launch-spec-root launch) handle)))
      (error
       (when spawn-tried
         (let ((attributable (or (gethash id magpi--actions) action)))
           (puthash id (magpi--record-launch-problem attributable error-data)
                    magpi--actions)
           (when handle
             (puthash id handle magpi--handles))
           (message "Magpi launch for %s failed: %s"
                    id (error-message-string error-data))))))
    (magpi--schedule-refresh)
    (gethash id magpi--actions)))

(defun magpi--resume-root (action)
  "Return the project root to reopen ACTION."
  (or (and (magpi-action-launch action)
           (magpi-launch-spec-root (magpi-action-launch action)))
      (when-let ((intention-id (magpi-action-intention-id action))
                 (intention (or (gethash intention-id magpi--intentions)
                                (ignore-errors (magpi--intention intention-id)))))
        (magpi-intention-worktree-path intention))
      (magpi-action-source-root action)
      (magpi--root)))

(defun magpi--action-with-launch (action)
  "Return ACTION bearing a launch.  Hydrate may lack one; defaults fill RAM only."
  (if (magpi-action-launch action)
      action
    (let* ((id (magpi-action-id action))
           (launch (magpi-launch-build
                    (or (magpi--resume-root action)
                        (user-error "This action has no repository locator"))
                    magpi-default-thinking magpi-default-role '(:kind none)))
           (next (copy-magpi-action action)))
      (setf (magpi-action-launch next) launch)
      (puthash id next magpi--actions)
      next)))

(defun magpi--ensure-process (id &optional resume)
  "Bring ID's agent up from known process state.  Action must already exist.

live visits; dead respawns without send-initial; absent attaches.
RESUME non-nil skips send-initial (reopen / retry).  Dead always resumes.
Never creates or rebakes an Action.  Miss loads the kernel, then errors."
  (let ((action (magpi--action-with-launch (magpi--action id))))
    (pcase (magpi--known-process-state id)
      ('live
       (magpi-backend-visit magpi-backend (gethash id magpi--handles))
       (gethash id magpi--actions))
      ('dead
       (remhash id magpi--handles)
       (magpi--attach id action t))
      ('absent
       (magpi--attach id action resume)))))

(defun magpi--birth (id prompt launch &optional intention-id title)
  "Create one Action: disk kernel plus launch theatre.  Save once.  No process.

Standalone freezes spawn-oid from launch root HEAD.  chat-ref and created-at
are set here; save does not invent them."
  (let* ((root (or (and launch (magpi-launch-spec-root launch))
                   (user-error "A Magpi action needs a launch root")))
         (action (make-magpi-action
                  :id id
                  :title title
                  :prompt prompt
                  :intention-id intention-id
                  :launch launch
                  :observation (magpi-observation-initial)
                  :started-at (current-time)
                  :chat-ref id
                  :created-at (magpi-store-unix-time)
                  :source-root (file-name-as-directory (expand-file-name root))
                  :spawn-oid (unless intention-id
                               (magpi-git--maybe root "rev-parse" "HEAD")))))
    (magpi-action-save action)
    (puthash id action magpi--actions)
    action))

(defun magpi-spawn-spec (id prompt launch &optional intention-id title)
  "Birth ID once when new, then ensure its process.

PROMPT is adapter/programmatic only; normal launches leave it nil so the user
authors the task in chat.  TITLE labels the chat; INTENTION-ID is membership.

An ID already in RAM or on disk is process retry only: never a second birth."
  (if (magpi--lookup-action id)
      (magpi--ensure-process id t)
    (magpi--birth id prompt launch intention-id title)
    (magpi--ensure-process id nil)))

(defun magpi-spawn-from-options (options)
  "Capture validated semantic OPTIONS and launch their immutable specification."
  (let* ((intention-id (plist-get options :intention-id))
         (requested-intent (magpi-normalize-objective (plist-get options :intent)))
         (_validated
          (when requested-intent
            (user-error "Create an intention with C-c m i, then launch its task")))
         ;; The task is authored in the chat after launch.  An intention is
         ;; selected explicitly; without one this is standalone work.
         (intention (and intention-id (magpi--intention intention-id)))
         ;; A Git change is made only when work begins, not when the user
         ;; captures the intention or its references.
         (intention (and intention (magpi-intention-ensure-worktree intention)))
         (intention-id (and intention (magpi-intention-id intention)))
         (root (if intention
                   (magpi-intention-worktree-path intention)
                 (magpi--root)))
         (context (magpi--capture-bind (or (plist-get options :bind) (plist-get options :context-kind))))
         (prompt nil)
         (thinking (if (plist-member options :thinking)
                       (plist-get options :thinking)
                     magpi-default-thinking))
         (role (or (plist-get options :role) magpi-default-role))
         (exclusive (and (eq role 'writer) (plist-get options :lease)))
         (launch (magpi-launch-build root thinking role context
                                     (plist-get options :model)))
         (id (magpi-store-new-id)))
    (puthash intention-id intention magpi--intentions)
    (magpi-launch-remember-model root (plist-get options :model))
    (magpi--birth id prompt launch intention-id
                  (and intention (magpi-intention-objective intention)))
    (when intention
      (setq intention (magpi-intention-add-action intention id role exclusive))
      (puthash intention-id intention magpi--intentions))
    (let ((action (magpi--ensure-process id nil)))
      (when (and intention (null action))
        (setq intention (magpi-intention-release-writer intention id "spawn rejected"))
        (puthash intention-id intention magpi--intentions))
      action)))

(setq magpi-launch-execute-function #'magpi-spawn-from-options)

;;;###autoload
(defun magpi-spawn (&optional intention-id)
  "Configure and spawn an action.  INTENTION-ID is Magit lineage at point."
  (interactive (list (magpi--section-intention-id)))
  (when intention-id
    (magpi--intention intention-id))
  (magpi-launch intention-id))
(defun magpi--action-time (action)
  (or (magpi-action-started-at action)
      (let ((created (magpi-action-created-at action)))
        (and created (seconds-to-time created)))
      '(0 0)))

(defun magpi--action-root (action)
  (or (and (magpi-action-launch action)
           (magpi-launch-spec-root (magpi-action-launch action)))
      (magpi-action-source-root action)))

(defun magpi--action-in-root-p (action root)
  "Return non-nil when ACTION belongs to ROOT."
  (let* ((root (file-truename root))
         (action-root (magpi--action-root action))
         (intention-id (magpi-action-intention-id action))
         (intention (and intention-id
                         (or (gethash intention-id magpi--intentions)
                             (ignore-errors (magpi--intention intention-id))))))
    (or (and action-root
             (equal (file-truename action-root) root))
        (and intention
             (equal (file-truename (magpi-intention-source-root intention))
                    root)))))

(defun magpi--join-action (disk)
  "Return this Emacs's theatre for DISK's id, or DISK (cold).

Paint does not write the registry."
  (let* ((id (magpi-action-id disk))
         (ram (gethash id magpi--actions)))
    (if (and (magpi-action-p ram)
             (or (magpi-action-observation ram)
                 (magpi-action-launch ram)
                 (gethash id magpi--handles)))
        ram
      disk)))

(defun magpi--actions-for-root (root)
  "Glance: disk kernels ⋈ this Emacs's theatre.  Paint does not puthash.

RAM-only rows (unpersisted test births) still appear."
  (let (actions ids)
    (dolist (record (magpi-action-list root))
      (when (magpi-action-p record)
        (let ((joined (magpi--join-action record)))
          (push (magpi-action-id joined) ids)
          (push joined actions))))
    (maphash
     (lambda (id action)
       (when (and (magpi-action-p action)
                  (not (member id ids))
                  (magpi--action-in-root-p action root))
         (push action actions)))
     magpi--actions)
    (sort actions
          (lambda (a b)
            (time-less-p (magpi--action-time b)
                         (magpi--action-time a))))))
(defun magpi--reconcile-actions-for-root (root)
  "Ask the adapter to re-emit live observations for ROOT's actions.

This is a snapshot pull, not part of event-driven paint."
  (let ((root (file-truename root)))
    (maphash
     (lambda (id action)
       (when-let ((action-root (magpi--action-root action)))
         (when (equal (file-truename action-root) root)
           (when-let ((handle (magpi--live-handle id)))
             (magpi-backend-reconcile
              magpi-backend handle
              (lambda (event) (magpi--handle-event id event)))))))
     magpi--actions)))

(defun magpi--refresh-visible-buffers ()
  (setq magpi--refresh-timer nil)
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (derived-mode-p 'magpi-status-mode)
        (ignore-errors (magit-refresh-buffer))))))

(defun magpi--schedule-refresh ()
  (when (timerp magpi--refresh-timer)
    (cancel-timer magpi--refresh-timer))
  (setq magpi--refresh-timer
        (run-with-idle-timer 0.15 nil #'magpi--refresh-visible-buffers)))


(defun magpi--actions-for-intention (intention-id)
  "Actions for INTENTION-ID from the glance join, oldest first."
  (let ((actions
         (seq-filter
          (lambda (action)
            (and (magpi-action-p action)
                 (equal (magpi-action-intention-id action) intention-id)))
          (magpi--actions-for-root (magpi--root)))))
    (sort actions
          (lambda (a b)
            (time-less-p (magpi--action-time a)
                         (magpi--action-time b))))))

(defun magpi--intention-metadata (intention)
  "Return metadata handed to Magit buffers for INTENTION."
  (let ((actions (magpi--actions-for-intention (magpi-intention-id intention))))
    (list :intention-id (magpi-intention-id intention)
          :objective (magpi-intention-objective intention)
          :branch (magpi-intention-branch intention)
          :base-ref (magpi-intention-base-ref intention)
          :action-ids (mapcar #'magpi-action-id actions)
          :writer-lease (copy-tree (magpi-intention-writer-lease intention))
          :audit (copy-tree (magpi-intention-audit intention))
          :chat-history
          (mapcar
           (lambda (action)
             (let* ((action-id (magpi-action-id action))
                    (handle (gethash action-id magpi--handles)))
               (list :action-id action-id
                     :prompt (magpi-action-prompt action)
                     :buffer (when (and handle
                                        (fboundp 'magpi-pimacs-handle-chat-buffer))
                               (when-let ((buffer (magpi-pimacs-handle-chat-buffer handle)))
                                 (and (buffer-live-p buffer) (buffer-name buffer)))))))
           actions))))

(defun magpi--bind-magit-metadata (intention)
  "Attach INTENTION metadata to the buffer opened by a delegated Magit action."
  (setq-local magpi-intention-metadata (magpi--intention-metadata intention)))

(defun magpi--land-magit (directory)
  "Open Magit for DIRECTORY when Magit is available. Failed Git lands here."
  (when (and directory (fboundp 'magit-status))
    (let ((default-directory (file-name-as-directory directory)))
      (ignore-errors (magit-status default-directory)))))

(defun magpi--changes-action (git-action &optional section)
  "Delegate Git GIT-ACTION to Magit from SECTION's Magit lineage."
  (let* ((intention-id (magpi--section-intention-id section))
         (action-id (magpi--section-action-id section))
         (intention (and intention-id (ignore-errors (magpi--intention intention-id)))))
    (if intention
        (let* ((path (or (magpi-intention-worktree-path intention)
                         (user-error "Start an action before opening this intention's Git changes"))))
          (pcase git-action
            ('status
             (magit-status path)
             (magpi--bind-magit-metadata intention))
            ('diff
             (let ((default-directory path))
               (magit-diff-range (magpi-intention-work-range intention) nil))
             (magpi--bind-magit-metadata intention))
            ('log
             (let ((default-directory path))
               (magit-log-range (magpi-intention-work-range intention) nil))
             (magpi--bind-magit-metadata intention))
            ('commit
             (puthash (file-truename path) (magpi--intention-metadata intention)
                      magpi--pending-commit-metadata)
             (let ((default-directory path))
               (call-interactively #'magit-commit-create))
             (magpi--bind-magit-metadata intention))
            (_ (user-error "Unknown Magpi changes action: %S" git-action))))
      (magpi--standalone-changes git-action action-id))))

(defun magpi--standalone-changes (git-action action-id)
  "Magit doors for a standalone Action: dirt at source-root, work from spawn-oid."
  (let* ((record (or (and action-id (magpi--lookup-action action-id))
                     (user-error "This item has no managed intention")))
         (root (or (magpi-action-source-root record)
                   (user-error "This action has no repository locator"))))
    (pcase git-action
      ('status (magit-status root))
      ('diff
       (let ((default-directory root))
         (magit-diff-range
          (magpi-store-frozen-range root (magpi-action-spawn-oid record) t)
          nil)))
      ('log
       (let ((default-directory root))
         (magit-log-range
          (magpi-store-frozen-range root (magpi-action-spawn-oid record) t)
          nil)))
      ('commit (user-error "Standalone actions have no Magpi merge destination"))
      (_ (user-error "Unknown Magpi changes action: %S" git-action)))))

;;;###autoload
(defun magpi-changes-status ()
  (interactive)
  (magpi--changes-action 'status))

;;;###autoload
(defun magpi-changes-diff ()
  (interactive)
  (magpi--changes-action 'diff))

;;;###autoload
(defun magpi-changes-log ()
  (interactive)
  (magpi--changes-action 'log))

;;;###autoload
(defun magpi-changes-commit ()
  (interactive)
  (magpi--changes-action 'commit))

(defun magpi--react-intention (section)
  (let ((id (magpi--section-intention-id section)))
    (and id (ignore-errors (magpi--intention id)))))

(defun magpi--react-release (&optional section)
  "Operator recovery: release the writer lease for SECTION's intention."
  (let* ((intention (or (magpi--react-intention section)
                        (user-error "This item has no managed intention")))
         (lease (or (magpi-intention-writer-lease intention)
                    (user-error "This intention has no writer lease")))
         (action-id (plist-get lease :action-id))
         (selected (magpi--section-action-id section)))
    (when (and selected (not (equal selected action-id)))
      (user-error "Selected action %s does not hold the writer lease" selected))
    (when (yes-or-no-p
           (format "Release writer %s for %s? This is recovery, not a fence. "
                   action-id (magpi-intention-objective intention)))
      (setq intention
            (magpi-intention-release-writer intention action-id "operator recovery"))
      (puthash (magpi-intention-id intention) intention magpi--intentions)
      (magpi--schedule-refresh)
      (message "Released writer %s (operator recovery; Pi is not fenced)" action-id))))

(defun magpi--react-merge (&optional section)
  "Merge SECTION's intention, then land in Magit. Failure also lands in Magit."
  (let* ((intention (or (magpi--react-intention section)
                        (user-error "This item has no managed intention")))
         (source (magpi-intention-source-root intention)))
    (when (yes-or-no-p (format "Merge %s into %s? "
                               (magpi-intention-objective intention)
                               (magpi-intention-base-ref intention)))
      (condition-case err
          (progn
            (magpi-intention-merge (magpi-intention-id intention))
            (magpi--schedule-refresh)
            (magpi--land-magit source))
        (error
         (magpi--intentions-for-root source)
         (magpi--schedule-refresh)
         (magpi--land-magit source)
         (signal (car err) (cdr err)))))))

(defun magpi-discard--git-scope-p ()
  "Return non-nil when Magit would discard a Git hunk, file, or list."
  (and (fboundp 'magit-diff-scope)
       (memq (ignore-errors (magit-diff-scope))
             '(region hunk hunks file files list))))

(defun magpi-discard-scope (&optional section)
  "Return discard depth at SECTION, innermost first.

Like `magit-diff-scope': chat is a hunk, action a file, intention a
list, worktree the Magit checkout Magpi owns.  `ask' means React, not
discard.  Nil means nothing Magpi should discard."
  (cond
   ((magpi--section-ask-id section) 'ask)
   ((and (derived-mode-p 'pimacs-chat-mode)
         (not (derived-mode-p 'magpi-status-mode)))
    'chat)
   ((and (magpi--section-action-id section)
         (not (magpi--section-intention-id section)))
    'chat)
   ((magpi--section-action-id section) 'action)
   ((magpi--section-intention-id section) 'intention)
   ((and magpi-intention-metadata
         (not (magpi-discard--git-scope-p)))
    'worktree)))

(defun magpi-discard--chat-action-id (&optional buffer)
  "Return the Magpi action id whose handle owns BUFFER, or nil."
  (setq buffer (or buffer (current-buffer)))
  (catch 'found
    (maphash
     (lambda (id handle)
       (when (and handle
                  (fboundp 'magpi-pimacs-handle-chat-buffer)
                  (eq (ignore-errors
                        (magpi-pimacs-handle-chat-buffer handle))
                      buffer))
         (throw 'found id)))
     magpi--handles)
    nil))

(defun magpi-discard--kernel (action)
  "Return ACTION with this Emacs's theatre stripped.  Kernel identity stays."
  (let ((next (copy-magpi-action action)))
    (setf (magpi-action-launch next) nil
          (magpi-action-observation next) nil
          (magpi-action-prompt next) nil
          (magpi-action-started-at next) nil)
    next))

(defun magpi-discard--child-actions (intention-id)
  "RAM actions nested under INTENTION-ID.  Live handles live here."
  (let (actions)
    (maphash (lambda (_id action)
               (when (and (magpi-action-p action)
                          (equal (magpi-action-intention-id action)
                                 intention-id))
                 (push action actions)))
             magpi--actions)
    actions))

(defun magpi-discard--drop-theatre (action-id)
  "Terminate ACTION-ID's agent and drop theatre.  Kernel remains.

Does not invent disconnect: the operator discarded this doing's chat."
  (when-let ((handle (gethash action-id magpi--handles)))
    (let ((chat (and (fboundp 'magpi-pimacs-handle-chat-buffer)
                     (ignore-errors (magpi-pimacs-handle-chat-buffer handle)))))
      (magpi-backend-terminate magpi-backend handle)
      (when (and (bufferp chat) (buffer-live-p chat))
        (kill-buffer chat)))
    (remhash action-id magpi--handles))
  (when-let ((action (gethash action-id magpi--actions)))
    (puthash action-id (magpi-discard--kernel action) magpi--actions)))

(defun magpi-discard--chat (action-id)
  "Discard ACTION-ID's chat (hunk depth).  Does not release a writer."
  (unless action-id
    (user-error "This chat is not a Magpi action"))
  (when (yes-or-no-p
         (format "Discard chat for %s? This terminates the agent. " action-id))
    (magpi-discard--drop-theatre action-id)
    (magpi--schedule-refresh)
    (message "Discarded chat %s" action-id)))

(defun magpi-discard--action (action-id)
  "Discard ACTION-ID (file depth): chat, then writer lease if this doing holds it."
  (let* ((action (magpi--action action-id))
         (intention-id (magpi-action-intention-id action))
         (intention (and intention-id (ignore-errors (magpi--intention intention-id))))
         (lease (and intention (magpi-intention-writer-lease intention)))
         (holds (equal action-id (plist-get lease :action-id))))
    (when (yes-or-no-p
           (format "Discard action %s? This terminates the agent%s. "
                   action-id
                   (if holds " and releases the writer" "")))
      (magpi-discard--drop-theatre action-id)
      (when (and intention holds)
        (setq intention
              (magpi-intention-release-writer intention action-id "discard"))
        (puthash intention-id intention magpi--intentions))
      (magpi--schedule-refresh)
      (message "Discarded action %s" action-id))))

(defun magpi-discard--intention (intention)
  "Discard INTENTION (list depth): nested chats/actions, then worktree."
  (let* ((intention (or intention
                        (user-error "This item has no managed intention")))
         (actions (magpi-discard--child-actions (magpi-intention-id intention)))
         (checkout (plist-get (magpi-intention-git-facts intention) :checkout))
         (n (length actions))
         (extra (if (zerop n)
                    "This force-removes it. "
                  (format "This terminates %d action%s and force-removes it. "
                          n (if (= n 1) "" "s")))))
    (when (yes-or-no-p
           (format "Discard %s worktree for %s? %s"
                   (or checkout "managed")
                   (magpi-intention-objective intention)
                   extra))
      (dolist (action actions)
        (magpi-discard--drop-theatre (magpi-action-id action)))
      (when-let ((lease (magpi-intention-writer-lease intention)))
        (setq intention
              (magpi-intention-release-writer
               intention (plist-get lease :action-id) "discard"))
        (puthash (magpi-intention-id intention) intention magpi--intentions))
      (setq intention (magpi-intention-discard-record intention))
      (puthash (magpi-intention-id intention) intention magpi--intentions)
      (magpi--schedule-refresh)
      (message "Discarded intention %s" (magpi-intention-id intention)))))

(defun magpi-discard--worktree (intention)
  "Discard the Magpi checkout (Magit depth above hunk).  Active means intention."
  (pcase (magpi-intention-state intention)
    ('active (magpi-discard--intention intention))
    ((or 'merged 'discarded)
     (when (yes-or-no-p
            (format "Remove leftover worktree for %s? This force-removes it. "
                    (magpi-intention-objective intention)))
       (setq intention (magpi-intention-remove-worktree intention))
       (puthash (magpi-intention-id intention) intention magpi--intentions)
       (magpi--schedule-refresh)))
    (_ (user-error "Cannot discard %s intention"
                   (magpi-intention-state intention)))))

(defun magpi-discard--apply (&optional section)
  "Apply Magpi discard at SECTION's resolved depth."
  (pcase (magpi-discard-scope section)
    ('ask (user-error "Pi-ask at point; use React, not discard"))
    ('chat
     (magpi-discard--chat
      (or (magpi--section-action-id section)
          (magpi-discard--chat-action-id))))
    ('action (magpi-discard--action (magpi--section-action-id section)))
    ('intention
     (magpi-discard--intention (magpi--react-intention section)))
    ('worktree
     (let ((id (plist-get magpi-intention-metadata :intention-id)))
       (unless id
         (user-error "No Magpi worktree at point"))
       (magpi-discard--worktree (magpi--intention id))))
    (_ (user-error "No Magpi discard at point"))))

(defun magpi-discard--magit-maybe ()
  "Let Magit `k' discard a Magpi worktree when Git has no hunk/file/list."
  (when (and magpi-intention-metadata
             (not (magpi-discard--git-scope-p)))
    (magpi-discard--apply nil)
    t))

(with-eval-after-load 'magit
  (advice-add 'magit-discard :before-until #'magpi-discard--magit-maybe))

;;;###autoload
(defun magpi-discard (&optional section)
  "Discard the Magpi surface or Git change at point.

Like Magit's discard hunk: the section at point chooses the depth.
Chat, action, intention, and worktree each include nested children.
On a Magit hunk, file, or unstaged list this is `magit-discard'."
  (interactive)
  (setq section (or section (and (fboundp 'magit-current-section)
                                 (magit-current-section))))
  (if (and (not (derived-mode-p 'magpi-status-mode))
           (magpi-discard--git-scope-p)
           (fboundp 'magit-discard))
      (call-interactively #'magit-discard)
    (magpi-discard--apply section)))

(defun magpi--react-discard (&optional section)
  "Force-remove SECTION's worktree after an honest confirmation.

Deep: nested chats and actions terminate first."
  (magpi-discard--intention
   (or (magpi--react-intention section)
       (user-error "This item has no managed intention"))))

(defun magpi--react-observation (section)
  (when-let* ((id (magpi--section-action-id section))
              (action (gethash id magpi--actions)))
    (magpi-action-observation action)))

(defun magpi--react-disconnected-p (section)
  (when-let ((observation (magpi--react-observation section)))
    (eq (magpi-observation-connection-state observation) 'disconnected)))

(defun magpi--react-pending-ask (section)
  "Return the pending Pi-ask at SECTION, or nil when judgment is stale."
  (let* ((obs (magpi--react-observation section))
         (asks (and obs (magpi-observation-asks obs)))
         (ask-id (magpi--section-ask-id section))
         (ask (if ask-id
                  (seq-find (lambda (candidate)
                              (equal ask-id (magpi-ask-id candidate)))
                            asks)
                (seq-find (lambda (candidate)
                            (eq (magpi-ask-state candidate) 'pending))
                          asks))))
    (and ask (eq (magpi-ask-state ask) 'pending) ask)))

(defun magpi--react-answer (response section)
  "Route an explicit Pi-ask RESPONSE to SECTION's live action."
  (let* ((action-id (magpi--section-action-id section))
         (ask (magpi--react-pending-ask section))
         (handle (and action-id (magpi--live-handle action-id))))
    (when (magpi--react-disconnected-p section)
      (user-error "Action %s is disconnected — ownership uncertain; do not fake completion"
                  action-id))
    (unless handle
      (user-error "This Pi-ask has no live adapter handle"))
    (unless ask
      (user-error "This Pi-ask is no longer pending"))
    (unless (magpi-backend-ask-supported-p magpi-backend)
      (user-error "This adapter cannot answer structured Pi-asks"))
    (magpi-backend-respond-ask magpi-backend handle (magpi-ask-id ask) response)
    (message "%s sent for Pi-ask %s; awaiting adapter confirmation"
             (capitalize (symbol-name response)) (magpi-ask-id ask))))

(defun magpi-react--choices (&optional section)
  "Return React menu choices for SECTION via semantic selectors."
  (cond
   ((magpi--react-disconnected-p section)
    '(("Show uncertainty" . uncertainty)))
   ((magpi--react-pending-ask section)
    '(("Approve" . approved) ("Reject" . rejected)))
   ;; Non-pending ask at point: no reaction (do not fall through to action).
   ((magpi--section-ask-id section)
    nil)
   ((magpi--section-action-id section)
    (let ((intention (magpi--react-intention section)))
      (if (and intention (magpi-intention-writer-lease intention))
          '(("Release writer" . release))
        nil)))
   ((magpi--section-intention-id section)
    (let ((intention (magpi--react-intention section)))
      (cond
       ((and intention (magpi-intention-writer-lease intention))
        '(("Release writer" . release)))
       ((and intention (eq (magpi-intention-state intention) 'active))
        '(("Merge" . merge) ("Discard" . discard)))
       (t nil))))
   (t nil)))

;;;###autoload
(defun magpi-react (&optional section)
  "Open React for the Magit section at point.

Binary, at point, Magit-short.  Not chat-as-UI.  Offers depend on lineage:
  Pi-ask → approve / reject
  intention + writer → release (honest recovery)
  intention quiescent → merge / discard
  action disconnected → show uncertainty (no fake completion)"
  (interactive)
  (setq section (or section (and (fboundp 'magit-current-section)
                                 (magit-current-section))))
  (let ((choices (magpi-react--choices section)))
    (unless choices
      (user-error "No reaction applies at point"))
    (magpi-react--run section choices)))

(defvar magpi-react--pending nil
  "Transient React payload: (SECTION . CHOICES).")

(defun magpi-react--run (section choices)
  "Run React for SECTION with CHOICES — Transient when live, else two-key read."
  (setq magpi-react--pending (cons section choices))
  (if (and (fboundp 'transient-setup)
           (fboundp 'transient-define-prefix))
      (magpi-react--transient)
    (magpi-react--fallback section choices)))

(defun magpi-react--fallback (section choices)
  "Fallback React when Transient is unavailable: single-key from CHOICES."
  (let* ((prompt (mapconcat
                  (lambda (c)
                    (format "[%s] %s"
                            (downcase (substring (car c) 0 1))
                            (car c)))
                  choices "  "))
         (keys (mapcar (lambda (c) (downcase (aref (car c) 0))) choices))
         (ch (read-char-choice (concat "React: " prompt " ") keys))
         (choice (cdr (seq-find (lambda (c)
                                  (eq (downcase (aref (car c) 0)) ch))
                                choices))))
    (magpi-react--dispatch section choice)))

(defun magpi-react--dispatch (section choice)
  "Apply React CHOICE to SECTION."
  (pcase choice
    ((or 'approved 'rejected) (magpi--react-answer choice section))
    ('release (magpi--react-release section))
    ('merge (magpi--react-merge section))
    ('discard (magpi--react-discard section))
    ('uncertainty
     (message "Action %s is disconnected — ownership uncertain; do not fake completion"
              (magpi--section-action-id section)))
    (_ (user-error "Unknown reaction: %S" choice))))

(defun magpi-react--call (choice)
  "Transient suffix helper: dispatch CHOICE for the pending React section."
  (interactive)
  (pcase-let ((`(,section . ,_) magpi-react--pending))
    (magpi-react--dispatch section choice)))

(when (fboundp 'transient-define-prefix)
  (transient-define-prefix magpi-react--transient ()
    "React — intervene at point."
    [:class transient-column
     :description
     (lambda ()
       (pcase-let ((`(,section . ,_) magpi-react--pending))
         (format "React · %s"
                 (or (magpi--section-ask-id section)
                     (magpi--section-action-id section)
                     (magpi--section-intention-id section)
                     (magpi--section-root section)
                     "?"))))
     :setup-children
     (lambda (_)
       (pcase-let ((`(,_ . ,choices) magpi-react--pending))
         (mapcar
          (lambda (c)
            (let* ((label (car c))
                   (sym (cdr c))
                   (key (downcase (substring label 0 1))))
              (transient-parse-suffix
               'magpi-react--transient
               (list key label
                     (lambda ()
                       (interactive)
                       (magpi-react--call sym))))))
          choices)))]))

(defun magpi--visit-or-open-action (id)
  "Visit a live handle, or resume the same spawn path without send-initial.

Retry is keyed on known process state, not buffer liveness.  Never a second birth."
  (if (eq (magpi--known-process-state id) 'live)
      (magpi-backend-visit magpi-backend (gethash id magpi--handles))
    (magpi--ensure-process id t)
    (if-let ((handle (magpi--live-handle id)))
        (magpi-backend-visit magpi-backend handle)
      (user-error "Action %s is not live" id))))

(defun magpi--visit-observed-file (action-id path)
  (let* ((action (magpi--action action-id))
         (root (file-name-as-directory
                (file-truename
                 (or (magpi--action-root action)
                     (user-error "This action has no repository locator")))))
         (file (file-truename (expand-file-name path root))))
    (unless (file-in-directory-p file root)
      (user-error "Observed file is outside the action project"))
    (find-file file)))

;;;###autoload
(defun magpi-visit (&optional section)
  "Visit the Magit section at point.

Precedence uses semantic selectors only: ask → path → action → intention → root.
Pi-ask opens React.  Exact path opens the observed file.  Action visits chat.
Intention opens Magit changes.  Root visits the adapter chat."
  (interactive)
  (setq section (or section (and (fboundp 'magit-current-section)
                                 (magit-current-section))))
  (cond
   ((magpi--section-ask-id section)
    (magpi-react section))
   ((and (magpi--section-path section) (magpi--section-action-id section))
    (magpi--visit-observed-file (magpi--section-action-id section)
                                (magpi--section-path section)))
   ((magpi--section-action-id section)
    (magpi--visit-or-open-action (magpi--section-action-id section)))
   ((magpi--section-intention-id section)
    (magpi--changes-action 'status section))
   ((magpi--section-root section)
    (magpi-backend-visit-root magpi-backend (magpi--section-root section)))
   ((fboundp 'magpi-status-toggle-section)
    (magpi-status-toggle-section))
   (t
    (user-error "Nothing to visit at point"))))

;;;###autoload
(defun magpi-status (&optional root)
  "Open the Magit-backed Magpi status buffer for ROOT.

Titles: query `magpi-backend-chat-candidates' once, pass one callback.
`g' refreshes that snapshot then reconciles.  Event paint reuses it.
Does not spawn, write the registry, or change auspice."
  (interactive)
  (let* ((root (file-name-as-directory
                (expand-file-name (or root (magpi--root)))))
         (candidates (magpi-backend-chat-candidates magpi-backend root)))
    (magpi-status-open root
                       (lambda ()
                         (magpi--actions-for-root root))
                       (lambda ()
                         (append (magpi--intentions-for-root root)
                                 (seq-filter #'magpi-unreadable-p
                                             (magpi-intention-list root))))
                       (lambda ()
                         (setq candidates
                               (magpi-backend-chat-candidates magpi-backend root))
                         (magpi--reconcile-actions-for-root root)
                         (magpi-launch-refresh-catalog root))
                       (lambda (action)
                         (when-let ((candidate (magpi--chat-candidate-for-action
                                                action candidates))
                                    (label (plist-get candidate :label)))
                           (cons label (plist-get candidate :last))))
                       (lambda (action)
                         (when-let ((handle (gethash (magpi-action-id action)
                                                     magpi--handles)))
                           (magpi-backend-history-pending magpi-backend handle))))
    (magpi--reconcile-actions-for-root root)
    (magpi-launch-refresh-catalog root)))

(provide 'magpi)
;;; magpi.el ends here
