;;; magpi-status.el --- Glance: intention, action, ask marks -*- lexical-binding: t; -*-

;; One job: glance surfaces as typed Magit sections.  It does not spawn,
;; reconcile, or own the registry.

(require 'magit-mode)
(require 'magit-section)
(require 'magpi-action)
(require 'magpi-launch)
(require 'magpi-intention)

(defvar-local magpi-status-root nil)
(defvar-local magpi-status-actions-function nil)
(defvar-local magpi-status-intentions-function nil)
(defvar-local magpi-status-visit-function nil)
(defvar-local magpi-status-spawn-function nil)
(defvar-local magpi-status-create-function nil)
(defvar-local magpi-status-prepare-function nil)
(defvar-local magpi-status-changes-action-function nil)
(defvar-local magpi-status-react-function nil)
(defvar-local magpi-status-bind-function nil)
;;; Faces — six roles
;;
;; identity   authored intent
;; live       running
;; pending    starting, writer
;; quiet      idle, meta, keys, last, placeholder
;; alert      problem, disconnected
;; evidence   observed files

(defface magpi-status-header
  '((t :inherit bold))
  "Project header line (`MAGPI · root')."
  :group 'magpi)

(defface magpi-status-identity
  '((t :inherit font-lock-function-name-face :weight bold))
  "Initial title from the first message or frozen context."
  :group 'magpi)

(defface magpi-status-title
  '((t :inherit font-lock-string-face))
  "Adapter-observed replacement for the generated chat title."
  :group 'magpi)

(defface magpi-status-live
  '((t :inherit success :weight bold))
  "Active work indicator."
  :group 'magpi)

(defface magpi-status-pending
  '((t :inherit warning))
  "Startup indicator."
  :group 'magpi)

(defface magpi-status-quiet
  '((t :inherit shadow))
  "Secondary facts: idle, thinking, model, keys, last, placeholder."
  :group 'magpi)

(defface magpi-status-alert
  '((t :inherit error :weight bold))
  "Problem or disconnection."
  :group 'magpi)

(defface magpi-status-evidence
  '((t :inherit font-lock-string-face))
  "Observed evidence paths."
  :group 'magpi)

(defun magpi-status--face (text face)
  "Return TEXT with FACE, or TEXT unchanged when blank.

Magit-section enables font-lock with empty keywords, so a `face'-only
property paints once and is then stripped.  Set both `face' and
`font-lock-face', matching Magit's own heading helpers."
  (if (and (stringp text) (not (string-empty-p text)) face)
      (propertize text 'face face 'font-lock-face face)
    text))

(defun magpi-status--placeholder ()
  (magpi-status--face "◯" 'magpi-status-quiet))

(defun magpi-status--activity-state-face (state)
  (pcase state
    ('starting 'magpi-status-pending)
    ('running 'magpi-status-live)
    (_ 'magpi-status-quiet)))

(defun magpi-status--activity-state-label (state)
  "Render semantic activity STATE."
  (pcase state
    ('starting "starting")
    ('running "running")
    ('idle "idle")
    ('unknown "unknown")
    (_ "unknown")))

(defun magpi-status--generated-title (action)
  "Return ATTEMPT's task prompt or its context-derived fallback."
  (or (magpi-action-prompt action)
      (when-let ((launch (magpi-action-launch action)))
        (magpi-launch-context-title (magpi-launch-spec-context launch)))
      "New task"))
(defun magpi-status--heading (action)
  "Return ATTEMPT's observed title or its stable generated chat title."
  (let* ((observation (magpi-action-observation action))
         (title (and observation (magpi-observation-display-title observation))))
    (magpi-status--face
     (truncate-string-to-width (or title (magpi-status--generated-title action))
                               60 nil nil "…")
     (if title 'magpi-status-title 'magpi-status-identity))))
(defun magpi-status--heading-parts (action)
  "Return (TEXT . FACE) pairs for ATTEMPT's heading suffix."
  (let* ((launch (magpi-action-launch action))
         (observation (magpi-action-observation action))
         (state (and observation (magpi-observation-activity-state observation)))
         (running (and observation (magpi-observation-running-model observation))))
    (delq nil
          (list (cons (if observation
                          (magpi-status--activity-state-label state)
                        "cold")
                      (if observation
                          (magpi-status--activity-state-face state)
                        'magpi-status-quiet))
                (cons (magpi-launch-thinking-label
                       (and launch (magpi-launch-spec-thinking launch)))
                      'magpi-status-quiet)
                (when running
                  (cons running 'magpi-status-quiet))))))

(defun magpi-status--heading-suffix (action)
  "Return scannable activity, thinking, and running model for ATTEMPT."
  (mapconcat #'car (magpi-status--heading-parts action) " · "))

(defun magpi-status--format-heading-suffix (action)
  "Like `magpi-status--heading-suffix' with hierarchical faces."
  (let ((sep (magpi-status--face " · " 'magpi-status-quiet)))
    (mapconcat (lambda (part)
                 (magpi-status--face (car part) (cdr part)))
               (magpi-status--heading-parts action)
               sep)))

(defun magpi-status--insert-kv (key value &optional value-face)
  "Insert a labeled KEY/VALUE row.  VALUE may already be propertized."
  (insert "    "
          (magpi-status--face (format "%-10s" key) 'magpi-status-quiet)
          " "
          (cond
           ((and (stringp value) (not (string-empty-p value)))
            (if (text-properties-at 0 value)
                value
              (magpi-status--face value (or value-face 'default))))
           (t (magpi-status--placeholder)))
          "\n"))
(defun magpi-status--ask-state-presentation (state)
  "Return the glance symbol, label, and face for Pi-ask STATE."
  (pcase state
    ('approved '("✓" "answered" magpi-status-live))
    ('rejected '("✕" "rejected" magpi-status-alert))
    ('dismissed '("–" "dismissed" magpi-status-quiet))
    (_ '("?" "ask" magpi-status-pending))))

(defun magpi-status--pending-ask-p (observation)
  "Return non-nil when OBSERVATION has a pending Pi-ask."
  (seq-find (lambda (ask) (eq (magpi-ask-state ask) 'pending))
            (and observation (magpi-observation-asks observation))))

(defun magpi-status--glance (observation)
  "Return a short colour glance mark for OBSERVATION, or nil."
  (cond
   ((and observation (eq (magpi-observation-connection-state observation) 'disconnected))
    (magpi-status--face "!" 'magpi-status-alert))
   ((magpi-status--pending-ask-p observation)
    (magpi-status--face "?" 'magpi-status-pending))
   (t nil)))

(defun magpi-status--insert-ask (action-id ask asks depth seen)
  "Insert Pi-ask fact ASK beneath ACTION-ID (glance, not a Magpi being)."
  (let* ((id (magpi-ask-id ask))
         (presentation (magpi-status--ask-state-presentation
                        (magpi-ask-state ask)))
         (indent (make-string depth ?\s))
         (children (seq-filter
                    (lambda (candidate)
                      (equal id (magpi-ask-parent-id candidate)))
                    asks)))
    (unless (member id seen)
      (magit-insert-section (magpi-ask (cons action-id id) nil)
        (magit-insert-heading
         (concat indent
                 (magpi-status--face (nth 0 presentation) (nth 2 presentation))
                 " "
                 (magpi-status--face (nth 1 presentation) (nth 2 presentation))
                 "  "
                 (magpi-status--face
                  (or (magpi--one-line (magpi-ask-question ask) 78)
                      "Pi-ask")
                  'magpi-status-identity)))
        (when (and (eq (magpi-ask-state ask) 'pending)
                   magpi-status-react-function)
          (insert (make-string (+ depth 2) ?\s)
                  (magpi-status--face "a  react" 'magpi-status-quiet)
                  "\n"))
        (when-let ((requester (magpi-ask-requester ask)))
          (insert (make-string (+ depth 2) ?\s)
                  (magpi-status--face "requested by  " 'magpi-status-quiet)
                  (magpi-status--face requester 'magpi-status-quiet) "\n"))
        (when-let ((detail (magpi-ask-detail ask)))
          (insert (make-string (+ depth 2) ?\s)
                  (magpi-status--face "details       " 'magpi-status-quiet)
                  (magpi-status--face (magpi--one-line detail 110)
                                      'magpi-status-quiet) "\n"))
        (when-let ((paths (magpi-ask-affected-paths ask)))
          (magit-insert-section (magpi-ask-paths (cons action-id id) nil)
            (magit-insert-heading
             (concat (make-string (+ depth 2) ?\s)
                     (magpi-status--face
                      (format "affected paths (%d)" (length paths))
                      'magpi-status-evidence)))
            (dolist (path paths)
              (magit-insert-section (magpi-ask-path (list action-id id path) nil)
                (insert (make-string (+ depth 4) ?\s)
                        (magpi-status--face path 'magpi-status-evidence) "\n")))))
        (dolist (child children)
          (magpi-status--insert-ask action-id child asks (+ depth 2)
                                    (cons id seen)))))))

(defun magpi-status--insert-asks (action-id asks)
  "Render top-level and orphaned Pi-asks with nested descendants."
  (let ((ids (mapcar #'magpi-ask-id asks)))
    (dolist (ask asks)
      (when (or (null (magpi-ask-parent-id ask))
                (not (member (magpi-ask-parent-id ask) ids)))
        (magpi-status--insert-ask action-id ask asks 4 nil)))))

(defun magpi-status--insert-action (action)
  "Insert ATTEMPT as a typed, foldable Magit section.

The heading holds the chat title and scan meta.  The body holds role,
activity detail, connection, problem, last response, and observed files."
  (let* ((id (magpi-action-id action))
         (launch (magpi-action-launch action))
         (observation (magpi-action-observation action))
         (role (and launch (magpi-launch-spec-role launch))))
    (magit-insert-section (magpi-action id nil)
      (magit-insert-heading
       (concat (or (magpi-status--glance observation) "")
               (when (magpi-status--glance observation) " ")
               (magpi-status--heading action)
               "    "
               (magpi-status--format-heading-suffix action)))
      (when role
        (magpi-status--insert-kv
         "role"
         (magpi-launch-role-label role)
         (if (eq role 'writer)
             'magpi-status-pending
           'magpi-status-quiet)))
      (when observation
        (when-let ((activity (magpi-observation-activity observation)))
          (when (stringp activity)
            (magpi-status--insert-kv
             "activity" activity
             (magpi-status--activity-state-face
              (magpi-observation-activity-state observation)))))
        (when (eq (magpi-observation-connection-state observation) 'disconnected)
          (magpi-status--insert-kv "connection" "disconnected"
                                   'magpi-status-alert))
        (when-let ((problem (magpi-observation-problem observation)))
          (magpi-status--insert-kv "problem" problem 'magpi-status-alert))
        (when-let ((last (magpi-observation-last-response observation)))
          (magpi-status--insert-kv "last" last 'magpi-status-quiet))
        (when-let ((asks (magpi-observation-asks observation)))
          (magpi-status--insert-asks id asks))
        (when-let ((files (magpi-observation-observed-files observation)))
          (magit-insert-section (magpi-observed-files id nil)
            (magit-insert-heading
             (magpi-status--face "    observed files" 'magpi-status-quiet))
            (dolist (file files)
              (magit-insert-section (magpi-observed-file (cons id file) nil)
                (insert "      "
                        (magpi-status--face file 'magpi-status-evidence)
                        "\n")))))))))

(defun magpi-status--intention-suffix (intention)
  "Return branch and checkout glance for INTENTION — depth stays in Magit."
  (let* ((facts (magpi-intention-git-facts intention))
         (branch (or (magpi-intention-branch intention) "—"))
         (checkout (or (plist-get facts :checkout) 'unknown))
         (ahead (plist-get facts :ahead))
         (behind (plist-get facts :behind)))
    (if (and ahead behind)
        (format "%s · %s · +%s -%s" branch checkout ahead behind)
      (format "%s · %s" branch checkout))))

(defun magpi-status--insert-bindings (intention)
  "Render the durable file and chat tags on INTENTION."
  (when-let ((bindings (magpi-intention-bindings intention)))
    (magit-insert-section (magpi-bindings (magpi-intention-id intention) nil)
      (magit-insert-heading
       (magpi-status--face "    bindings" 'magpi-status-quiet))
      (dolist (attachment bindings)
        (let ((tags (plist-get attachment :tags)))
          (insert "      "
                  (magpi-status--face
                   (format "@%s %s%s"
                           (plist-get attachment :kind)
                           (or (plist-get attachment :label)
                               (plist-get attachment :reference))
                           (if tags (format "  #%s" (string-join tags " #")) ""))
                   'magpi-status-evidence)
                  "\n"))))))
(defun magpi-status--insert-intention (intention actions)
  "Insert persisted INTENTION with nested independent ACTIONS."
  (magit-insert-section (magpi-intention (magpi-intention-id intention) nil)
    (magit-insert-heading
     (concat (when (magpi-intention-writer-lease intention)
               (concat (magpi-status--face "?" 'magpi-status-pending) " "))
             (magpi-status--face (magpi-intention-objective intention)
                                 'magpi-status-identity)
             "    "
             (magpi-status--face (magpi-status--intention-suffix intention)
                                 'magpi-status-quiet)))
    (magpi-status--insert-bindings intention)
    (mapc #'magpi-status--insert-action actions)))

(defun magpi-status--insert-records (intentions actions)
  "Insert persisted INTENTIONS and nest their ACTIONS."
  (let ((groups (make-hash-table :test #'equal)) ungrouped known)
    (dolist (action actions)
      (if-let ((intention-id (magpi-action-intention-id action)))
          (puthash intention-id
                   (append (gethash intention-id groups) (list action)) groups)
        (push action ungrouped)))
    (mapc #'magpi-status--insert-action (nreverse ungrouped))
    (dolist (intention intentions)
      (cond
       ((magpi-intention-p intention)
        (push (magpi-intention-id intention) known)
        (magpi-status--insert-intention
         intention (gethash (magpi-intention-id intention) groups)))
       ((magpi-unreadable-p intention)
        (insert (magpi-status--face
                 (format "    unreadable %s · %s"
                         (file-name-nondirectory (magpi-unreadable-path intention))
                         (magpi-unreadable-error intention))
                 'magpi-status-alert)
                "\n"))))
    ;; Keep in-memory actions visible if their persisted record is unreadable.
    (maphash
     (lambda (id grouped)
       (unless (member id known)
         (mapc #'magpi-status--insert-action grouped)))
     groups)))

(defun magpi-status-refresh-buffer ()
  "Render the concise persisted-intention dashboard."
  (magit-insert-section (magpi-status magpi-status-root)
    (magit-insert-heading
     (magpi-status--face
      (format "MAGPI · %s"
              (file-name-nondirectory
               (directory-file-name magpi-status-root)))
      'magpi-status-header))
    (let ((actions (and magpi-status-actions-function
                         (funcall magpi-status-actions-function)))
          (intentions (and magpi-status-intentions-function
                           (funcall magpi-status-intentions-function))))
      (if (or intentions actions)
          (magpi-status--insert-records intentions actions)
        (insert (magpi-status--face
                 (concat "    i  create intention
"
                         "    @  bind context
"
                         "    s  spawn action
"
                         "    a  react
")
                 'magpi-status-quiet))))))

(defun magpi-status-target-at-point ()
  "Return the typed Magpi target at point, or nil."
  (cond
   ((magit-section-value-if 'magpi-ask-path)
    (pcase-let ((`(,action-id ,ask-id ,path)
                 (magit-section-value-if 'magpi-ask-path)))
      (list :kind 'ask-path :action-id action-id
            :ask-id ask-id :path path)))
   ((magit-section-value-if 'magpi-ask)
    (pcase-let ((`(,action-id . ,ask-id)
                 (magit-section-value-if 'magpi-ask)))
      (list :kind 'ask :action-id action-id :ask-id ask-id)))
   ((magit-section-value-if 'magpi-observed-file)
    (pcase-let ((`(,action-id . ,path)
                 (magit-section-value-if 'magpi-observed-file)))
      (list :kind 'observed-file :action-id action-id :path path)))
   ((magit-section-value-if 'magpi-action)
    (list :kind 'action :action-id (magit-section-value-if 'magpi-action)))
   ((magit-section-value-if 'magpi-intention)
    (list :kind 'intention
          :intention-id (magit-section-value-if 'magpi-intention)))
   ((magit-section-value-if 'magpi-status)
    (list :kind 'root :root (magit-section-value-if 'magpi-status)))))

(defun magpi-status-toggle-section ()
  "Toggle a detail section; preview the status root without hiding it."
  (interactive)
  (when-let ((section (magit-current-section)))
    (if (magit-section-value-if 'magpi-status)
        (magit-section-show-headings section)
      (magit-section-toggle section))))

(defun magpi-status-visit-at-point ()
  "Visit the typed Magpi object at point, or toggle its section."
  (interactive)
  (if-let ((target (magpi-status-target-at-point)))
      (if magpi-status-visit-function
          (funcall magpi-status-visit-function target)
        (user-error "No visitor is configured for this Magpi buffer"))
    (magpi-status-toggle-section)))

(defun magpi-status-react ()
  "Open React for the surface at point — Magpi's intervention pane."
  (interactive)
  (let ((target (magpi-status-target-at-point)))
    (unless target
      (user-error "Nothing to react to at point"))
    (unless magpi-status-react-function
      (user-error "No react command is configured for this Magpi buffer"))
    (funcall magpi-status-react-function target)))

(defun magpi-status-create ()
  "Create an intention through the configured orchestration callback."
  (interactive)
  (unless magpi-status-create-function
    (user-error "No intention creation command is configured for this Magpi buffer"))
  (call-interactively magpi-status-create-function))
(defun magpi-status-spawn ()
  "Launch an action through the status buffer's configured command.

When point is inside an action or intention section, pass its typed target so
the owner can retain the shared intention."
  (interactive)
  (if magpi-status-spawn-function
      (funcall magpi-status-spawn-function (magpi-status-target-at-point))
    (user-error "No spawn command is configured for this Magpi buffer")))

(defun magpi-status-bind ()
  "Bind context onto the surface at point."
  (interactive)
  (let ((target (magpi-status-target-at-point)))
    (unless (and target magpi-status-bind-function)
      (user-error "No bind command is available here"))
    (funcall magpi-status-bind-function target)))
(defun magpi-status-changes-action (action)
  "Run changes ACTION for the typed intention or action at point."
  (interactive)
  (let ((target (magpi-status-target-at-point)))
    (unless (and target magpi-status-changes-action-function)
      (user-error "No changes action is available here"))
    (funcall magpi-status-changes-action-function action target)))

(defun magpi-status-changes-open ()
  (interactive)
  (magpi-status-changes-action 'status))

(defun magpi-status-changes-diff ()
  (interactive)
  (magpi-status-changes-action 'diff))

(defun magpi-status-changes-log ()
  (interactive)
  (magpi-status-changes-action 'log))

(defun magpi-status-changes-commit ()
  (interactive)
  (magpi-status-changes-action 'commit))

(defun magpi-status-refresh ()
  "Snapshot transport state, then refresh the current Magpi status buffer."
  (interactive)
  (when magpi-status-prepare-function
    (funcall magpi-status-prepare-function))
  (magit-refresh-buffer))

(defvar-keymap magpi-status-mode-map
  :parent special-mode-map
  "RET" #'magpi-status-visit-at-point
  "TAB" #'magpi-status-toggle-section
  "C-i" #'magpi-status-toggle-section
  "i" #'magpi-status-create
  "@" #'magpi-status-bind
  "s" #'magpi-status-spawn
  "a" #'magpi-status-react
  "m" #'magpi-status-changes-open
  "d" #'magpi-status-changes-diff
  "l" #'magpi-status-changes-log
  "c" #'magpi-status-changes-commit
  "g" #'magpi-status-refresh
  "q" #'quit-window)

(define-derived-mode magpi-status-mode magit-mode "Magpi"
  "Magpi glance composed from Magit sections.  Depth is Magit; React intervenes."
  (setq-local truncate-lines nil)
  (setq-local truncate-partial-width-windows nil)
  (setq-local word-wrap t)
  ;; Magpi does not use Magit margins or diff hunk selection; those hooks
  ;; soft-depend on other Magit libraries and blow up if only magit-mode is loaded.
  (when (boundp 'magit-setup-buffer-hook)
    (setq-local magit-setup-buffer-hook
                (remove 'magit-set-buffer-margins magit-setup-buffer-hook)))
  (when (boundp 'magit-region-highlight-hook)
    (setq-local magit-region-highlight-hook nil)))

(defun magpi-status-open (root actions-function intentions-function
                               visit-function spawn-function
                               &optional prepare-function changes-action-function
                               react-function bind-function
                               create-function)
  "Show ROOT using Magit's transactional status-buffer machinery.

PREPARE-FUNCTION, when non-nil, is the explicit snapshot pull (`g').  Event
paints never call it; orchestration may invoke it once after the buffer opens."
  (let* ((root (file-name-as-directory (expand-file-name root)))
         (name (format "*Magpi:%s*"
                       (file-name-nondirectory (directory-file-name root)))))
    (magit-setup-buffer #'magpi-status-mode nil
      :buffer name
      :directory root
      (magpi-status-root root)
      (magpi-status-actions-function actions-function)
      (magpi-status-intentions-function intentions-function)
      (magpi-status-visit-function visit-function)
      (magpi-status-spawn-function spawn-function)
      (magpi-status-prepare-function prepare-function)
      (magpi-status-changes-action-function changes-action-function)
      (magpi-status-react-function react-function)
      (magpi-status-bind-function bind-function)
      (magpi-status-create-function create-function))))

(provide 'magpi-status)
;;; magpi-status.el ends here
