;;; magpi-status.el --- Magit-backed status projection for Magpi -*- lexical-binding: t; -*-

(require 'magit-mode)
(require 'magit-section)
(require 'magpi-core)
(require 'magpi-launch)

(defvar-local magpi-status-root nil)
(defvar-local magpi-status-attempts-function nil)
(defvar-local magpi-status-visit-function nil)
(defvar-local magpi-status-spawn-function nil)
(defvar-local magpi-status-prepare-function nil)

;;; Faces — information hierarchy
;;
;; Level 0  header          project identity
;; Level 1  attempt heading authored intent or ◯
;; Level 2  state           running / idle / problem (pre-attentive)
;; Level 3  policy meta     authority, effort, model
;; Level 4  keys / files    structure and evidence
;; Level 5  secondary       last response, placeholders

(defface magpi-status-header
  '((t :inherit bold))
  "Project header line (`MAGPI · root')."
  :group 'magpi)

(defface magpi-status-attempt-heading
  '((t :inherit font-lock-function-name-face :weight bold))
  "Primary attempt identity: authored intent, never observed title."
  :group 'magpi)

(defface magpi-status-key
  '((t :inherit font-lock-comment-face))
  "Field labels inside an attempt body."
  :group 'magpi)

(defface magpi-status-intent
  '((t :inherit default :weight bold))
  "Authored intention value."
  :group 'magpi)

(defface magpi-status-title
  '((t :inherit font-lock-string-face))
  "Observed backend title; never heading identity."
  :group 'magpi)

(defface magpi-status-meta
  '((t :inherit shadow))
  "Quiet launch metadata: profile, effort, model."
  :group 'magpi)

(defface magpi-status-model
  '((t :inherit font-lock-type-face))
  "Model identifiers."
  :group 'magpi)

(defface magpi-status-activity-running
  '((t :inherit success :weight bold))
  "Active work indicator."
  :group 'magpi)

(defface magpi-status-activity-starting
  '((t :inherit warning))
  "Startup indicator."
  :group 'magpi)

(defface magpi-status-activity-idle
  '((t :inherit shadow))
  "Idle indicator."
  :group 'magpi)

(defface magpi-status-activity-unknown
  '((t :inherit shadow :slant italic))
  "Unknown activity indicator."
  :group 'magpi)

(defface magpi-status-authority-writer
  '((t :inherit warning :weight bold))
  "Writer authority — elevated rights."
  :group 'magpi)

(defface magpi-status-authority-read-only
  '((t :inherit shadow))
  "Read-only authority."
  :group 'magpi)

(defface magpi-status-problem
  '((t :inherit error :weight bold))
  "Problem and disconnection emphasis."
  :group 'magpi)

(defface magpi-status-file
  '((t :inherit font-lock-string-face))
  "Observed evidence paths."
  :group 'magpi)

(defface magpi-status-file-heading
  '((t :inherit font-lock-comment-face :weight bold))
  "Observed-files section heading."
  :group 'magpi)

(defface magpi-status-last
  '((t :inherit shadow))
  "Collapsed last-response excerpt."
  :group 'magpi)

(defface magpi-status-placeholder
  '((t :inherit shadow))
  "Quiet absence marker (◯)."
  :group 'magpi)

(defface magpi-status-empty
  '((t :inherit shadow :slant italic))
  "Empty-state guidance copy."
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
  (magpi-status--face "◯" 'magpi-status-placeholder))

(defun magpi-status--activity-state-face (state)
  (pcase state
    ('starting 'magpi-status-activity-starting)
    ('running 'magpi-status-activity-running)
    ('idle 'magpi-status-activity-idle)
    ('unknown 'magpi-status-activity-unknown)
    (_ 'magpi-status-activity-unknown)))

(defun magpi-status--activity-state-label (state)
  "Render semantic activity STATE."
  (pcase state
    ('starting "starting")
    ('running "running")
    ('idle "idle")
    ('unknown "unknown")
    (_ "unknown")))

(defun magpi-status--connection-state-label (state)
  "Render semantic connection STATE."
  (pcase state
    ('connected "connected")
    ('disconnected "disconnected")
    (_ "unknown")))

(defun magpi-status--heading (attempt)
  "Return authored intent, or a quiet placeholder.

Observed backend titles never become identity.  They remain a body
observation and must not fill or replace authored intent."
  (if-let ((intent (magpi-attempt-intent attempt)))
      (truncate-string-to-width intent 42 nil nil "…")
    (magpi-status--placeholder)))

(defun magpi-status--activity-detail (activity)
  "Return an optional semantic ACTIVITY label."
  (and (stringp activity) activity))

(defun magpi-status--model-label (requested running)
  "Render requested and RUNNING models without conflating them."
  (cond
   ((and requested running)
    (if (equal requested running)
        running
      (format "%s → %s" requested running)))
   (requested requested)
   (running running)))

(defun magpi-status--format-count (n)
  "Format token count N with a short K/M suffix, or nil when absent."
  (cond
   ((not (numberp n)) nil)
   ((>= n 1000000) (format "%.1fM" (/ n 1000000.0)))
   ((>= n 1000) (format "%.1fk" (/ n 1000.0)))
   (t (number-to-string n))))

(defun magpi-status--format-cost (cost)
  "Format COST dollars without trailing zeros."
  (when (numberp cost)
    (concat "$"
            (string-trim-right
             (string-trim-right (format "%.4f" cost) "0+")
             "\\."))))

(defun magpi-status--usage-label (usage)
  "Render USAGE as a scannable secondary line, or nil when empty."
  (when usage
    (let* ((input (plist-get usage :input))
           (output (plist-get usage :output))
           (cache-read (plist-get usage :cache-read))
           (cache-write (plist-get usage :cache-write))
           (context-tokens (plist-get usage :context-tokens))
           (context-window (plist-get usage :context-window))
           (cost (plist-get usage :cost))
           (token-parts
            (delq nil
                  (list (when-let ((n (magpi-status--format-count input)))
                          (concat "↑" n))
                        (when-let ((n (magpi-status--format-count output)))
                          (concat "↓" n))
                        (when (and (numberp cache-read) (> cache-read 0))
                          (concat "R" (magpi-status--format-count cache-read)))
                        (when (and (numberp cache-write) (> cache-write 0))
                          (concat "W" (magpi-status--format-count cache-write))))))
           (parts (delq nil
                        (list (when token-parts
                                (string-join token-parts " "))
                              (when (or (numberp context-tokens)
                                        (numberp context-window))
                                (format "ctx %s/%s"
                                        (or (magpi-status--format-count
                                             context-tokens)
                                            "?")
                                        (or (magpi-status--format-count
                                             context-window)
                                            "?")))
                              (magpi-status--format-cost cost)))))
      (when parts
        (string-join parts " · ")))))

(defun magpi-status--heading-suffix (attempt)
  "Return scannable activity, effort, and running model meta for ATTEMPT."
  (let* ((launch (magpi-attempt-launch attempt))
         (observation (magpi-attempt-observation attempt))
         (parts (list (magpi-status--activity-state-label
                       (magpi-observation-activity-state observation))
                      (magpi-launch-spec-profile launch)
                      (magpi-launch-thinking-label
                       (magpi-launch-spec-thinking launch))))
         ;; Prefer the live running model in the heading; requested-only is
         ;; launch policy and stays on the body line.
         (running (magpi-observation-running-model observation)))
    (when running
      (setq parts (append parts (list running))))
    (string-join parts " · ")))

(defun magpi-status--format-heading-suffix (attempt)
  "Like `magpi-status--heading-suffix' with hierarchical faces."
  (let* ((launch (magpi-attempt-launch attempt))
         (observation (magpi-attempt-observation attempt))
         (state (magpi-observation-activity-state observation))
         (sep (magpi-status--face " · " 'magpi-status-meta))
         (parts
          (list (magpi-status--face
                 (magpi-status--activity-state-label state)
                 (magpi-status--activity-state-face state))
                (magpi-status--face (magpi-launch-spec-profile launch)
                                    'magpi-status-meta)
                (magpi-status--face
                 (magpi-launch-thinking-label
                  (magpi-launch-spec-thinking launch))
                 'magpi-status-meta)))
         (running (magpi-observation-running-model observation)))
    (when running
      (setq parts
            (append parts
                    (list (magpi-status--face running 'magpi-status-model)))))
    (string-join parts sep)))

(defun magpi-status--insert-kv (key value &optional value-face)
  "Insert a labeled KEY/VALUE row.  VALUE may already be propertized."
  (insert "    "
          (magpi-status--face (format "%-10s" key) 'magpi-status-key)
          " "
          (cond
           ((and (stringp value) (not (string-empty-p value)))
            (if (text-properties-at 0 value)
                value
              (magpi-status--face value (or value-face 'default))))
           (t (magpi-status--placeholder)))
          "\n"))

(defun magpi-status--insert-attempt (attempt)
  "Insert ATTEMPT as a typed, foldable Magit section."
  (let* ((id (magpi-attempt-id attempt))
         (launch (magpi-attempt-launch attempt))
         (observation (magpi-attempt-observation attempt))
         (authority (magpi-launch-spec-authority launch))
         (model (magpi-status--model-label
                 (magpi-launch-spec-requested-model launch)
                 (magpi-observation-running-model observation)))
         (heading (magpi-status--heading attempt))
         (blank-heading (string= (substring-no-properties heading) "◯")))
    (magit-insert-section (magpi-attempt id nil)
      (magit-insert-heading
       (concat (if blank-heading
                   (magpi-status--placeholder)
                 (magpi-status--face heading 'magpi-status-attempt-heading))
               "    "
               (magpi-status--format-heading-suffix attempt)))
      (magpi-status--insert-kv
       "intent"
       (or (magpi-attempt-intent attempt) (magpi-status--placeholder))
       'magpi-status-intent)
      (when-let ((title (magpi-observation-display-title observation)))
        (magpi-status--insert-kv "title" title 'magpi-status-title))
      (magpi-status--insert-kv
       "authority"
       (magpi-launch-authority-label authority)
       (if (eq authority 'writer)
           'magpi-status-authority-writer
         'magpi-status-authority-read-only))
      (magpi-status--insert-kv
       "effort"
       (magpi-launch-thinking-label (magpi-launch-spec-thinking launch))
       'magpi-status-meta)
      (magpi-status--insert-kv
       "model"
       (or model (magpi-status--placeholder))
       'magpi-status-model)
      (when-let ((usage (magpi-status--usage-label
                         (magpi-observation-usage observation))))
        (magpi-status--insert-kv "usage" usage 'magpi-status-meta))
      (when-let ((activity (magpi-status--activity-detail
                            (magpi-observation-activity observation))))
        (magpi-status--insert-kv
         "activity" activity
         (magpi-status--activity-state-face
          (magpi-observation-activity-state observation))))
      (when (eq (magpi-observation-connection-state observation) 'disconnected)
        (magpi-status--insert-kv "connection" "disconnected"
                                 'magpi-status-problem))
      (when-let ((problem (magpi-observation-problem observation)))
        (magpi-status--insert-kv "problem" problem 'magpi-status-problem))
      (when-let ((last (magpi-observation-last-response observation)))
        (magpi-status--insert-kv "last" last 'magpi-status-last))
      (when-let ((files (magpi-observation-observed-files observation)))
        (magit-insert-section (magpi-observed-files id nil)
          (magit-insert-heading
           (magpi-status--face "    observed files" 'magpi-status-file-heading))
          (dolist (file files)
            (magit-insert-section (magpi-observed-file (cons id file) nil)
              (insert "      "
                      (magpi-status--face file 'magpi-status-file)
                      "\n"))))))))

(defun magpi-status-refresh-buffer ()
  "Render the status projection from its read-model function.

Event-driven paints must not reconcile.  A snapshot pull restates transport
facts as events; feeding those back into refresh is a control-plane loop."
  (magit-insert-section (magpi-status magpi-status-root)
    (magit-insert-heading
     (magpi-status--face
      (format "MAGPI · %s"
              (file-name-nondirectory
               (directory-file-name magpi-status-root)))
      'magpi-status-header))
    (let ((attempts (and magpi-status-attempts-function
                         (funcall magpi-status-attempts-function))))
      (if attempts
          (mapc #'magpi-status--insert-attempt attempts)
        (insert
         (magpi-status--face
          "\n  No attempts. Press s to state an intention and spawn.\n"
          'magpi-status-empty))))))

(defun magpi-status-target-at-point ()
  "Return the typed Magpi target at point, or nil."
  (cond
   ((magit-section-value-if 'magpi-observed-file)
    (pcase-let ((`(,attempt-id . ,path)
                 (magit-section-value-if 'magpi-observed-file)))
      (list :kind 'observed-file :attempt-id attempt-id :path path)))
   ((magit-section-value-if 'magpi-attempt)
    (list :kind 'attempt :attempt-id (magit-section-value-if 'magpi-attempt)))))

(defun magpi-status-toggle-section ()
  "Toggle the Magit section at point without exposing unrelated Magit commands."
  (interactive)
  (when-let ((section (magit-current-section)))
    (magit-section-toggle section)))

(defun magpi-status-visit-at-point ()
  "Visit the typed Magpi object at point, or toggle its section."
  (interactive)
  (if-let ((target (magpi-status-target-at-point)))
      (if magpi-status-visit-function
          (funcall magpi-status-visit-function target)
        (user-error "No visitor is configured for this Magpi buffer"))
    (magpi-status-toggle-section)))

(defun magpi-status-spawn ()
  "Launch an attempt through the status buffer's configured command."
  (interactive)
  (if magpi-status-spawn-function
      (call-interactively magpi-status-spawn-function)
    (user-error "No spawn command is configured for this Magpi buffer")))

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
  "s" #'magpi-status-spawn
  "g" #'magpi-status-refresh
  "q" #'quit-window)

(define-derived-mode magpi-status-mode magit-mode "Magpi"
  "Read-only Magpi porcelain composed from Magit sections and refreshes."
  (setq-local truncate-lines t))

(defun magpi-status-open (root attempts-function visit-function spawn-function
                               &optional prepare-function)
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
      (magpi-status-attempts-function attempts-function)
      (magpi-status-visit-function visit-function)
      (magpi-status-spawn-function spawn-function)
      (magpi-status-prepare-function prepare-function))))

(provide 'magpi-status)
;;; magpi-status.el ends here
