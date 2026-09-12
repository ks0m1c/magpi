;;; magpi-launch.el --- Frozen launch: model, thinking, role, source -*- lexical-binding: t; -*-

;; Copyright (C) 2026 ks0m1c_dharma
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is part of Magpi.

;; One job: freeze a semantic launch specification and remember launch
;; defaults.  Catalog fill is registered by an adapter; this file does not
;; speak Pi or Pimacs.  Launch-local source is not an Intention `@' binding.

(require 'cl-lib)
(require 'project)
(require 'seq)
(require 'subr-x)

(defgroup magpi nil
  "Magpi: freeze launch (model, thinking, role, source) for one action."
  :group 'tools)

(defconst magpi-launch-thinking-values
  '(nil off minimal low medium high xhigh max)
  "Semantic thinking depths a launch specification may freeze.")

(defcustom magpi-launch-thinking-choices
  '(("Default"  . nil)
    ("Quick"    . low)
    ("Standard" . medium)
    ("Deep"     . high)
    ("Off"      . off))
  "Launch labels for thinking depth.  One axis; later richness extends this list."
  :type '(repeat (cons string sexp))
  :group 'magpi)

(defcustom magpi-default-thinking 'medium
  "Default semantic thinking depth."
  :type '(choice (const :tag "Default" nil)
                 (const off)
                 (const minimal)
                 (const low)
                 (const medium)
                 (const high)
                 (const xhigh)
                 (const max))
  :group 'magpi)

(defcustom magpi-launch-models
  '("Inherit")
  "Canonical model identifiers offered at launch.
Use the literal \"Inherit\" (or a blank value) to leave requested-model nil.
Entries should be opaque provider/model identifiers such as
\"anthropic/claude-sonnet\".  Completing-read still accepts any well-formed
identifier not listed here."
  :type '(repeat string)
  :group 'magpi)

(defcustom magpi-default-model "Inherit"
  "Default launch model label.  \"Inherit\" means requested-model is nil.
A remembered last-used model for the project root overrides this."
  :type 'string
  :group 'magpi)

(defcustom magpi-default-role 'writer
  "Default semantic launch role: `w' (writer) or `r' (reader)."
  :type '(choice (const :tag "r · Reader" reader)
                 (const :tag "w · Writer" writer))
  :group 'magpi)

(defvar magpi-launch--catalog (make-hash-table :test #'equal)
  "Cached canonical model ids by project-root key.")

(defvar magpi-launch--last-model (make-hash-table :test #'equal)
  "Last requested canonical model id by project-root key.")

(defvar magpi-launch-catalog-refresh-function nil
  "Optional function (ROOT &optional HANDLE) that refreshes the model catalog.

It must not wait.  A live reply may store the catalog later.  A synchronous
fill should call `magpi-launch-store-catalog' before returning.")

(cl-defstruct magpi-launch-spec
  root requested-model thinking role context)

(defun magpi-normalize-objective (objective)
  "Return authored OBJECTIVE as text or nil when it is blank.
This is the sole normalization point for input collected at dispatch."
  (let ((normalized (and (stringp objective) (string-trim objective))))
    (unless (or (null normalized) (string-empty-p normalized)) normalized)))

(defun magpi-normalize-model (model)
  "Return canonical MODEL identifier or nil when inheriting/blank.
Accepts the UI label \"Inherit\" as nil."
  (let ((normalized (and (stringp model) (string-trim model))))
    (cond
     ((or (null normalized) (string-empty-p normalized)) nil)
     ((member (downcase normalized) '("inherit" "default" "none" "◯")) nil)
     (t normalized))))

(defun magpi-launch--fold-model (string)
  "Fold STRING for fuzzy model search (lowercase, drop non-alphanumerics)."
  (replace-regexp-in-string "[^a-z0-9]+" "" (downcase (or string ""))))

(defun magpi-launch--model-query-matches-p (query candidate)
  "Return non-nil when QUERY fuzzily matches CANDIDATE after folding."
  (let* ((q (magpi-launch--fold-model query))
         (c (magpi-launch--fold-model candidate)))
    (cond
     ((string-empty-p q) t)
     ((string-search q c) t)
     (t
      (let ((i 0)
            (n (length c)))
        (catch 'magpi-launch--no-match
          (dotimes (qi (length q))
            (let ((ch (aref q qi)))
              (while (and (< i n) (not (eq (aref c i) ch)))
                (setq i (1+ i)))
              (when (>= i n)
                (throw 'magpi-launch--no-match nil))
              (setq i (1+ i))))
          t))))))

(defun magpi-launch--filter-models (query candidates &optional pred)
  "Return CANDIDATES matching QUERY under `magpi-launch--model-query-matches-p'."
  (seq-filter
   (lambda (candidate)
     (and (or (null pred) (funcall pred candidate))
          (magpi-launch--model-query-matches-p query candidate)))
   candidates))

(defun magpi-launch-model-completion-table (&optional root)
  "Completion table for ROOT's models with normalized fuzzy search."
  (let ((candidates (magpi-launch-model-choices root)))
    (lambda (string pred action)
      (pcase action
        ('metadata
         '(metadata (category . magpi-model)
                    (display-sort-function . identity)
                    (cycle-sort-function . identity)))
        ('t
         (magpi-launch--filter-models string candidates pred))
        ('lambda
         (test-completion string candidates pred))
        ('nil
         (try-completion string
                         (magpi-launch--filter-models string candidates pred)
                         pred))
        (_
         (complete-with-action action candidates string pred))))))

(defun magpi-launch-current-root ()
  "Return the project root that launch choices should remember against."
  (file-name-as-directory
   (expand-file-name
    (if-let ((project (project-current)))
        (project-root project)
      default-directory))))

(defun magpi-launch--root-key (root)
  (file-truename (file-name-as-directory (expand-file-name (or root default-directory)))))

(defun magpi-launch-cached-models (&optional root)
  "Return cached canonical model ids for ROOT, or nil."
  (gethash (magpi-launch--root-key root) magpi-launch--catalog))

(defun magpi-launch-store-catalog (root models)
  "Remember MODELS as the launch catalog for ROOT.

Empty or malformed replies leave the previous catalog in place."
  (let ((ids (delq nil (mapcar #'magpi-normalize-model models))))
    (when ids
      (puthash (magpi-launch--root-key root) (seq-uniq ids) magpi-launch--catalog)
      ids)))

(defun magpi-launch-refresh-catalog (&optional root handle)
  "Refresh ROOT's model catalog without waiting.

A registered filler may store models now or when a live reply arrives.
Returns whatever is already cached."
  (when magpi-launch-catalog-refresh-function
    (funcall magpi-launch-catalog-refresh-function
             (or root (magpi-launch-current-root))
             handle))
  (magpi-launch-cached-models root))

(defun magpi-launch-last-model (&optional root)
  "Return the last requested canonical model for ROOT, or nil."
  (gethash (magpi-launch--root-key root) magpi-launch--last-model))

(defun magpi-launch-remember-model (root model)
  "Remember MODEL as the next launch default for ROOT.

Inherit/blank does not erase a previous explicit choice."
  (when-let ((id (magpi-normalize-model model)))
    (puthash (magpi-launch--root-key root) id magpi-launch--last-model)
    id))

(defun magpi-launch-default-model (&optional root)
  "Return the model label to offer first at launch for ROOT."
  (or (magpi-launch-last-model root) magpi-default-model))

(defun magpi-launch-model-choices (&optional root)
  "Return completing-read candidates for ROOT's launch model."
  (let* ((root (magpi-launch--root-key root))
         (last (gethash root magpi-launch--last-model))
         (cached (gethash root magpi-launch--catalog)))
    (seq-uniq
     (delq nil
           (append (list "Inherit" last)
                   magpi-launch-models
                   cached)))))

(defun magpi-launch-role-label (role)
  "Return the transient label for semantic ROLE, or reject it."
  (pcase role
    ('reader "r")
    ('writer "w")
    (_ (user-error "Unknown Magpi role: %S" role))))

(defun magpi-launch-role-from-label (label)
  "Translate a compact `r' or `w' LABEL to semantic role."
  (pcase label
    ((or "r" "Reader" "Read-only") 'reader)
    ((or "w" "Writer") 'writer)
    (_ (user-error "Unknown Magpi role label: %s" label))))

(defun magpi-launch-bind-from-label (label)
  "Translate launch-local source LABEL to a semantic kind, or reject it."
  (pcase label
    ("None" 'none)
    ("Point" 'point)
    ("Region" 'region)
    (_ (user-error "Unknown Magpi source label: %s" label))))

(defun magpi-launch-bind-label (kind)
  "Return the transient label for launch-local source KIND."
  (pcase kind
    ('none "None")
    ('point "Point")
    ('region "Region")
    (_ (user-error "Unknown Magpi source kind: %S" kind))))

(defun magpi-launch-source-buffer-p ()
  "Return non-nil when the current buffer can be frozen as file evidence.

Magpi status and Pimacs chat are porcelain, not source."
  (and (stringp buffer-file-name)
       (not (string-empty-p buffer-file-name))
       (not (derived-mode-p 'magpi-status-mode 'pimacs-chat-mode))))

(defun magpi-launch-default-bind ()
  "Default launch-local source: None unless an active region is source evidence."
  (if (and (use-region-p) (magpi-launch-source-buffer-p))
      'region
    'none))

(defun magpi-launch-thinking-label (thinking)
  "Return a scannable label for semantic THINKING."
  (pcase thinking
    ('nil "default")
    ('off "off")
    ('minimal "minimal")
    ('low "low")
    ('medium "medium")
    ('high "high")
    ('xhigh "xhigh")
    ('max "max")
    (_ (format "%s" thinking))))

(defun magpi-launch-thinking-choice-label (thinking)
  "Return the launch-menu label for THINKING."
  (or (car (rassq thinking magpi-launch-thinking-choices))
      (magpi-launch-thinking-label thinking)))

(defun magpi-launch-thinking-from-label (label)
  "Translate launch-menu LABEL to a semantic thinking value."
  (let* ((label (or label
                    (magpi-launch-thinking-choice-label magpi-default-thinking)))
         (entry (assoc label magpi-launch-thinking-choices)))
    (unless entry
      (user-error "Unknown Magpi thinking: %s" label))
    (cdr entry)))

(defun magpi-launch-context-title (context)
  "Return a concise fallback chat title derived from frozen CONTEXT."
  (let ((file (plist-get context :file))
        (line (plist-get context :line)))
    (cond
     ((and (stringp file) line) (format "%s:%s" file line))
     ((stringp file) file)
     ((eq (plist-get context :kind) 'region) "Selection")
     ((eq (plist-get context :kind) 'point) "Current location")
     (t "New chat"))))

(defun magpi-launch-compose-first-message (prompt context)
  "Compose first-message text from PROMPT and launch-local CONTEXT.

PROMPT is adapter/programmatic task text, or nil when the user will
author in chat.  CONTEXT is spawn-local source (`point', `region', or
`none'), never an Intention `@' binding.

A normal spawn leaves PROMPT nil; captured source still becomes the
first message.  Neither a blank prompt nor `none' source is sent."
  (let* ((prompt (magpi-normalize-objective prompt))
         (kind (plist-get context :kind))
         (source
          (and context
               (not (memq kind '(none nil)))
               (format "Context captured at dispatch:\n- %s%s%s"
                       (or (plist-get context :file) "buffer")
                       (if-let ((line (plist-get context :line)))
                           (format ":%d" line)
                         "")
                       (if-let ((text (plist-get context :text)))
                           (format "\n\n%s" text)
                         "")))))
    (cond
     ((and prompt source) (concat prompt "\n\n" source))
     (prompt prompt)
     (source source))))

(defun magpi-launch-build (root thinking role context &optional requested-model)
  "Resolve one immutable launch specification.

ROOT, THINKING, ROLE, and CONTEXT are captured before an adapter is called.
CONTEXT is launch-local source, not a durable `@' reference.
THINKING is a value from `magpi-launch-thinking-values'.
REQUESTED-MODEL is a canonical provider/model id or nil (inherit).
Authored intent belongs exclusively to the action, not this configuration."
  (unless (memq thinking magpi-launch-thinking-values)
    (user-error "Unknown Magpi thinking: %S" thinking))
  (unless (memq role '(reader writer))
    (user-error "Unknown Magpi role: %S" role))
  (unless (memq (plist-get context :kind) '(none point region))
    (user-error "Unknown Magpi source kind: %S" (plist-get context :kind)))
  (let ((model (magpi-normalize-model requested-model)))
    (when (and model
               (not (string-match-p "\\`[^/]+/.+\\\'" model)))
      (user-error "Model must be a provider/model identifier: %s" model))
    (make-magpi-launch-spec
     :root root
     :requested-model model
     :thinking thinking
     :role role
     :context context)))

(provide 'magpi-launch)
;;; magpi-launch.el ends here
