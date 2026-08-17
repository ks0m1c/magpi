;;; magpi-launch.el --- Frozen Magpi launch specifications -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'subr-x)

(defgroup magpi nil
  "A lean actionable workbench for Pi sessions."
  :group 'tools)

(defcustom magpi-launch-profiles
  '(("Default"  :thinking nil)
    ("Quick"    :thinking low)
    ("Standard" :thinking medium)
    ("Deep"     :thinking high)
    ("Off"      :thinking off)
    ("Minimal"  :thinking minimal)
    ("Low"      :thinking low)
    ("Medium"   :thinking medium)
    ("High"     :thinking high)
    ("Xhigh"    :thinking xhigh)
    ("Max"      :thinking max))
  "Named effort/thinking profiles.
Each entry is (LABEL . PLIST).  Profiles select thinking depth; an explicit
effort choice at dispatch overrides the profile's thinking.  Requested model is
independent and may be nil (inherit)."
  :type '(repeat (cons string plist))
  :group 'magpi)

(defcustom magpi-default-profile "Standard"
  "Default Magpi launch profile."
  :type 'string
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
  "Default launch model label.  \"Inherit\" means requested-model is nil."
  :type 'string
  :group 'magpi)

(defcustom magpi-launch-efforts
  '(("Profile"  . profile)
    ("Default"  . none)
    ("Off"      . off)
    ("Minimal"  . minimal)
    ("Low"      . low)
    ("Medium"   . medium)
    ("High"     . high)
    ("Xhigh"    . xhigh)
    ("Max"      . max))
  "Effort labels mapped to semantic thinking values.
`profile' defers to the selected profile.  `none' means pass no thinking flag."
  :type '(repeat (cons string symbol))
  :group 'magpi)

(defcustom magpi-default-effort "Profile"
  "Default effort label.  \"Profile\" defers to the selected profile."
  :type 'string
  :group 'magpi)

(defcustom magpi-default-authority 'writer
  "Default semantic launch authority."
  :type '(choice (const :tag "Read-only" read-only)
                 (const :tag "Writer" writer))
  :group 'magpi)

(cl-defstruct magpi-launch-spec
  root profile requested-model thinking authority context)

(defun magpi-normalize-intent (intent)
  "Return authored INTENT as text or nil when it is blank.
This is the sole normalization point for input collected at dispatch."
  (let ((normalized (and (stringp intent) (string-trim intent))))
    (unless (or (null normalized) (string-empty-p normalized)) normalized)))

(defun magpi-normalize-model (model)
  "Return canonical MODEL identifier or nil when inheriting/blank.
Accepts the UI label \"Inherit\" as nil."
  (let ((normalized (and (stringp model) (string-trim model))))
    (cond
     ((or (null normalized) (string-empty-p normalized)) nil)
     ((member (downcase normalized) '("inherit" "default" "none" "◯")) nil)
     (t normalized))))

(defun magpi-launch-authority-label (authority)
  "Return the transient label for semantic AUTHORITY, or reject it."
  (pcase authority
    ('read-only "Read-only")
    ('writer "Writer")
    (_ (user-error "Unknown Magpi authority: %S" authority))))

(defun magpi-launch-authority-from-label (label)
  "Translate transient LABEL to a semantic authority, or reject it."
  (pcase label
    ("Read-only" 'read-only)
    ("Writer" 'writer)
    (_ (user-error "Unknown Magpi authority label: %s" label))))

(defun magpi-launch-context-kind-from-label (label)
  "Translate transient LABEL to a semantic context kind, or reject it."
  (pcase label
    ("None" 'none)
    ("Point" 'point)
    ("Region" 'region)
    (_ (user-error "Unknown Magpi context label: %s" label))))

(defun magpi-launch-context-kind-label (kind)
  "Return the transient label for semantic context KIND."
  (pcase kind
    ('none "None")
    ('point "Point")
    ('region "Region")
    (_ (user-error "Unknown Magpi context kind: %S" kind))))

(defun magpi-launch-source-buffer-p ()
  "Return non-nil when the current buffer can be frozen as file evidence.

Magpi status and Pimacs chat are porcelain, not source.  Capturing them as
Point/Region context feeds the workbench back into the agent prompt."
  (and (stringp buffer-file-name)
       (not (string-empty-p buffer-file-name))
       (not (derived-mode-p 'magpi-status-mode 'pimacs-chat-mode))))

(defun magpi-launch-default-context-kind ()
  "Choose a context kind that cannot capture workbench porcelain as evidence."
  (cond
   ((and (use-region-p) (magpi-launch-source-buffer-p)) 'region)
   ((magpi-launch-source-buffer-p) 'point)
   (t 'none)))

(defun magpi-launch-profile (name)
  "Return the settings for profile NAME, or reject an unknown profile."
  (or (cdr (assoc name magpi-launch-profiles))
      (user-error "Unknown Magpi profile: %s" name)))

(defun magpi-launch-effort-from-label (label)
  "Translate effort LABEL to a thinking value or the symbol `profile'.
nil means pass no explicit thinking flag (provider default)."
  (let* ((label (or label magpi-default-effort))
         (entry (assoc label magpi-launch-efforts)))
    (unless entry
      (user-error "Unknown Magpi effort label: %s" label))
    (cdr entry)))

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

(defun magpi-launch-build (root profile authority context &optional requested-model effort)
  "Resolve one immutable launch specification.

ROOT, PROFILE, AUTHORITY, and CONTEXT are captured before a backend is called.
REQUESTED-MODEL is a canonical provider/model id or nil (inherit).
EFFORT is a thinking symbol, `profile' (use PROFILE's thinking), or `none'
(no thinking flag).  Omitted EFFORT means `profile'.
Authored intent belongs exclusively to the attempt, not this configuration."
  (unless (memq authority '(read-only writer))
    (user-error "Unknown Magpi authority: %S" authority))
  (unless (memq (plist-get context :kind) '(none point region))
    (user-error "Unknown Magpi context kind: %S" (plist-get context :kind)))
  (let* ((settings (magpi-launch-profile profile))
         ;; Omitted EFFORT defaults to `profile' so legacy callers keep
         ;; resolving thinking from the named profile.  `none' freezes
         ;; thinking as nil (no transport flag).
         (effort (if (eq effort nil) 'profile effort))
         (thinking (pcase effort
                     ('profile (plist-get settings :thinking))
                     ('none nil)
                     (_ effort)))
         (model (magpi-normalize-model requested-model)))
    (when (and model
               (not (string-match-p "\\`[^/]+/.+\\'" model)))
      (user-error "Model must be a provider/model identifier: %s" model))
    (make-magpi-launch-spec
     :root root :profile profile
     :requested-model model
     :thinking thinking
     :authority authority :context context)))

(provide 'magpi-launch)
;;; magpi-launch.el ends here
