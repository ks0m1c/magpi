;;; magpi-launch.el --- Frozen Magpi launch specifications -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'subr-x)

(defgroup magpi nil
  "A lean actionable workbench for Pi sessions."
  :group 'tools)

(defcustom magpi-model-profiles
  '(("Inherit"  :thinking nil)
    ("Quick"    :thinking low)
    ("Standard" :thinking medium)
    ("Deep"     :thinking high))
  "Named launch profiles.
Each entry is (LABEL . PLIST).  A profile resolves before an attempt starts."
  :type '(repeat (cons string plist))
  :group 'magpi)

(defcustom magpi-default-profile "Standard"
  "Default Magpi launch profile."
  :type 'string
  :group 'magpi)

(defcustom magpi-default-authority "Read-only"
  "Default launch authority, either Writer or Read-only."
  :type '(choice (const "Writer") (const "Read-only"))
  :group 'magpi)

(cl-defstruct magpi-launch-spec
  id name root intent profile model thinking authority context flags first-message)

(defun magpi-launch-name (intent id)
  "Return the stable display name for INTENT and launch ID."
  (let ((intent (string-trim
                 (replace-regexp-in-string "[\n\r\t ]+" " " intent))))
    (format "%s · %s"
            (truncate-string-to-width (if (string-empty-p intent) "◯" intent)
                                      42 nil nil "…")
            (substring id 0 4))))

(defun magpi-launch-profile (name)
  "Return the settings for profile NAME, or reject an unknown profile."
  (or (cdr (assoc name magpi-model-profiles))
      (user-error "Unknown Magpi profile: %s" name)))

(defun magpi-launch-first-message (intent context authority)
  "Render the frozen first message, or nil when no intention was supplied."
  (unless (string-empty-p intent)
    (concat
     intent
     (unless (eq (plist-get context :kind) 'none)
       (format "\n\nContext captured at dispatch:\n- %s%s%s"
               (or (plist-get context :file) "buffer")
               (if-let ((line (plist-get context :line)))
                   (format ":%d" line)
                 "")
               (if-let ((text (plist-get context :text)))
                   (format "\n\n%s" text)
                 "")))
     (format "\n\nAuthority: %s." authority))))

(defun magpi-launch-build (id root intent profile authority context)
  "Resolve one immutable launch specification.

ID, ROOT, INTENT, PROFILE, AUTHORITY, and CONTEXT are captured before a
backend is called.  An empty INTENT is represented explicitly, not invented.
The backend may later supply a title for that absence."
  (setq intent (string-trim (or intent "")))
  (let* ((settings (magpi-launch-profile profile))
         (resolved-model (plist-get settings :model))
         (thinking (plist-get settings :thinking))
         (flags (append (and resolved-model (list "--model" resolved-model))
                        (and thinking (list "--thinking" (symbol-name thinking)))
                        (and (equal authority "Read-only")
                             '("--tools" "read,grep,find,ls")))))
    (make-magpi-launch-spec
     :id id :name (magpi-launch-name intent id) :root root :intent intent
     :profile profile :model resolved-model :thinking thinking
     :authority authority :context context :flags flags
     :first-message (magpi-launch-first-message intent context authority))))

(provide 'magpi-launch)
;;; magpi-launch.el ends here
