;;; magpi-status.el --- Glance: intention, action, ask marks -*- lexical-binding: t; -*-

;; Copyright (C) 2026 ks0m1c_dharma
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is part of Magpi.

;; One job: glance surfaces as typed Magit sections.  Verbs live in magpi.el.
;; Effects consume semantic magpi-section-* selectors; facet (TYPE . VALUE) is
;; glance-side only.  Magit: the section is the subject.  Lineage is parent.
;; Magit section classes and per-type keymaps remain a later prior.

(require 'cl-lib)
(require 'color)
(require 'eieio)
(require 'magit-mode)
(require 'magit-section)
(require 'magpi-action)
(require 'magpi-launch)
(require 'magpi-intention)
(autoload 'magpi-spawn "magpi" nil t)
(autoload 'magpi-bind "magpi" nil t)
(autoload 'magpi-react "magpi" nil t)
(autoload 'magpi-discard "magpi" nil t)
(autoload 'magpi-visit "magpi" nil t)
(autoload 'magpi-visit-worktree "magpi" nil t)
(autoload 'magpi-intention-create "magpi" nil t)
(autoload 'magpi-changes-status "magpi" nil t)
(autoload 'magpi-changes-diff "magpi" nil t)
(autoload 'magpi-changes-log "magpi" nil t)
(autoload 'magpi-changes-commit "magpi" nil t)

(defvar magpi--actions)
(defvar magpi--intentions)
(defvar-local magpi-status-root nil)
(defvar-local magpi-status-origin nil
  "Invoking checkout whose attached HEAD culls glance membership.")
(defvar-local magpi-status-ref nil
  "Membership branch.  Default is checkout HEAD; `b' peeks.  `g' restores HEAD.")
(defvar-local magpi-status-head nil
  "Snapshotted attached HEAD of `magpi-status-origin'.  Paint never refreshes it.")
(defvar-local magpi-status-actions-function nil)
(defvar-local magpi-status-intentions-function nil)
(defvar-local magpi-status-prepare-function nil)
(defvar-local magpi-status-git-facts nil
  "Intention-id → Git facts.  Filled by `g' and first open; event paint reuses it.")
(defvar-local magpi-status-actions nil
  "Prepared Action sample.  Disk membership is snapshot; this Emacs's new members are admitted.")
(defvar-local magpi-status-intentions nil
  "Prepared Intention sample.  Disk membership is snapshot; this Emacs's new members are admitted.")
(defvar-local magpi-status-action-title-function nil)
(defvar-local magpi-status-action-loading-function nil)
(defvar-local magpi-status-layout-width nil
  "Hidden-buffer width override.  A visible window always wins and cannot be overstated.")
(defvar magpi-status--paint-frame nil
  "Explicit motion frame for one heading paint.  Nil is the still cast.")
(defvar magpi-status--hours (make-hash-table :test #'equal)
  "Action id → last sampled auspice.  Theatre only; never persisted.")
(defvar magpi-status--films (make-hash-table :test #'equal)
  "Action id → (TO INDEX FRAMES TIMER).  Disposable overlay.")
(defcustom magpi-status-title-min-width 18
  "Minimum title seat width in a status heading."
  :type 'integer
  :group 'magpi)

(defcustom magpi-status-title-max-width 52
  "Maximum title seat width in a status heading."
  :type 'integer
  :group 'magpi)

(defcustom magpi-status-reduced-motion nil
  "When non-nil, suppress every later lifecycle-edge overlay.
The still frame stays legible.  History `load.' never moves."
  :type 'boolean
  :group 'magpi)

(defconst magpi-status-effort-width 7
  "Thinking seat width.  `minimal' and `default' fill it.")

(defconst magpi-status-context-width 10
  "Typical compact context form (`9k/200k').  The seat is the fact's width.")

(defconst magpi-status-age-width 4
  "Typical age form (`now', `12h', `3d').  The seat is the fact's width.")

(defconst magpi-status-action-indent 2
  "Nested Action heading inset under an Intention.  Standalone stays flush.")
(defconst magpi-status--omen-overhead 10
  "Attention 2, two separators, motion 5, retention 1.")

(defconst magpi-status--hard-min 11
  "Below this, only an atomic attention-plus-title summary fits.")

(defconst magpi-status--model-min 8
  "Narrowest admitted model seat before the whole seat is omitted.")

(cl-defstruct (magpi-status-heading-view
               (:constructor magpi-status--make-heading-view)
               (:copier nil))
  "Ephemeral semantic projection.  Named seats, never a bag or paint.
Omen: attention, title, motion, retention.  Quiet: lease through age.
Values stay semantic until the renderer selects casts and faces."
  attention title title-kind motion retention
  lease history git model effort context age)
;;; Faces — Magit cues.  Perch is a section heading; the repo is Head.
(defgroup magpi-faces nil
  "Faces in Magpi."
  :group 'magpi)

(defface magpi-status-identity
  '((t :inherit default :weight bold))
  "Authored why and user text.  Theme foreground, bold."
  :group 'magpi-faces)

(defface magpi-status-header
  '((t :inherit magit-section-heading))
  "Storehouse labels (`Perch', `Cold').  Same cue as Magit's Staged changes."
  :group 'magpi-faces)

(defface magpi-status-repo
  '((t :inherit magit-branch-local))
  "Repository name on the MAGPI line.  Same cue as Magit's Head branch."
  :group 'magpi-faces)

(defface magpi-status-title
  '((t :inherit font-lock-string-face :weight normal))
  "Action title.  Theme string, not the authored why."
  :group 'magpi-faces)

(defface magpi-status-live
  '((t :inherit success :weight bold))
  "Thought aloft.  Theme success."
  :group 'magpi-faces)

(defface magpi-status-pending
  '((t :inherit warning))
  "Lift or pending judgment.  Theme warning."
  :group 'magpi-faces)

(defface magpi-status-quiet
  '((t :inherit default))
  "Receding support: age, keys, last reply.  Shaded toward background."
  :group 'magpi-faces)

(defface magpi-status-alert
  '((t :inherit error :weight bold))
  "Problem or disconnection.  Theme error."
  :group 'magpi-faces)

(defface magpi-status-evidence
  '((t :inherit link :underline nil))
  "Bound context and observed paths.  Theme link, no underline."
  :group 'magpi-faces)

(defun magpi-status--mix (fg bg frac)
  "Mix FG toward BG by FRAC (0 = FG, 1 = BG).  Named or hex colors."
  (let ((from (and (stringp fg) (color-name-to-rgb fg)))
        (to (and (stringp bg) (color-name-to-rgb bg)))
        (frac (min 1.0 (max 0.0 frac))))
    (when (and from to)
      (apply #'color-rgb-to-hex
             (append (cl-mapcar (lambda (a b)
                                  (+ (* a (- 1.0 frac)) (* b frac)))
                                from to)
                     '(2))))))

(defun magpi-status--recede (frac)
  "Theme default foreground mixed toward background by FRAC."
  (magpi-status--mix
   (face-foreground 'default nil t)
   (or (face-background 'default nil t)
       (frame-parameter nil 'background-color))
   frac))

(defvar-local magpi-status--shade-cookies nil)

(defun magpi-status--apply-shades ()
  "Buffer-local receding shade of the current theme's default."
  (dolist (cookie magpi-status--shade-cookies)
    (face-remap-remove-relative cookie))
  (setq magpi-status--shade-cookies
        (delq nil
              (list
               (when-let ((color (magpi-status--recede 0.55)))
                 (face-remap-add-relative 'magpi-status-quiet :foreground color))))))

(defun magpi-status--apply-shades-to-buffers (&rest _)
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (derived-mode-p 'magpi-status-mode)
        (magpi-status--apply-shades)))))

(defun magpi-status--face (text face)
  "Return TEXT with FACE, or TEXT unchanged when blank.

Magit-section enables font-lock with empty keywords, so a `face'-only
property paints once and is then stripped.  Set both `face' and
`font-lock-face', matching Magit's own heading helpers."
  (if (and (stringp text) (not (string-empty-p text)) face)
      (propertize text 'face face 'font-lock-face face)
    text))

(defun magpi-status--heading-cell (text face)
  (magpi-status--face
   text
   (and face (append (if (listp face) face (list face)) '(fixed-pitch)))))

(defconst magpi-status-motion-width 5
  "Still motion seat width.  Animation does not change it.")

(defconst magpi-status-attention-width 2
  "Attention mark plus its quiet separating column.")

(defconst magpi-status-retention-width 1
  "Muninn retention seat width.")

(defconst magpi-status-retention-cast
  '((empty . "·")
    (bound . "◌")
    (hears . "◉"))
  "Still Muninn cast: empty sky, context held, evidence returned.")

(defconst magpi-status-retention-fallback
  '((empty . ".")
    (bound . "o")
    (hears . "@"))
  "One-column terminal fallback for the Muninn cast.")

(defconst magpi-status-group-cast
  '((perch . "⊤")
    (cold . "⊓"))
  "One-column storehouse marks.  ⊤ is a roost bar; ⊓ is a still bench.
Words remain; the mark is lineage, not a second name.")

(defun magpi-status--group-heading (group)
  "Storehouse heading.  Perch is the living roost; Cold is the still bench."
  (let* ((word (pcase group
                 ('perch "Perch")
                 ('cold "Cold")
                 (_ (capitalize (symbol-name group)))))
         (mark (alist-get group magpi-status-group-cast)))
    (magpi-status--heading-cell
     (if (magpi-status--glyph-displayable-p mark 1)
         (concat mark " " word)
       word)
     'magpi-status-header)))

(defconst magpi-status-still-cast
  '((cold . "·····")
    (lift . "◈····")
    (aloft . "·ˋ◈ˊ·")
    (rest . "ˏˋ◇ˎˊ")
    (blood . "  *  ")
    (empty . "·····")
    (quiescent . "·····"))
  "Still 5-column motion.  Ladder seats 1–4 are flight, not hours.
Blood is centered * — not a ladder seat.")

(defun magpi-status--auspice-face (auspice)
  "Return the semantic face for AUSPICE.  No custom background."
  (pcase auspice
    ('lift 'magpi-status-pending)
    ('aloft 'magpi-status-live)
    ('blood 'magpi-status-alert)
    (_ 'magpi-status-quiet)))

(defun magpi-status--auspice-word (auspice)
  "Five-column fallback hour when the still cast will not hold."
  (let ((word (pcase auspice
                ('quiescent "still")
                ((or 'cold 'lift 'aloft 'rest 'blood 'empty)
                 (symbol-name auspice))
                (_ "cold"))))
    (format "%-5s" word)))

(defun magpi-status--glyph-displayable-p (s width)
  (and (stringp s)
       (= (string-width s) width)
       (not (seq-some (lambda (ch) (not (char-displayable-p ch))) s))))

(defun magpi-status--cast-displayable-p (s)
  (and (= (length s) magpi-status-motion-width)
       (magpi-status--glyph-displayable-p s magpi-status-motion-width)))

(defun magpi-status--auspice-motion (auspice &optional frame)
  "Still motion seat for AUSPICE, or FRAME when it holds the seat."
  (if (and (stringp frame) (magpi-status--cast-displayable-p frame))
      frame
    (let ((glyph (alist-get auspice magpi-status-still-cast)))
      (if (magpi-status--cast-displayable-p glyph)
          glyph
        (magpi-status--auspice-word auspice)))))

(defconst magpi-status-lift-aloft-film
  '("‧◈‧··" "·ˋ◈ˊ·" "··‧◈‧" "····◈" "·····" "·ˋ◈ˊ·")
  "Lift → aloft.  Last frame is the aloft still.  Not lift's still.")

(defconst magpi-status-aloft-rest-film
  '("·ˋ◇ˊ·" "ˏˋ◇ˎˊ")
  "Aloft → rest, one way.  Last frame is the rest still.")

(defconst magpi-status--sky-dwell 0.14
  "Seconds between lift → aloft frames.")

(defconst magpi-status--settle-dwell 0.07
  "Seconds between aloft → rest frames.")

(defun magpi-status--film-displayable-p (frames)
  (and (consp frames)
       (seq-every-p #'magpi-status--cast-displayable-p frames)))

(defun magpi-status--film-for (from to)
  (pcase (list from to)
    (`(lift aloft) magpi-status-lift-aloft-film)
    (`(aloft rest) magpi-status-aloft-rest-film)))

(defun magpi-status--motion-alive-p (id)
  (and (boundp 'magpi--actions)
       (hash-table-p magpi--actions)
       (magpi-action-p (gethash id magpi--actions))))

(defun magpi-status--motion-frame (id)
  (when-let ((film (gethash id magpi-status--films)))
    (nth (nth 1 film) (nth 2 film))))

(defun magpi-status--motion-cancel (id)
  (when-let ((film (gethash id magpi-status--films)))
    (when (timerp (nth 3 film))
      (cancel-timer (nth 3 film))))
  (remhash id magpi-status--films))

(defun magpi-status--motion-cancel-all ()
  (maphash (lambda (id _) (magpi-status--motion-cancel id))
           magpi-status--films))

(defun magpi-status--motion-sweep ()
  "Drop overlays whose Action is gone.  No timer outlives its Action."
  (maphash (lambda (id _)
             (unless (magpi-status--motion-alive-p id)
               (magpi-status--motion-cancel id)))
           magpi-status--films))

(defun magpi-status--motion-repaint ()
  "Presentation-only.  Never `g' or Git.  Does not start a film."
  (dolist (buffer (magpi-status--visible-status-buffers))
    (with-current-buffer buffer
      (magit-refresh-buffer))))

(defun magpi-status--motion-start (id to frames dwell)
  (magpi-status--motion-cancel id)
  (let ((film (list to 0 frames nil)))
    (puthash id film magpi-status--films)
    (setcar (nthcdr 3 film)
            (run-at-time dwell dwell #'magpi-status--motion-tick id))))

(defun magpi-status--motion-tick (id)
  (let ((film (gethash id magpi-status--films)))
    (cond
     ((or (null film)
          magpi-status-reduced-motion
          (not (magpi-status--motion-alive-p id)))
      (magpi-status--motion-cancel id)
      (when film (magpi-status--motion-repaint)))
     (t
      (let ((next (1+ (nth 1 film)))
            (frames (nth 2 film)))
        (if (>= next (length frames))
            (progn
              (magpi-status--motion-cancel id)
              (magpi-status--motion-repaint))
          (setcar (nthcdr 1 film) next)
          (magpi-status--motion-repaint)))))))

(defun magpi-status--motion-sync (id hour)
  "Return the overlay frame for ID at HOUR, or nil for the still cast.

Remembers HOUR.  Starts a film only on lift → aloft or aloft → rest.
Blood and reduced motion cancel.  A playing film whose target is HOUR
continues.  Nil frame is the still cast."
  (let ((prev (gethash id magpi-status--hours))
        (film (gethash id magpi-status--films)))
    (puthash id hour magpi-status--hours)
    (cond
     ((or magpi-status-reduced-motion (eq hour 'blood))
      (magpi-status--motion-cancel id)
      nil)
     ((and film (eq (car film) hour))
      (nth (nth 1 film) (nth 2 film)))
     (t
      (magpi-status--motion-cancel id)
      (let ((frames (and (magpi-status--motion-alive-p id)
                         (magpi-status--film-for prev hour))))
        (when (and frames (magpi-status--film-displayable-p frames))
          (magpi-status--motion-start
           id hour frames
           (if (eq hour 'rest)
               magpi-status--settle-dwell
             magpi-status--sky-dwell))
          (magpi-status--motion-frame id)))))))
(defun magpi-status--retention-mark (state)
  "Return one-column Muninn mark for retention STATE."
  (let ((glyph (alist-get state magpi-status-retention-cast)))
    (if (magpi-status--glyph-displayable-p glyph magpi-status-retention-width)
        glyph
      (or (alist-get state magpi-status-retention-fallback) "."))))

(defun magpi-status--retention-face (state)
  (if (eq state 'empty) 'magpi-status-quiet 'magpi-status-evidence))

(defun magpi-status--in-flight-p (action)
  (memq (magpi-observation-auspice (magpi-action-observation action))
        '(lift aloft)))

(defun magpi-status--cold-action-p (action)
  (eq (magpi-observation-auspice (magpi-action-observation action)) 'cold))

(defun magpi-status--pending-ask-p (observation)
  (seq-find (lambda (ask) (eq (magpi-ask-state ask) 'pending))
            (and observation (magpi-observation-asks observation))))

(defun magpi-status--action-attention (action)
  "Return ACTION's attention.  Fault outranks pending judgment."
  (let ((observation (magpi-action-observation action)))
    (cond
     ((eq (magpi-observation-auspice observation) 'blood) 'fault)
     ((magpi-status--pending-ask-p observation) 'ask))))

(defun magpi-status--action-bound-p (action)
  (when-let* ((launch (magpi-action-launch action))
              (context (magpi-launch-spec-context launch)))
    (not (eq (plist-get context :kind) 'none))))

(defun magpi-status--action-retention-state (action)
  "Project ACTION facts to Muninn's empty, bound, or hearing seat."
  (let ((observation (magpi-action-observation action)))
    (cond
     ((and observation
           (or (magpi-observation-observed-files observation)
               (magpi-observation-last-response observation)
               (magpi-observation-asks observation)))
      'hears)
     ((magpi-status--action-bound-p action) 'bound)
     (t 'empty))))

(defun magpi-status--now ()
  (time-convert nil 'integer))

(defun magpi-status--unix (value)
  (cond
   ((integerp value) value)
   ((numberp value) (floor value))
   ((consp value) (floor (float-time value)))))

(defun magpi-status--age-label (unix now)
  "Quiet age for UNIX seconds relative to sampled NOW."
  (when (and unix now)
    (let ((delta (max 0 (- now unix))))
      (cond
       ((< delta 60) "now")
       ((< delta 3600) (format "%dm" (/ delta 60)))
       ((< delta 86400) (format "%dh" (/ delta 3600)))
       (t (format "%dd" (/ delta 86400)))))))

(defun magpi-status--action-unix (action)
  (or (magpi-status--unix (magpi-action-started-at action))
      (magpi-status--unix (magpi-action-created-at action))))

(defun magpi-status--intention-unix (intention actions)
  (let ((times (delq nil
                     (cons (magpi-status--unix
                            (magpi-intention-created-at intention))
                           (mapcar #'magpi-status--action-unix actions)))))
    (when times (apply #'max times))))

(defun magpi-status--sort-actions (actions)
  (seq-sort (lambda (a b)
              (> (or (magpi-status--action-unix a) 0)
                 (or (magpi-status--action-unix b) 0)))
            actions))

(defun magpi-status--git-fact-present-p (_intention &optional facts)
  (plist-get facts :exists))

(defun magpi-status--subject-glance (intention actions &optional facts)
  "Empty or quiescent for the subject.  Not Action auspice, not stored."
  (cond
   ((and (null intention) (null actions)) 'empty)
   ((and (magpi-intention-active-p intention)
         (not (seq-some #'magpi-status--in-flight-p actions))
         (magpi-status--git-fact-present-p intention facts))
    'quiescent)))

(defun magpi-status--plain (text)
  "Return TEXT without incoming properties.  Missing is the empty string."
  (if (stringp text) (substring-no-properties text) ""))

(defun magpi-status--seat-text (text)
  "Normalize TEXT to one line at the projection boundary.  Missing is nil.

Incoming display properties are stripped so visual width is the renderer's."
  (let ((line (string-trim
               (replace-regexp-in-string
                "[\n\r\t ]+" " " (magpi-status--plain text)))))
    (unless (string-empty-p line) line)))

(defun magpi-status--generated-title (action)
  "Return durable title, prompt, context fallback, or New task."
  (or (magpi-status--seat-text (magpi-action-title action))
      (magpi-status--seat-text (magpi-action-prompt action))
      (when-let ((launch (magpi-action-launch action)))
        (magpi-status--seat-text
         (magpi-launch-context-title (magpi-launch-spec-context launch))))
      "New task"))

(defun magpi-status--supplied-peek (action)
  "Return (TITLE . LAST) from the historical callback, or nil.

TITLE is a string.  LAST is last assistant text, or nil.  A bare string
is TITLE only.  Never manufactures Observation."
  (when magpi-status-action-title-function
    (let ((supplied (funcall magpi-status-action-title-function action)))
      (cond
       ((and (stringp supplied) (not (string-empty-p supplied)))
        (cons supplied nil))
       ((and (consp supplied)
             (not (keywordp (car supplied)))
             (stringp (car supplied))
             (not (string-empty-p (car supplied))))
        (cons (car supplied)
              (and (stringp (cdr supplied)) (not (string-empty-p (cdr supplied)))
                   (cdr supplied))))))))

(defun magpi-status--action-title (action)
  "Return (TEXT . KIND) for ACTION.  KIND is observed or doing.

Doing is never the authored why.  Intention headings keep `authored'."
  (let* ((observation (magpi-action-observation action))
         (observed (and observation
                        (magpi-status--seat-text
                         (magpi-observation-display-title observation))))
         (historical (magpi-status--seat-text
                      (car (magpi-status--supplied-peek action)))))
    (cond
     (observed (cons observed 'observed))
     (historical (cons historical 'doing))
     (t (cons (magpi-status--generated-title action) 'doing)))))

(defun magpi-status--action-loading (action)
  "Return a porcelain loading label for ACTION, or nil.

Never manufactures Observation or recodes auspice."
  (when magpi-status-action-loading-function
    (let ((loading (funcall magpi-status-action-loading-function action)))
      (and (stringp loading) (not (string-empty-p loading)) loading))))
(defun magpi-status--role-profile (role)
  "Quiet tool-profile word for ROLE.  Bare w is reserved for the writer lease."
  (pcase role
    ('writer "writer")
    ('reader "reader")))

(defun magpi-status--quiet-pad (width)
  (magpi-status--heading-cell (make-string (max 0 width) ?\s) 'magpi-status-quiet))

(defun magpi-status--fit-cell (text width &optional align)
  "Truncate and pad TEXT to WIDTH display columns, preserving properties.
ALIGN `right' leans the text; otherwise it stays at the origin."
  (let* ((text (or text ""))
         (fitted (truncate-string-to-width text (max 0 width) nil nil "…"))
         (pad (magpi-status--quiet-pad (max 0 (- width (string-width fitted))))))
    (magpi-status--own-columns
     (if (eq align 'right)
         (concat pad fitted)
       (concat fitted pad))
     width)))

(defun magpi-status--fit-atomic (text width &optional align)
  "Pad TEXT to WIDTH, or blank the seat when the fact does not fit.

Facts never ellipsize.  Only title and model may use a fitting ellipsis."
  (let* ((text (or text ""))
         (width (max 0 width))
         (tw (string-width text)))
    (cond
     ((zerop width) "")
     ((> tw width) (magpi-status--quiet-pad width))
     (t
      (let ((pad (magpi-status--quiet-pad (- width tw))))
        (magpi-status--own-columns
         (if (eq align 'right)
             (concat pad text)
           (concat text pad))
         width))))))

(defun magpi-status--column-pitch (&optional window)
  "Pixel width of one heading column.  Cells are `fixed-pitch'."
  (or (and (fboundp 'window-font-width)
           (let ((width (ignore-errors
                          (window-font-width window 'fixed-pitch))))
             (and (numberp width) (> width 0) width)))
      (default-font-width)))

(defun magpi-status--window-columns (window)
  "Wrap-safe `fixed-pitch' columns for WINDOW.

`window-body-width' counts the default face and does not reserve wrap.

On a graphic frame with fringes, `window-max-chars-per-line' lets the
newline overflow into the fringe and does not reserve a column.
`word-wrap' still wraps the last word (age) onto the next visual line.
Live windows therefore keep one free column.  Hidden and test windows
fall back to body width."
  (let* ((body (and (fboundp 'window-body-width)
                    (ignore-errors (window-body-width window))))
         (pixels (and (window-live-p window)
                      (ignore-errors (window-body-width window t))))
         (pitch (magpi-status--column-pitch window))
         (from-pixels (and (numberp pixels) (numberp body) (numberp pitch)
                           (> pixels body) (> pitch 0)
                           (/ pixels pitch)))
         (wrap (and (window-live-p window)
                    (fboundp 'window-max-chars-per-line)
                    (ignore-errors
                      (window-max-chars-per-line window 'fixed-pitch))))
         (raw (let ((candidates (delq nil (list
                                           (and (numberp wrap) (> wrap 0) wrap)
                                           (and (numberp from-pixels)
                                                (> from-pixels 0)
                                                from-pixels)
                                           (and (numberp body) (> body 0) body)))))
                (and candidates (apply #'min candidates)))))
    (cond
     ((and (window-live-p window) (numberp raw)) (max 1 (1- raw)))
     (t raw))))

(defun magpi-status--own-columns (text columns)
  "Pad TEXT so it occupies COLUMNS on glass without changing string-width.

Huginn modifier letters can be `string-width' 1 while painting narrower
than a column.  Canonical spaces then leave later seats, including age,
short of the shared right edge.  A zero-width stretch owns the missing
pixels.  Over-wide glyphs are not shrunk."
  (let ((text (or text ""))
        (columns (max 0 columns)))
    (if (or (string-empty-p text)
            (zerop columns)
            (not (fboundp 'string-pixel-width))
            (not (fboundp 'default-font-width)))
        text
      (let* ((fw (magpi-status--column-pitch))
             (actual (and (> fw 0) (string-pixel-width text)))
             (deficit (and actual (- (* columns fw) actual))))
        (if (or (not deficit) (<= deficit 0))
            text
          (concat text
                  (propertize (char-to-string #x200B)
                              'display (list 'space :width
                                             (/ (float deficit) fw)))))))))

(defun magpi-status--take-right (text width)
  "Longest suffix of TEXT whose display width is at most WIDTH."
  (let ((text (or text ""))
        (width (max 0 width)))
    (cond
     ((zerop width) "")
     ((<= (string-width text) width) text)
     (t
      (let ((start 0)
            (len (length text)))
        (while (and (< start len)
                    (> (string-width (substring text start)) width))
          (setq start (1+ start)))
        (substring text start))))))

(defun magpi-status--fit-from-left (text width)
  "Fit TEXT into WIDTH columns, ellipsis on the left.
Keeps the specific suffix of a model id."
  (let* ((text (or text ""))
         (width (max 0 width)))
    (cond
     ((zerop width) "")
     ((<= (string-width text) width) text)
     (t
      (let ((ellipsis "…")
            (ew (string-width "…")))
        (if (<= width ew)
            (truncate-string-to-width text width nil nil "")
          (concat ellipsis (magpi-status--take-right text (- width ew)))))))))

(defconst magpi-status-quiet-order
  '(lease history git model effort context age)
  "Quiet-seat grammar, left to right after the omen (STRUCTURE.org).

Occupancy is row-specific; this is only the order a row may hold seats in.
A row never holds a seat out of this order, and age is its right edge.")
(defun magpi-status--title-bounds ()
  (let* ((title-max (max 1 magpi-status-title-max-width))
         (title-min (min title-max (max 1 magpi-status-title-min-width))))
    (cons title-min title-max)))

(defun magpi-status--occupied-quiet (view)
  (delq nil
        (list (and (magpi-status-heading-view-lease view) 'lease)
              (and (magpi-status-heading-view-history view) 'history)
              (and (magpi-status-heading-view-git view) 'git)
              (and (magpi-status-heading-view-model view) 'model)
              (and (magpi-status-heading-view-effort view) 'effort)
              (and (magpi-status-heading-view-context view) 'context)
              (and (magpi-status-heading-view-age view) 'age))))

(defun magpi-status--seat-natural-width (view slot)
  (pcase slot
    ('lease 1)
    ('history 5)
    ('git (string-width (or (magpi-status-heading-view-git view) "")))
    ('model (string-width (or (magpi-status-heading-view-model view) "")))
    ('effort magpi-status-effort-width)
    ('context (string-width (or (magpi-status-heading-view-context view) "")))
    ('age (string-width (or (magpi-status-heading-view-age view) "")))
    (_ 0)))

(defun magpi-status--quiet-widths (view admitted model-w)
  (let (widths)
    (dolist (slot magpi-status-quiet-order)
      (when (memq slot admitted)
        (push (cons slot
                    (if (eq slot 'model)
                        (or model-w (magpi-status--seat-natural-width view slot))
                      (magpi-status--seat-natural-width view slot)))
              widths)))
    (nreverse widths)))

(defun magpi-status--quiet-span (widths)
  (let ((n (length widths)))
    (if (zerop n)
        0
      (+ 1 (apply #'+ (mapcar #'cdr widths)) (max 0 (1- n))))))

(defun magpi-status--heading-need (title-w quiet-widths &optional overhead)
  (+ (or overhead magpi-status--omen-overhead)
     title-w
     (magpi-status--quiet-span quiet-widths)))

(defun magpi-status--ownership-occupied-p (view)
  "Return non-nil when VIEW holds a lease or Git decision."
  (or (magpi-status-heading-view-lease view)
      (magpi-status-heading-view-git view)))

(defun magpi-status--allocate (view width title-min title-max)
  "Return (MODE TITLE-W ADMITTED MODEL-WIDTH) for VIEW at WIDTH.

Judgment and ownership outrank ornament.  Truncate model, omit effort and
context, contract title, omit age and history, then omit model.  A held
lease or Git fact then drops the five-column cast and retention rather
than disappearing.  Git and lease leave the heading only when the row
cannot hold them with a bare title.  Omitted ornament may continue on a
glance overflow line.  `magpi-status-quiet-order' is paint order, not this
drop order."
  (cond
   ((< width magpi-status--hard-min)
    (list 'ultra nil nil nil))
   ((and (< width (+ magpi-status--omen-overhead title-min))
         (not (magpi-status--ownership-occupied-p view)))
    (list 'emergency (max 1 (- width magpi-status--omen-overhead)) nil nil))
   (t
    (let* ((admitted (magpi-status--occupied-quiet view))
           (title-w title-max)
           (model-w (and (memq 'model admitted)
                         (magpi-status--seat-natural-width view 'model)))
           (omen 'cast))
      (cl-labels
          ((overhead ()
             (if (eq omen 'cast)
                 magpi-status--omen-overhead
               magpi-status-attention-width))
           (need ()
             (magpi-status--heading-need
              title-w (magpi-status--quiet-widths view admitted model-w)
              (overhead)))
           (overflow ()
             (> (need) width))
           (drop (slot)
             (setq admitted (delq slot admitted))
             (when (eq slot 'model) (setq model-w nil))))
        (when (and (memq 'model admitted) (overflow))
          (when (> model-w magpi-status--model-min)
            (setq model-w
                  (max magpi-status--model-min
                       (- model-w (- (need) width))))))
        (when (and (memq 'effort admitted) (overflow))
          (drop 'effort))
        (when (and (memq 'context admitted) (overflow))
          (drop 'context))
        (when (overflow)
          (setq title-w (max title-min (- title-w (- (need) width)))))
        (dolist (slot '(age history))
          (when (and (memq slot admitted) (overflow))
            (drop slot)))
        (when (and (memq 'model admitted) (overflow))
          (drop 'model))
        (when (and (eq omen 'cast) (overflow)
                   (magpi-status--ownership-occupied-p view))
          (setq omen 'bare))
        (when (overflow)
          (setq title-w (max 4 (- title-w (- (need) width)))))
        (when (and (overflow) (memq 'git admitted))
          (drop 'git))
        (when (and (overflow) (memq 'lease admitted))
          (drop 'lease))
        (when (and (< (need) width) (< title-w title-max))
          (setq title-w (min title-max (+ title-w (- width (need))))))
        (list (if (eq omen 'bare) 'bare 'normal) title-w admitted model-w))))))

(defun magpi-status--ultra-plan (view width)
  (let* ((mark (magpi-status-heading-view-attention view))
         (mark-w (if (and mark (> width 0)) 1 0))
         (title-w (max 0 (- width mark-w)))
         plan)
    (when (> mark-w 0)
      (push (list 'attention 0 mark-w 'pad) plan))
    (when (> title-w 0)
      (push (list 'title mark-w title-w 'pad) plan))
    (nreverse plan)))

(defun magpi-status--omen-plan (view title-w width admitted model-w &optional bare)
  "One seat plan in column order: omen, then the quiet grammar, age on the right.

BARE omits the five-column cast and retention so lease and Git can remain."
  (let* ((title-start magpi-status-attention-width)
         (plan (list (list 'attention 0 magpi-status-attention-width 'pad)
                     (list 'title title-start title-w 'pad)))
         (col (+ magpi-status-attention-width title-w))
         (tail nil))
    (unless bare
      (let ((motion-start (+ title-start title-w 1)))
        (setq plan (nconc plan
                          (list (list 'motion motion-start
                                      magpi-status-motion-width 'pad)
                                (list 'retention
                                      (+ motion-start magpi-status-motion-width 1)
                                      magpi-status-retention-width 'pad)))
              col (+ magpi-status--omen-overhead title-w))))
    (dolist (slot magpi-status-quiet-order)
      (when (memq slot admitted)
        (let ((w (if (eq slot 'model)
                     (or model-w (magpi-status--seat-natural-width view slot))
                   (magpi-status--seat-natural-width view slot)))
              (mode (cond ((eq slot 'age) 'right)
                          ((eq slot 'model) 'left)
                          (t 'pad))))
          (if (eq slot 'age)
              ;; The right-leaning seat lands at the sampled edge, never mid-row.
              (push (list slot (max col (- width w))
                          w mode)
                    tail)
            (setq col (1+ col))
            (push (list slot col w mode) tail)
            (setq col (+ col w))))))
    (nconc plan (nreverse tail))))

(defun magpi-status--heading-plan (view width)
  "Private allocator.  Each item is (SLOT START WIDTH MODE)."
  (pcase-let* ((`(,title-min . ,title-max) (magpi-status--title-bounds))
               (`(,mode ,title-w ,admitted ,model-w)
                (magpi-status--allocate view width title-min title-max)))
    (pcase mode
      ('ultra (magpi-status--ultra-plan view width))
      ('emergency (magpi-status--omen-plan view title-w width nil nil))
      ('bare (magpi-status--omen-plan view title-w width admitted model-w t))
      (_ (magpi-status--omen-plan view title-w width admitted model-w)))))

(defun magpi-status--slot-cell (view slot)
  (pcase slot
    ('attention
     (pcase (magpi-status-heading-view-attention view)
       ('fault (cons "!" 'magpi-status-alert))
       ('ask (cons "?" 'magpi-status-pending))
       (_ (cons "" nil))))
    ('title
     (cons (or (magpi-status-heading-view-title view) "")
           (if (eq (magpi-status-heading-view-title-kind view) 'authored)
               'magpi-status-identity
             'magpi-status-title)))
    ('motion
     (let ((hour (or (magpi-status-heading-view-motion view) 'cold)))
       (cons (magpi-status--auspice-motion hour magpi-status--paint-frame)
             (magpi-status--auspice-face hour))))
    ('retention
     (let ((state (or (magpi-status-heading-view-retention view) 'empty)))
       (cons (magpi-status--retention-mark state)
             (magpi-status--retention-face state))))
    ('lease (cons "w" 'magpi-status-quiet))
    ('history (cons "load." 'magpi-status-quiet))
    ('git (cons (or (magpi-status-heading-view-git view) "") 'magpi-status-quiet))
    ('model (cons (or (magpi-status-heading-view-model view) "") 'magpi-status-quiet))
    ('effort (cons (or (magpi-status-heading-view-effort view) "") 'magpi-status-quiet))
    ('context (cons (or (magpi-status-heading-view-context view) "") 'magpi-status-quiet))
    ('age (cons (or (magpi-status-heading-view-age view) "") 'magpi-status-quiet))))

(defun magpi-status--paint-slot (view slot width mode)
  (pcase-let* ((`(,text . ,face) (magpi-status--slot-cell view slot))
               (painted (magpi-status--heading-cell
                         (magpi-status--plain (or text "")) face)))
    (pcase (cons slot mode)
      (`(title . ,_) (magpi-status--fit-cell painted width))
      (`(model . ,_) (magpi-status--fit-from-left painted width))
      (`(_ . right) (magpi-status--fit-atomic painted width 'right))
      (_ (magpi-status--fit-atomic painted width)))))

(defun magpi-status--paint-heading (view plan width)
  "Paint PLAN left to right by START column.  List order never matters."
  (let* ((ordered (seq-sort (lambda (a b) (< (nth 1 a) (nth 1 b))) plan))
         (out "")
         (col 0)
         (complete (magpi-status--plan-slot plan 'motion)))
    (dolist (item ordered)
      (pcase-let ((`(,slot ,start ,w ,mode) item))
        (when (< col start)
          (setq out (concat out (magpi-status--quiet-pad (- start col)))
                col start))
        (let ((piece (magpi-status--paint-slot view slot w mode)))
          (setq out (concat out piece)
                col (+ col (string-width piece))))))
    (when (and complete (< col width))
      (setq out (concat out (magpi-status--quiet-pad (- width col)))))
    out))

(defun magpi-status--plan-slot (plan slot)
  (seq-find (lambda (item) (eq (car item) slot)) plan))

(defconst magpi-status-overflow-order
  '(motion retention history model effort context)
  "Ornament that may continue under the heading when squeezed off it.

Lease, Git, and age stay on the Magit heading so fold still shows ownership.")

(defun magpi-status--overflow-cast-p (view)
  "Return non-nil when VIEW's motion hour is worth continuing under the heading.

Empty, cold, and quiescent still frames stay off the overflow line."
  (memq (magpi-status-heading-view-motion view) '(lift aloft rest blood)))

(defun magpi-status--overflow-slots (view plan)
  "Return VIEW seats omitted from PLAN that may continue as glance overflow."
  (let ((shown (mapcar #'car plan))
        (cast (magpi-status--overflow-cast-p view)))
    (seq-filter
     (lambda (slot)
       (and (not (memq slot shown))
            (pcase slot
              ('motion cast)
              ('retention cast)
              ('history (magpi-status-heading-view-history view))
              ('model (magpi-status-heading-view-model view))
              ('effort (magpi-status-heading-view-effort view))
              ('context (magpi-status-heading-view-context view)))))
     magpi-status-overflow-order)))

(defun magpi-status--overflow-piece (view slot)
  (let* ((w (pcase slot
              ('motion magpi-status-motion-width)
              ('retention magpi-status-retention-width)
              ('model (max magpi-status--model-min
                           (magpi-status--seat-natural-width view 'model)))
              (_ (magpi-status--seat-natural-width view slot))))
         (mode (if (eq slot 'model) 'left 'pad)))
    (and (> w 0) (magpi-status--paint-slot view slot w mode))))

(defun magpi-status--paint-overflow (view slots width)
  "Paint SLOTS as one indented continuation.  Atomic except model."
  (let* ((indent "    ")
         (budget (max 0 (- width (string-width indent))))
         (col 0)
         pieces)
    (dolist (slot slots)
      (when-let ((piece (magpi-status--overflow-piece view slot)))
        (let ((w (string-width piece))
              (sep (if pieces 1 0)))
          (when (and (> w 0) (<= (+ col sep w) budget))
            (when pieces
              (push " " pieces)
              (setq col (1+ col)))
            (push piece pieces)
            (setq col (+ col w))))))
    (when pieces
      (concat indent (apply #'concat (nreverse pieces))))))

(defun magpi-status--insert-overflow (view)
  "Insert omitted ornament under the heading.  Not a Magit heading.

Fold still hides this line.  Lease and Git never live here."
  (let ((width (magpi-status--render-width)))
    (unless (< width magpi-status--hard-min)
      (when-let ((plan (magpi-status--heading-plan view width))
                 (slots (magpi-status--overflow-slots view plan))
                 (line (magpi-status--paint-overflow view slots width)))
        (insert line "\n")))))
(defvar magpi-status--paint-width nil
  "Width locked for one paint.  Nil means sample now.")

(defun magpi-status--compute-width ()
  "Sample wrap-safe columns.  One profile; does not lock a paint."
  (let* ((windows (get-buffer-window-list (current-buffer) nil t))
         (actual (when windows
                   (apply #'min (mapcar #'magpi-status--window-columns windows)))))
    (cond
     (actual
      (if magpi-status-layout-width
          (min magpi-status-layout-width actual)
        actual))
     (magpi-status-layout-width magpi-status-layout-width)
     (t 80))))

(defun magpi-status--render-width ()
  "Return one width profile shared by every row in the current buffer.

Visible windows sample wrap-safe `fixed-pitch' columns so age stays on
the Magit heading.  Hidden and test buffers fall back to body width.
A live paint locks the sample so later rows cannot see a moved window."
  (or magpi-status--paint-width
      (magpi-status--compute-width)))

(defun magpi-status--spread (left right)
  "Place LEFT at the origin and RIGHT at the sampled width.

LEFT and RIGHT must already fit.  This never ellipsizes."
  (let* ((left (or left ""))
         (right (or right ""))
         (width (magpi-status--render-width))
         (pad (max 0 (- width (string-width left) (string-width right)))))
    (concat left
            (magpi-status--quiet-pad pad)
            right)))

(defun magpi-status--root-branch ()
  "Attached HEAD of the status checkout.  Detached is silence."
  (and magpi-status-root
       (magpi-status--seat-text
        (magpi-git--maybe magpi-status-root
                          "symbolic-ref" "--quiet" "--short" "HEAD"))))

(defun magpi-status--intention-for-ref (ref intentions)
  "Return the INTENTIONS row whose change branch is REF, or nil."
  (and ref
       (seq-find (lambda (intention)
                   (and (magpi-intention-p intention)
                        (magpi-intention--refs-same-p
                         (magpi-intention-branch intention) ref)))
                 intentions)))

(defun magpi-status--root-heading (intentions actions)
  "MAGPI line.  Repo name and why may ellipsize; roost and branch never fragment.

When HEAD is magpi/<id>, the why sits in front of that derived branch.
A garden folder that is only the id yields to the why."
  (let* ((width (magpi-status--render-width))
         (right (or (magpi-status--root-roost intentions actions) ""))
         (prefix (magpi-status--heading-cell "MAGPI · " 'default))
         (ref (magpi-status--root-branch))
         (intention (magpi-status--intention-for-ref ref intentions))
         (why (and intention
                   (magpi-status--seat-text
                    (magpi-intention-objective intention))))
         (dirname (or (magpi-status--seat-text
                       (file-name-nondirectory
                        (directory-file-name (or magpi-status-root ""))))
                      ""))
         (garden-p (and intention why
                        (equal dirname (magpi-intention-id intention))))
         (name (magpi-status--heading-cell
                (if garden-p why dirname)
                (if garden-p 'magpi-status-identity 'magpi-status-repo)))
         (sep (magpi-status--heading-cell " · " 'magpi-status-quiet))
         (why-cell (and why (not garden-p)
                        (magpi-status--heading-cell why 'magpi-status-identity)))
         (why-mark (and why-cell (concat sep why-cell)))
         (mark (and ref
                    (concat sep
                            (magpi-status--heading-cell ref 'magpi-status-quiet)))))
    (unless (<= (string-width right) width)
      (setq right ""))
    (let ((rest (max 0 (- width (string-width right)))))
      (unless (<= (string-width prefix) rest)
        (setq prefix ""))
      (let* ((budget (max 0 (- rest (string-width prefix))))
             (mark-w (string-width (or mark ""))))
        (cond
         ((<= (+ (string-width name)
                 (string-width (or why-mark ""))
                 mark-w)
              budget))
         ((and mark (<= mark-w budget))
          (let ((left (max 0 (- budget mark-w))))
            (cond
             ((and why-mark
                   (<= (+ (string-width name) (string-width sep)) left))
              (let ((why-w (max 0 (- left (string-width name)
                                     (string-width sep)))))
                (setq why-cell (and (not (zerop why-w))
                                    (magpi-status--fit-cell why-cell why-w))
                      why-mark (and why-cell (concat sep why-cell)))))
             (t
              (setq why-mark nil
                    name (if (zerop left) ""
                           (magpi-status--fit-cell name left)))))))
         (t
          (setq mark nil why-mark nil)
          (when (> (string-width name) budget)
            (setq name (magpi-status--fit-cell name budget)))))
        (magpi-status--spread
         (concat prefix name (or why-mark "") (or mark ""))
         right)))))

(defun magpi-status--render-heading (view &optional indent frame)
  "Render ephemeral heading VIEW.  Casts and faces are chosen here.

INDENT is left inset for a nested Action.  FRAME is an explicit motion
overlay; nil is the still cast.  The plan uses the remaining width."
  (let* ((magpi-status--paint-frame (or frame magpi-status--paint-frame))
         (indent (or indent 0))
         (width (max 0 (- (magpi-status--render-width) indent)))
         (plan (magpi-status--heading-plan view width)))
    (concat (magpi-status--quiet-pad indent)
            (magpi-status--paint-heading view plan width))))

(defun magpi-status--action-heading-view (action now)
  "Project ACTION to named semantic seats.  NOW is a sampled clock."
  (let* ((observation (magpi-action-observation action))
         (launch (magpi-action-launch action))
         (title (magpi-status--action-title action)))
    (magpi-status--make-heading-view
     :attention (magpi-status--action-attention action)
     :title (car title)
     :title-kind (cdr title)
     :motion (magpi-observation-auspice observation)
     :retention (magpi-status--action-retention-state action)
     :history (and (magpi-status--action-loading action) t)
     :model (and observation
                 (magpi-status--seat-text
                  (magpi-observation-running-model observation)))
     :effort (and launch
                  (magpi-launch-thinking-label
                   (magpi-launch-spec-thinking launch)))
     :context (magpi-status--context-label
               (and observation (magpi-observation-usage observation)))
     :age (magpi-status--age-label (magpi-status--action-unix action) now))))
(defun magpi-status--count-label (n)
  "Compact count for glance.  Missing is silence."
  (when (numberp n)
    (cond
     ((< (abs n) 1000) (format "%d" (truncate n)))
     ((< (abs n) 1000000) (magpi-status--scaled-count n 1000.0 "k"))
     (t (magpi-status--scaled-count n 1000000.0 "M")))))

(defun magpi-status--scaled-count (n scale suffix)
  (let ((value (/ (float n) scale)))
    (if (zerop (mod n scale))
        (format "%d%s" (truncate value) suffix)
      (format "%.1f%s" value suffix))))

(defun magpi-status--usage-label (usage)
  "One-line tokens and cost from USAGE.  Blank facts stay silent."
  (when (listp usage)
    (let* ((input (magpi-status--count-label (plist-get usage :input)))
           (output (magpi-status--count-label (plist-get usage :output)))
           (total (magpi-status--count-label (plist-get usage :total)))
           (context (magpi-status--context-label usage))
           (cost (plist-get usage :cost))
           (parts (delq nil
                        (list
                         (cond
                          ((and input output)
                           (format "%s in · %s out" input output))
                          (input (format "%s in" input))
                          (output (format "%s out" output))
                          (total total))
                         (when context (format "%s ctx" context))
                         (when (and (numberp cost) (> cost 0))
                           (format "$%.4f" cost))))))
      (when parts (mapconcat #'identity parts " · ")))))

(defun magpi-status--context-label (usage)
  "Compact context/window for the heading seat.  Missing is silence."
  (when (listp usage)
    (let ((context (magpi-status--count-label (plist-get usage :context)))
          (window (magpi-status--count-label (plist-get usage :window))))
      (when (and context window)
        (format "%s/%s" context window)))))


(defun magpi-status--gap ()
  "One blank line between logical groups."
  (insert "\n"))

(defun magpi-status--insert-kv (key value &optional value-face)
  "Insert a labeled KEY/VALUE row.  Blank VALUE is silence, not a mark."
  (when (and (stringp value) (not (string-empty-p value)))
    (insert "    "
            (magpi-status--face (format "%-10s" key) 'magpi-status-quiet)
            " "
            (if (text-properties-at 0 value)
                value
              (magpi-status--face value (or value-face 'default)))
            "\n")))

(defun magpi-status--same-glance-p (left right)
  (let ((left (and (stringp left) (magpi--one-line left 60)))
        (right (and (stringp right) (magpi--one-line right 60))))
    (and left right (equal left right))))

(defun magpi-status--fit-under (text indent)
  "Collapse TEXT onto one wrap-safe glance line under INDENT, or nil."
  (let* ((indent (or indent ""))
         (budget (max 0 (- (magpi-status--render-width) (string-width indent)))))
    (and (> budget 0) (magpi--one-line text budget))))

(defun magpi-status--insert-pair (user last heading)
  "Insert USER with LAST indented beneath it.  USER matching HEADING is omitted.

Each line is wrap-safe against the glance width."
  (let* ((heading (and (stringp heading) (substring-no-properties heading)))
         (show-user (and user
                         (not (magpi-status--same-glance-p user heading))
                         (not (magpi-status--same-glance-p user last))))
         (user-indent "    ")
         (last-indent (if show-user "      " "    "))
         (user (and show-user (magpi-status--fit-under user user-indent)))
         (last (and last
                    (not (magpi-status--same-glance-p last heading))
                    (magpi-status--fit-under last last-indent))))
    (when user
      (insert user-indent (magpi-status--face user 'magpi-status-identity) "\n"))
    (when last
      (insert last-indent (magpi-status--face last 'magpi-status-quiet) "\n"))))

(defun magpi-status--ask-state-presentation (state)
  "Return the glance symbol, label, and face for Pi-ask STATE."
  (pcase state
    ('approved '("✓" "answered" magpi-status-live))
    ('rejected '("✕" "rejected" magpi-status-alert))
    ('dismissed '("–" "dismissed" magpi-status-quiet))
    (_ '("?" "ask" magpi-status-pending))))

(defun magpi-status--ask-start-hidden-p (ask)
  "Fold resolved ASK so pending judgment owns the open body."
  (not (eq (magpi-ask-state ask) 'pending)))

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
      (magit-insert-section (magpi-ask (cons action-id id)
                                       (magpi-status--ask-start-hidden-p ask))
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

(defun magpi-status--insert-action (action &optional now indent)
  "Insert ACTION as a typed, foldable Magit section.

The heading is the omen, then named quiet seats.  Fold is Magit.  Ornament
squeezed off the heading continues on the next glance line.  The body holds
role, requested model, activity detail, connection, problem, tokens, the
user/last pair, asks, and observed files.  INDENT insets a nested Action
under its why.
The heading model seat is running observation only; requested stays here."
  (let* ((id (magpi-action-id action))
         (launch (magpi-action-launch action))
         (observation (magpi-action-observation action))
         (role (and launch (magpi-launch-spec-role launch)))
         (now (or now (magpi-status--now)))
         (view (magpi-status--action-heading-view action now))
         (hour (magpi-status-heading-view-motion view))
         (magpi-status--paint-frame
          (and (derived-mode-p 'magpi-status-mode)
               (magpi-status--motion-sync id hour))))
    (magit-insert-section (magpi-action id nil)
      (magit-insert-heading (magpi-status--render-heading view indent))
      (magpi-status--insert-overflow view)
      (let ((body-mark (point)))
        (when-let ((profile (magpi-status--role-profile role)))
          (magpi-status--insert-kv
           "role"
           profile
           'magpi-status-quiet))
        (when-let ((requested (and launch
                                   (magpi-status--seat-text
                                    (magpi-launch-spec-requested-model launch)))))
          (magpi-status--insert-kv "requested" requested 'magpi-status-quiet))
        (when observation
          (when-let ((activity (magpi-observation-activity observation)))
            (when (stringp activity)
              (magpi-status--insert-kv
               "activity" activity
               (magpi-status--auspice-face
                (magpi-observation-auspice observation)))))
          (when (eq (magpi-observation-connection-state observation) 'disconnected)
            (magpi-status--insert-kv "connection" "disconnected"
                                     'magpi-status-alert))
          (when-let ((problem (magpi-observation-problem observation)))
            (magpi-status--insert-kv "problem" problem 'magpi-status-alert))
          (when-let ((tokens (magpi-status--usage-label
                              (magpi-observation-usage observation))))
            (magpi-status--insert-kv "tokens" tokens 'magpi-status-quiet)))
        (magpi-status--insert-pair
         (or (and observation (magpi-observation-last-prompt observation))
             (magpi-action-prompt action))
         (or (and observation (magpi-observation-last-response observation))
             (cdr (magpi-status--supplied-peek action)))
         (magpi-status-heading-view-title view))
        (when observation
          (when-let ((asks (magpi-observation-asks observation)))
            (magpi-status--insert-asks id asks))
          (when-let ((files (magpi-observation-observed-files observation)))
            (when (> (point) body-mark)
              (magpi-status--gap))
            (magit-insert-section (magpi-observed-files id nil)
              (magit-insert-heading
               (magpi-status--face "    observed files" 'magpi-status-quiet))
              (dolist (file files)
                (magit-insert-section (magpi-observed-file (cons id file) nil)
                  (insert "      "
                          (magpi-status--face file 'magpi-status-evidence)
                          "\n"))))))))))

(defun magpi-status--git-label (facts)
  "Dirt and drift from snapshotted FACTS.  Derived branch names stay off the heading.

Not started, dirty, missing, and non-zero drift are decisions.  magpi/<id>
is machinery."
  (when facts
    (let ((checkout (plist-get facts :checkout))
          (ahead (plist-get facts :ahead))
          (behind (plist-get facts :behind)))
      (cond
       ((or (null checkout) (eq checkout 'unstarted)) "not started")
       (t
        (let ((parts (delq nil
                           (list
                            (pcase checkout
                              ('dirty "dirty")
                              ('clean nil)
                              ('missing "missing")
                              ('wrong-branch "wrong branch")
                              ('unavailable "unavailable")
                              (_ (symbol-name checkout)))
                            (when (or (and (numberp ahead) (not (zerop ahead)))
                                      (and (numberp behind) (not (zerop behind))))
                              (format "+%s -%s" (or ahead 0) (or behind 0)))))))
          (and parts (mapconcat #'identity parts " · "))))))))

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

(defun magpi-status--intention-attention (actions)
  "Aggregate child attention so a folded intention still tells the truth."
  (cond
   ((seq-some (lambda (action)
                (eq (magpi-status--action-attention action) 'fault))
              actions)
    'fault)
   ((seq-some (lambda (action)
                (eq (magpi-status--action-attention action) 'ask))
              actions)
    'ask)))

(defun magpi-status--intention-retention-state (intention actions facts)
  "Project retained child, binding, and Git facts without inventing memory."
  (cond
   ((or (plist-get facts :exists)
        (seq-some (lambda (action)
                    (eq (magpi-status--action-retention-state action) 'hears))
                  actions))
    'hears)
   ((or (magpi-intention-bindings intention)
        (seq-some #'magpi-status--action-bound-p actions))
    'bound)
   (t 'empty)))

(defun magpi-status--intention-heading-view (intention actions facts now)
  "Project INTENTION and child ACTIONS from snapshotted FACTS and NOW."
  (let ((glance (or (magpi-status--subject-glance intention actions facts) 'cold)))
    (magpi-status--make-heading-view
     :attention (magpi-status--intention-attention actions)
     :title (or (magpi-status--seat-text (magpi-intention-objective intention)) "")
     :title-kind 'authored
     :motion glance
     :retention (magpi-status--intention-retention-state intention actions facts)
     :lease (and (magpi-intention-writer-lease intention) t)
     :git (magpi-status--git-label facts)
     :age (magpi-status--age-label
           (magpi-status--intention-unix intention actions) now))))

(defun magpi-status--snapshot-git-facts (intentions)
  "Return one Git sample per INTENTION.  Unreadable rows are skipped."
  (let ((table (make-hash-table :test #'equal)))
    (dolist (intention intentions)
      (when (magpi-intention-p intention)
        (puthash (magpi-intention-id intention)
                 (magpi-intention-git-facts intention)
                 table)))
    table))

(defun magpi-status--facts-for (intention)
  "Snapshotted Git facts for INTENTION, or nil.  Missing stays missing."
  (and magpi-status-git-facts
       (magpi-intention-p intention)
       (gethash (magpi-intention-id intention) magpi-status-git-facts)))

(defun magpi-status--insert-intention (intention actions &optional facts now)
  "Insert persisted INTENTION with nested independent ACTIONS.

FACTS are a Git snapshot.  Omitted facts stay missing; this never calls Git."
  (let* ((now (or now (magpi-status--now)))
         (view (magpi-status--intention-heading-view intention actions facts now)))
    (magit-insert-section (magpi-intention (magpi-intention-id intention) nil)
      (magit-insert-heading (magpi-status--render-heading view))
      (magpi-status--insert-overflow view)
      (magpi-status--insert-bindings intention)
      (when (and (magpi-intention-bindings intention) actions)
        (magpi-status--gap))
      (dolist (action (magpi-status--sort-actions actions))
        (magpi-status--insert-action action now magpi-status-action-indent)))))

(defun magpi-status--record-unix (record)
  (pcase (car record)
    ('action (magpi-status--action-unix (cdr record)))
    ('intention (magpi-status--intention-unix (nth 1 record) (nth 2 record)))))

(defun magpi-status--insert-records (intentions actions)
  "Insert Perch then Cold.  Latest live work at the top.

Perch holds intentions and actions with theatre.  Cold holds standalone
chats without theatre.  Nested actions stay with their intention."
  (let ((now (magpi-status--now))
        (groups (make-hash-table :test #'equal))
        perch cold-actions known unreadable)
    (dolist (action actions)
      (if-let ((intention-id (magpi-action-intention-id action)))
          (puthash intention-id
                   (append (gethash intention-id groups) (list action)) groups)
        (if (magpi-status--cold-action-p action)
            (push action cold-actions)
          (push (cons 'action action) perch))))
    (dolist (intention intentions)
      (cond
       ((magpi-intention-p intention)
        (let ((id (magpi-intention-id intention)))
          (push id known)
          (push (list 'intention intention (gethash id groups)) perch)))
       ((magpi-unreadable-p intention)
        (push intention unreadable))))
    (maphash
     (lambda (id grouped)
       (unless (member id known)
         (dolist (action grouped)
           (if (magpi-status--cold-action-p action)
               (push action cold-actions)
             (push (cons 'action action) perch)))))
     groups)
    (setq perch (seq-sort (lambda (a b)
                            (> (or (magpi-status--record-unix a) 0)
                               (or (magpi-status--record-unix b) 0)))
                          perch)
          cold-actions (magpi-status--sort-actions cold-actions)
          unreadable (nreverse unreadable))
    (when perch
      (magit-insert-section (magpi-perch nil)
        (magit-insert-heading (magpi-status--group-heading 'perch))
        (dolist (record perch)
          (pcase (car record)
            ('action (magpi-status--insert-action (cdr record) now))
            ('intention (magpi-status--insert-intention
                         (nth 1 record) (nth 2 record)
                         (magpi-status--facts-for (nth 1 record))
                         now))))))
    (when (and perch cold-actions)
      (magpi-status--gap))
    (when cold-actions
      (magit-insert-section (magpi-cold nil)
        (magit-insert-heading (magpi-status--group-heading 'cold))
        (dolist (action cold-actions)
          (magpi-status--insert-action action now))))
    (dolist (intention unreadable)
      (insert (magpi-status--face
               (format "    unreadable %s · %s"
                       (file-name-nondirectory (magpi-unreadable-path intention))
                       (magpi-unreadable-error intention))
               'magpi-status-alert)
              "\n"))))

(defun magpi-status--root-retention-state (intentions actions)
  (cond
   ((seq-some (lambda (action)
                (eq (magpi-status--action-retention-state action) 'hears))
              actions)
    'hears)
   ((or (seq-some (lambda (intention)
                    (and (magpi-intention-p intention)
                         (magpi-intention-bindings intention)))
                  intentions)
        (seq-some #'magpi-status--action-bound-p actions))
    'bound)
   (t 'empty)))

(defun magpi-status--root-roost (intentions actions)
  "Return the twin clearing marks without turning them into state."
  (let* ((aloft (seq-some #'magpi-status--in-flight-p actions))
         (retention (magpi-status--root-retention-state intentions actions))
         (huginn (if (magpi-status--glyph-displayable-p "◈" 1)
                     (if aloft "◈" "·")
                   (if aloft "*" "."))))
    (concat (magpi-status--heading-cell huginn
                                (if aloft 'magpi-status-live 'magpi-status-quiet))
            (magpi-status--heading-cell " " 'magpi-status-quiet)
            (magpi-status--heading-cell (magpi-status--retention-mark retention)
                                (magpi-status--retention-face retention)))))

(defun magpi-status--same-root-p (here root)
  (when (and here root)
    (equal (file-truename (file-name-as-directory here))
           (file-truename (file-name-as-directory root)))))

(defun magpi-status--action-in-root-p (action)
  "Return non-nil when ACTION belongs to this buffer's root from RAM only."
  (when magpi-status-root
    (let* ((root magpi-status-root)
           (action-root (or (and (magpi-action-launch action)
                                 (magpi-launch-spec-root
                                  (magpi-action-launch action)))
                            (magpi-action-source-root action)))
           (intention-id (magpi-action-intention-id action))
           (intention (and intention-id
                           (hash-table-p magpi--intentions)
                           (gethash intention-id magpi--intentions))))
      (or (magpi-status--same-root-p action-root root)
          (and (magpi-intention-p intention)
               (magpi-status--same-root-p
                (magpi-intention-source-root intention) root))))))

(defun magpi-status--intention-in-root-p (intention)
  (and magpi-status-root
       (magpi-intention-p intention)
       (magpi-status--same-root-p
        (magpi-intention-source-root intention) magpi-status-root)))

(defun magpi-status--attached-ref (directory)
  "Attached HEAD of DIRECTORY.  Detached or missing is nil."
  (and directory
       (magpi-status--seat-text
        (magpi-git--maybe directory
                          "symbolic-ref" "--quiet" "--short" "HEAD"))))

(defun magpi-status--change-ref-p (ref)
  (let ((short (magpi-intention--short-ref ref)))
    (and short (string-prefix-p "magpi/" short))))

(defun magpi-status--intention-related-p (intention &optional ref)
  "Return non-nil when INTENTION belongs on REF.
Nil REF does not cull.  Unreadable rows stay."
  (let ((ref (or ref magpi-status-ref)))
    (cond
     ((null ref) t)
     ((not (magpi-intention-p intention)) t)
     ((magpi-intention--refs-same-p
       (format "magpi/%s" (magpi-intention-id intention)) ref))
     ((magpi-intention--refs-same-p
       (magpi-intention-target-ref intention) ref))
     ((null (magpi-intention-target-ref intention))
      (and (not (magpi-status--change-ref-p ref))
           (or (null magpi-status-head)
               (magpi-intention--refs-same-p ref magpi-status-head)))))))

(defun magpi-status--action-root (action)
  (or (and (magpi-action-launch action)
           (magpi-launch-spec-root (magpi-action-launch action)))
      (magpi-action-source-root action)))

(defun magpi-status--lookup-intention (id &optional intentions)
  (or (seq-some (lambda (row)
                  (and (magpi-intention-p row)
                       (equal (magpi-intention-id row) id)
                       row))
                (or intentions magpi-status-intentions))
      (and id
           (boundp 'magpi--intentions)
           (hash-table-p magpi--intentions)
           (gethash id magpi--intentions))))

(defun magpi-status--action-related-p (action &optional ref origin intentions)
  "Return non-nil when ACTION belongs on REF from ORIGIN.
Nil REF does not cull.  Nested doings follow their intention."
  (let ((ref (or ref magpi-status-ref))
        (origin (or origin magpi-status-origin magpi-status-root)))
    (cond
     ((null ref) t)
     ((not (magpi-action-p action)) t)
     ((magpi-action-intention-id action)
      (let* ((id (magpi-action-intention-id action))
             (intention (magpi-status--lookup-intention id intentions)))
        (if (magpi-intention-p intention)
            (magpi-status--intention-related-p intention ref)
          (magpi-intention--refs-same-p (format "magpi/%s" id) ref))))
     (t
      ;; Paint-safe: peek is this checkout's HEAD.  Collect uses Git ancestry.
      (let ((here (magpi-status--action-root action))
            (head magpi-status-head))
        (and here origin (magpi-status--same-root-p here origin)
             head
             (magpi-intention--refs-same-p ref head)))))))

(defun magpi-status--standalone-related-p (action ref origin)
  "Return non-nil when ACTION's spawn-oid is an ancestor of REF.
Paint never calls this."
  (let ((here (magpi-status--action-root action))
        (oid (magpi-action-spawn-oid action)))
    (and here origin (magpi-status--same-root-p here origin)
         (magpi-store-oid-ancestor-p origin oid ref))))

(defun magpi-status--related (actions intentions &optional ref origin)
  "Cull ACTIONS and INTENTIONS to REF.  Nil REF is the full sample."
  (let ((ref (or ref magpi-status-ref))
        (origin (or origin magpi-status-origin magpi-status-root)))
    (if (null ref)
        (list actions intentions)
      (let ((intentions (seq-filter
                         (lambda (row)
                           (magpi-status--intention-related-p row ref))
                         intentions)))
        (list (seq-filter
               (lambda (action)
                 (if (and (magpi-action-p action)
                          (null (magpi-action-intention-id action)))
                     (magpi-status--standalone-related-p action ref origin)
                   (magpi-status--action-related-p action ref origin intentions)))
               actions)
              intentions)))))

(defun magpi-status--sample-has-action-p (id)
  (seq-some (lambda (record)
              (and (magpi-action-p record)
                   (equal (magpi-action-id record) id)))
            magpi-status-actions))

(defun magpi-status--sample-has-intention-p (id)
  (seq-some (lambda (record)
              (and (magpi-intention-p record)
                   (equal (magpi-intention-id record) id)))
            magpi-status-intentions))

(defun magpi-status--live-record (record table predicate id-fn)
  "Return RECORD, or TABLE's object of the same id when it is live."
  (if (and (funcall predicate record) (hash-table-p table))
      (let ((live (gethash (funcall id-fn record) table)))
        (if (funcall predicate live) live record))
    record))

(defun magpi-status--admit-live-members ()
  "Grow the sample from this Emacs's registry.  Disk membership stays snapshot."
  (when (and magpi-status-root
             (boundp 'magpi--actions)
             (hash-table-p magpi--actions))
    (maphash
     (lambda (id action)
       (when (and (magpi-action-p action)
                  (not (magpi-status--sample-has-action-p id))
                  (magpi-status--action-in-root-p action)
                  (magpi-status--action-related-p action))
         (setq magpi-status-actions (cons action magpi-status-actions))))
     magpi--actions))
  (when (and magpi-status-root
             (boundp 'magpi--intentions)
             (hash-table-p magpi--intentions))
    (maphash
     (lambda (id intention)
       (when (and (magpi-intention-active-p intention)
                  (not (magpi-status--sample-has-intention-p id))
                  (magpi-status--intention-in-root-p intention)
                  (magpi-status--intention-related-p intention))
         (setq magpi-status-intentions
               (cons intention magpi-status-intentions))))
     magpi--intentions)))

(defun magpi-status--keep-sampled-intention-p (record)
  "Keep RECORD unless this Emacs has ended that intention."
  (or (not (magpi-intention-p record))
      (let ((live (and (hash-table-p magpi--intentions)
                       (gethash (magpi-intention-id record) magpi--intentions))))
        (or (not (magpi-intention-p live))
            (magpi-intention-active-p live)))))

(defun magpi-status--prune-departed-members ()
  "Drop sample intentions this Emacs has ended.  Unloaded snapshot rows stay."
  (when (hash-table-p magpi--intentions)
    (setq magpi-status-intentions
          (seq-filter #'magpi-status--keep-sampled-intention-p
                      magpi-status-intentions))))

(defun magpi-status-publish-live ()
  "Publish live registry objects into the sample.

Events replace immutable records in RAM.  This copies those objects into
the presentation sample, drops this Emacs's ended members, and admits new
ones.  Disk list, Git, and prepare stay on `g'.  No list, no Git, no puthash."
  (when (and (boundp 'magpi--actions) (hash-table-p magpi--actions))
    (setq magpi-status-actions
          (mapcar (lambda (record)
                    (magpi-status--live-record
                     record magpi--actions #'magpi-action-p #'magpi-action-id))
                  magpi-status-actions)))
  (when (and (boundp 'magpi--intentions) (hash-table-p magpi--intentions))
    (setq magpi-status-intentions
          (mapcar (lambda (record)
                    (magpi-status--live-record
                     record magpi--intentions #'magpi-intention-p
                     #'magpi-intention-id))
                  magpi-status-intentions)))
  (magpi-status--prune-departed-members)
  (magpi-status--admit-live-members))

(defun magpi-status-refresh-buffer ()
  "Render the concise persisted-intention dashboard.

Event paint and Magit refresh call this.  They never snapshot (`g').
They publish live observations, drop ended members, admit new ones, then render."
  (magpi-status-publish-live)
  (magpi-status--motion-sweep)
  (let ((magpi-status--paint-width (magpi-status--compute-width))
        (actions magpi-status-actions)
        (intentions magpi-status-intentions))
    (magit-insert-section (magpi-status magpi-status-root)
      (magit-insert-heading
       (magpi-status--root-heading intentions actions))
      (magpi-status--gap)
      (if (or intentions actions)
          (magpi-status--insert-records intentions actions)
        (insert (magpi-status--face
                 "    i  create intention
    s  spawn action
"
                 'magpi-status-quiet))))))

(defun magpi-status--section (&optional section)
  (or section (and (fboundp 'magit-current-section)
                   (magit-current-section))))

(defun magpi-status--otype (section)
  (and section (ignore-errors (oref section type))))

(defun magpi-status--ovalue (section)
  (and section (ignore-errors (oref section value))))

(defun magpi-status--oparent (section)
  (and section (ignore-errors (oref section parent))))

(defun magpi-facet (&optional section)
  "Return (TYPE . VALUE) for the Magit section at SECTION.

Glance-side identity only.  Orchestration verbs use magpi-section-* selectors
instead of pcase on TYPE.  Lineage remains Magit parent.  Not a Target plist."
  (setq section (magpi-status--section section))
  (when-let ((type (magpi-status--otype section)))
    (cons type (magpi-status--ovalue section))))

(defun magpi-facet-type (&optional section)
  (car-safe (magpi-facet section)))

(defun magpi-facet-value (&optional section)
  (cdr (magpi-facet section)))

(defun magpi-section-intention-id (&optional section)
  "Intention id in SECTION's Magit lineage, or nil."
  (setq section (magpi-status--section section))
  (pcase (magpi-status--otype section)
    ('magpi-intention (magpi-status--ovalue section))
    ('magpi-bindings (magpi-status--ovalue section))
    ((or 'magpi-perch 'magpi-cold 'magpi-status) nil)
    (_ (and (magpi-status--oparent section)
            (magpi-section-intention-id (magpi-status--oparent section))))))

(defun magpi-section-action-id (&optional section)
  "Action id in SECTION's Magit lineage, or nil."
  (setq section (magpi-status--section section))
  (pcase (magpi-status--otype section)
    ('magpi-action (magpi-status--ovalue section))
    ('magpi-observed-files (magpi-status--ovalue section))
    ('magpi-observed-file (car-safe (magpi-status--ovalue section)))
    ('magpi-ask (car-safe (magpi-status--ovalue section)))
    ('magpi-ask-paths (car-safe (magpi-status--ovalue section)))
    ('magpi-ask-path (car-safe (magpi-status--ovalue section)))
    ((or 'magpi-intention 'magpi-bindings 'magpi-perch 'magpi-cold 'magpi-status) nil)
    (_ (and (magpi-status--oparent section)
            (magpi-section-action-id (magpi-status--oparent section))))))

(defun magpi-section-ask-id (&optional section)
  "Ask id in SECTION's Magit lineage, or nil."
  (setq section (magpi-status--section section))
  (pcase (magpi-status--otype section)
    ('magpi-ask (cdr-safe (magpi-status--ovalue section)))
    ('magpi-ask-paths (cdr-safe (magpi-status--ovalue section)))
    ('magpi-ask-path (nth 1 (magpi-status--ovalue section)))
    ((or 'magpi-action 'magpi-intention 'magpi-bindings
         'magpi-perch 'magpi-cold 'magpi-status
         'magpi-observed-file 'magpi-observed-files)
     nil)
    (_ (and (magpi-status--oparent section)
            (magpi-section-ask-id (magpi-status--oparent section))))))

(defun magpi-section-path (&optional section)
  "Exact path on SECTION, or nil.  Does not walk parents."
  (pcase (magpi-facet section)
    (`(magpi-ask-path . ,value) (nth 2 value))
    (`(magpi-observed-file . ,value) (cdr-safe value))
    (_ nil)))

(defun magpi-section-root (&optional section)
  "Status root in SECTION's lineage, or nil."
  (setq section (magpi-status--section section))
  (while (and section (not (eq (magpi-status--otype section) 'magpi-status)))
    (setq section (magpi-status--oparent section)))
  (magpi-status--ovalue section))

(defun magpi-section-bind-surface (&optional section)
  "Bind surface for SECTION: action, intention, or root."
  (pcase (magpi-facet-type section)
    ((or 'magpi-action 'magpi-ask 'magpi-ask-path 'magpi-ask-paths
         'magpi-observed-file 'magpi-observed-files)
     'action)
    ((or 'magpi-intention 'magpi-bindings)
     'intention)
    (_ 'root)))

(defun magpi-status-toggle-section ()
  "Toggle a detail section; preview the status root without hiding it."
  (interactive)
  (when-let ((section (magit-current-section)))
    (if (eq (magpi-status--otype section) 'magpi-status)
        (magit-section-show-headings section)
      (magit-section-toggle section))))

(defun magpi-status--collect (actions-function intentions-function
                               &optional ref origin)
  "Return (ACTIONS INTENTIONS GIT-FACTS).  Paint never calls this.

Membership and Git are sampled together.  Live overlay is publication,
not another collection.  REF culls to the glanced branch; nil does not."
  (let* ((origin (or origin magpi-status-origin magpi-status-root))
         (magpi-status-head (or magpi-status-head
                                (magpi-status--attached-ref origin)))
         (actions (and actions-function (funcall actions-function)))
         (intentions (and intentions-function (funcall intentions-function))))
    (pcase-let ((`(,actions ,intentions)
                 (magpi-status--related actions intentions ref origin)))
      (list actions intentions
            (magpi-status--snapshot-git-facts intentions)))))

(defun magpi-status--take-sample (&optional peek)
  "Install a collected sample into this buffer.  Paint never calls this.

Nil PEEK matches Magit: membership is the checkout's attached HEAD.
Non-nil PEEK is a glance overlay and does not check the branch out."
  (let ((origin (or magpi-status-origin magpi-status-root)))
    (setq magpi-status-head (magpi-status--attached-ref origin)
          magpi-status-ref (or peek magpi-status-head))
    (pcase-let ((`(,actions ,intentions ,facts)
                 (magpi-status--collect magpi-status-actions-function
                                        magpi-status-intentions-function
                                        magpi-status-ref origin)))
      (setq magpi-status-actions actions
            magpi-status-intentions intentions
            magpi-status-git-facts facts))))

(defun magpi-status--put-branch (table ref annotation)
  (let ((short (and ref (magpi-intention--short-ref ref))))
    (when short
      (puthash short annotation table))))

(defun magpi-status--branch-table (intentions)
  "Short ref → annotation for INTENTIONS plus checkout and glance."
  (let ((table (make-hash-table :test #'equal)))
    (magpi-status--put-branch table magpi-status-head "checkout")
    (dolist (intention intentions)
      (when (magpi-intention-p intention)
        (magpi-status--put-branch
         table (magpi-intention-target-ref intention) "target")
        (magpi-status--put-branch
         table (magpi-intention-branch intention)
         (or (magpi-status--seat-text
              (magpi-intention-objective intention))
             "change"))))
    (when magpi-status-ref
      (let ((short (magpi-intention--short-ref magpi-status-ref)))
        (when (and short (not (gethash short table)))
          (puthash short "glance" table))))
    table))

(defun magpi-status--branch-choices (table)
  (let (choices)
    (maphash (lambda (short note)
               (push (cons (if (and note (not (string-empty-p note)))
                               (format "%s · %s" short note)
                             short)
                           short)
                     choices))
             table)
    (sort choices (lambda (a b) (string< (cdr a) (cdr b))))))

(defun magpi-status--read-branch ()
  (let* ((intentions (and magpi-status-intentions-function
                          (funcall magpi-status-intentions-function)))
         (choices (magpi-status--branch-choices
                   (magpi-status--branch-table intentions))))
    (unless choices
      (user-error "No Magpi branches"))
    (let* ((default (car (rassoc (or magpi-status-ref magpi-status-head)
                                 choices)))
           (pick (completing-read "Peek branch: " (mapcar #'car choices)
                                  nil t nil nil default)))
      (or (cdr (assoc pick choices))
          (user-error "Unknown branch")))))

(defun magpi-status-branch ()
  "Peek another Magpi branch.  Does not check it out.  `g' restores HEAD."
  (interactive)
  (magpi-status--take-sample (magpi-status--read-branch))
  (magit-refresh-buffer))

(defun magpi-status-refresh ()
  "Snapshot transport and Git facts, then refresh the current Magpi status buffer.

This is `g'.  Event paints must call `magit-refresh-buffer' instead."
  (interactive)
  (when magpi-status-prepare-function
    (funcall magpi-status-prepare-function))
  (magpi-status--take-sample)
  (magit-refresh-buffer))

(defun magpi-status--group-section (type)
  "Return the status-root child of TYPE, or nil."
  (when-let ((root (or (and (boundp 'magit-root-section) magit-root-section)
                       (let ((section (magpi-status--section)))
                         (while (and section
                                     (not (eq (magpi-status--otype section)
                                              'magpi-status)))
                           (setq section (magpi-status--oparent section)))
                         section))))
    (and (eieio-object-p root)
         (ignore-errors
           (seq-find (lambda (child)
                       (eq (magpi-status--otype child) type))
                     (oref root children))))))

(defun magpi-status--current-group (&optional section)
  "Return `magpi-perch' or `magpi-cold' in SECTION's lineage, or nil."
  (setq section (magpi-status--section section))
  (while (and section
              (not (memq (magpi-status--otype section)
                         '(magpi-perch magpi-cold magpi-status))))
    (setq section (magpi-status--oparent section)))
  (pcase (magpi-status--otype section)
    ((or 'magpi-perch 'magpi-cold) (magpi-status--otype section))))

(defun magpi-status--jump-to-group (type name)
  (let ((section (magpi-status--group-section type)))
    (unless section
      (user-error "No %s" name))
    (goto-char (oref section start))
    section))

(defun magpi-status-jump ()
  "Toggle point between Perch and Cold.

From Perch, the root, or elsewhere, land on Cold.  From Cold, return to Perch."
  (interactive)
  (if (eq (magpi-status--current-group) 'magpi-cold)
      (magpi-status--jump-to-group 'magpi-perch "Perch")
    (magpi-status--jump-to-group 'magpi-cold "Cold")))

(defun magpi-status--collect-actions (section)
  "Return magpi-action sections under SECTION in display order."
  (let (out)
    (when (eq (magpi-status--otype section) 'magpi-action)
      (setq out (list section)))
    (dolist (child (or (and section (ignore-errors (oref section children)))
                       nil))
      (setq out (nconc out (magpi-status--collect-actions child))))
    out))

(defun magpi-status--action-sections (&optional root)
  "Return chat (action) sections under ROOT, or the status root."
  (let ((root (or root
                  (and (boundp 'magit-root-section) magit-root-section)
                  (let ((section (magpi-status--section)))
                    (while (and section
                                (not (eq (magpi-status--otype section)
                                         'magpi-status)))
                      (setq section (magpi-status--oparent section)))
                    section))))
    (magpi-status--collect-actions root)))

(defun magpi-status--order-last-seen (sections)
  "Return SECTIONS with last-seen live chats first, then the rest."
  (let ((by-id (make-hash-table :test #'equal))
        (ids (and (fboundp 'magpi-last-seen-action-ids)
                  (magpi-last-seen-action-ids)))
        ordered)
    (dolist (section sections)
      (puthash (magpi-status--ovalue section) section by-id))
    (dolist (id ids)
      (when-let ((section (gethash id by-id)))
        (push section ordered)
        (remhash id by-id)))
    (setq ordered (nreverse ordered))
    (dolist (section sections)
      (when (gethash (magpi-status--ovalue section) by-id)
        (setq ordered (nconc ordered (list section)))
        (remhash (magpi-status--ovalue section) by-id)))
    ordered))

(defun magpi-status--goto-section (section)
  (let ((start (and section (ignore-errors (oref section start)))))
    (unless start
      (user-error "No chats"))
    (goto-char start)
    section))

(defun magpi-status--cycle-chat (backward)
  "Move to the next last-seen chat heading.  BACKWARD goes up."
  (let ((sections (magpi-status--order-last-seen
                   (magpi-status--action-sections))))
    (unless sections
      (user-error "No chats"))
    (let* ((current (magpi-section-action-id))
           (pos (or (and current
                         (seq-position sections current
                                       (lambda (section id)
                                         (equal (magpi-status--ovalue section) id))))
                    -1))
           (from (if (and backward (< pos 0)) 0 pos))
           (index (mod (+ from (if backward -1 1)) (length sections))))
      (magpi-status--goto-section (nth index sections)))))

(defun magpi-status-next-chat ()
  "Move down to the next last-seen chat, wrapping."
  (interactive)
  (magpi-status--cycle-chat nil))

(defun magpi-status-previous-chat ()
  "Move up to the previous last-seen chat, wrapping."
  (interactive)
  (magpi-status--cycle-chat t))

(defvar magpi-status--resize-timer nil)

(defconst magpi-status--resize-delay 0.15
  "Debounce for presentation-only resize repaint.")

(defun magpi-status--visible-status-buffers (&optional frame)
  (let (buffers)
    (dolist (fr (if frame (list frame) (frame-list)))
      (dolist (window (window-list fr 'nomini))
        (let ((buffer (window-buffer window)))
          (when (and (buffer-live-p buffer)
                     (not (memq buffer buffers))
                     (with-current-buffer buffer
                       (derived-mode-p 'magpi-status-mode)))
            (push buffer buffers)))))
    (nreverse buffers)))

(defun magpi-status--repaint-visible (&optional _frame)
  "`magit-refresh-buffer' only.  Never `g', Git, or motion.

One debounce; every visible Magpi status buffer is painted."
  (setq magpi-status--resize-timer nil)
  (dolist (buffer (magpi-status--visible-status-buffers))
    (with-current-buffer buffer
      (magit-refresh-buffer))))

(defun magpi-status--on-window-size-change (_frame)
  (when (magpi-status--visible-status-buffers)
    (when (timerp magpi-status--resize-timer)
      (cancel-timer magpi-status--resize-timer))
    (setq magpi-status--resize-timer
          (run-at-time magpi-status--resize-delay nil
                       #'magpi-status--repaint-visible))))

(defvar-keymap magpi-status-mode-map
  :parent special-mode-map
  "RET" #'magpi-visit
  "v" #'magpi-visit-worktree
  "b" #'magpi-status-branch
  "TAB" #'magpi-status-toggle-section
  "C-i" #'magpi-status-toggle-section
  "n" #'magpi-status-next-chat
  "p" #'magpi-status-previous-chat
  "j" #'magpi-status-jump
  "i" #'magpi-intention-create
  "@" #'magpi-bind
  "s" #'magpi-spawn
  "a" #'magpi-react
  "k" #'magpi-discard
  "m" #'magpi-changes-status
  "d" #'magpi-changes-diff
  "l" #'magpi-changes-log
  "c" #'magpi-changes-commit
  "g" #'magpi-status-refresh
  "q" #'quit-window)

(define-derived-mode magpi-status-mode magit-mode "Magpi"
  "Magpi glance composed from Magit sections.  Depth is Magit; React intervenes.

Marks:
  ?  pending Ask            !  fault
  ·····  cold / empty       ◈····  lift
  ·ˋ◈ˊ·  aloft              ˏˋ◇ˎˊ  rest
    *    blood
  ·  empty   ◌  bound   ◉  hears
  w  writer lease           load.  history fill

Colour, Unicode, and motion restate these marks; they do not replace them."
  (setq-local truncate-lines nil)
  (setq-local truncate-partial-width-windows nil)
  (setq-local word-wrap t)
  ;; Magpi uses Magit's section machinery, but is not a Forge buffer, keep it lazy
  (dolist (hook '(forge-set-buffer-repository forge-bug-reference-setup))
    (setq-local magit-mode-hook
                (remove hook (copy-sequence magit-mode-hook))))
  (when (boundp 'magit-setup-buffer-hook)
    (setq-local magit-setup-buffer-hook
                (remove 'magit-set-buffer-margins magit-setup-buffer-hook)))
  (when (boundp 'magit-region-highlight-hook)
    (setq-local magit-region-highlight-hook nil))
  (magpi-status--apply-shades))

(add-hook 'window-size-change-functions #'magpi-status--on-window-size-change)
(when (boundp 'enable-theme-functions)
  (add-hook 'enable-theme-functions #'magpi-status--apply-shades-to-buffers))

(defun magpi-status-open (root actions-function intentions-function
                               &optional prepare-function action-title-function
                               action-loading-function origin)
  "Show ROOT using Magit's transactional status-buffer machinery.

PREPARE-FUNCTION, when non-nil, is the explicit snapshot pull (`g').  Event
paints never call it; orchestration may invoke it once after the buffer opens.
ACTION-TITLE-FUNCTION, when non-nil, supplies a historical session label for an
action without manufacturing Observation.  A cons is (LABEL . LAST).
ACTION-LOADING-FUNCTION, when non-nil, supplies a porcelain loading mark
without manufacturing Observation or recoding auspice.
ORIGIN is the invoking checkout; its attached HEAD is the first glanced branch."
  (pcase-let* ((root (file-name-as-directory (expand-file-name root)))
               (origin (file-name-as-directory
                        (expand-file-name (or origin root))))
               (ref (magpi-status--attached-ref origin))
               (name (format "*Magpi:%s*"
                             (file-name-nondirectory (directory-file-name root))))
               (`(,actions ,intentions ,facts)
                (magpi-status--collect actions-function intentions-function
                                       ref origin)))
    (magit-setup-buffer #'magpi-status-mode nil
      :buffer name
      :directory root
      (magpi-status-root root)
      (magpi-status-origin origin)
      (magpi-status-head ref)
      (magpi-status-ref ref)
      (magpi-status-actions-function actions-function)
      (magpi-status-intentions-function intentions-function)
      (magpi-status-prepare-function prepare-function)
      (magpi-status-action-title-function action-title-function)
      (magpi-status-action-loading-function action-loading-function)
      (magpi-status-actions actions)
      (magpi-status-intentions intentions)
      (magpi-status-git-facts facts))))

(provide 'magpi-status)
;;; magpi-status.el ends here
