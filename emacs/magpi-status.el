;;; magpi-status.el --- Glance: intention, action, ask marks -*- lexical-binding: t; -*-

;; One job: glance surfaces as typed Magit sections.  Verbs live in magpi.el.
;; Effects consume semantic magpi-section-* selectors; facet (TYPE . VALUE) is
;; glance-side only.  Magit: the section is the subject.  Lineage is parent.
;; Magit section classes and per-type keymaps remain a later prior.

(require 'cl-lib)
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
(autoload 'magpi-intention-create "magpi" nil t)
(autoload 'magpi-changes-status "magpi" nil t)
(autoload 'magpi-changes-diff "magpi" nil t)
(autoload 'magpi-changes-log "magpi" nil t)
(autoload 'magpi-changes-commit "magpi" nil t)

(defvar-local magpi-status-root nil)
(defvar-local magpi-status-actions-function nil)
(defvar-local magpi-status-intentions-function nil)
(defvar-local magpi-status-prepare-function nil)
(defvar-local magpi-status-action-title-function nil)
(defvar-local magpi-status-action-loading-function nil)
(defvar-local magpi-status-layout-width nil
  "Optional render width override.  Nil uses the narrowest visible window.")
(defcustom magpi-status-title-min-width 18
  "Minimum title seat width in a status heading."
  :type 'integer
  :group 'magpi)

(defcustom magpi-status-title-max-width 52
  "Maximum title seat width in a status heading."
  :type 'integer
  :group 'magpi)

(defcustom magpi-status-meta-target-width 18
  "Meta width reserved before the title seat begins to shrink."
  :type 'integer
  :group 'magpi)

(cl-defstruct (magpi-status-heading-view
               (:constructor magpi-status--make-heading-view))
  "Ephemeral presentation value.  It owns no identity or world state.
Slots are semantic cells (TEXT . FACE); META is a list of cells."
  attention title motion retention meta)
;;; Faces — pigment from auspice.  A theme cannot add an auspice; glyphs unused.
;;
;; identity   why
;; title      observed title
;; pending    lift
;; live       aloft
;; quiet      rest, cold, empty, meta
;; alert      blood
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
  "Lift or pending human judgment."
  :group 'magpi)

(defface magpi-status-quiet
  '((t :inherit shadow))
  "Secondary facts: rest, cold, empty, thinking, model, keys."
  :group 'magpi)

(defface magpi-status-alert
  '((t :inherit error :weight bold))
  "Problem or disconnection."
  :group 'magpi)

(defface magpi-status-evidence
  '((t :inherit font-lock-string-face))
  "Bound context, returned evidence, and observed paths."
  :group 'magpi)

(defun magpi-status--face (text face)
  "Return TEXT with FACE, or TEXT unchanged when blank.

Magit-section enables font-lock with empty keywords, so a `face'-only
property paints once and is then stripped.  Set both `face' and
`font-lock-face', matching Magit's own heading helpers."
  (if (and (stringp text) (not (string-empty-p text)) face)
      (propertize text 'face face 'font-lock-face face)
    text))

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
  "Quiet Perch/Cold heading.  Mark when the glyph holds; else the word."
  (let* ((word (pcase group
                 ('perch "Perch")
                 ('cold "Cold")
                 (_ (capitalize (symbol-name group)))))
         (mark (alist-get group magpi-status-group-cast)))
    (magpi-status--face
     (if (magpi-status--glyph-displayable-p mark 1)
         (concat mark " " word)
       word)
     'magpi-status-quiet)))

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

(defun magpi-status--auspice-motion (auspice)
  "Still motion seat for AUSPICE.  Glyph when the cast holds; else the hour."
  (let ((glyph (alist-get auspice magpi-status-still-cast)))
    (if (magpi-status--cast-displayable-p glyph)
        glyph
      (magpi-status--auspice-word auspice))))

(defcustom magpi-status-reduced-motion nil
  "When non-nil, history fill stays a still word instead of a traveling spark."
  :type 'boolean
  :group 'magpi)

(defconst magpi-status-fetch-cast
  '("·•···" "··•··" "···•·" "····•" "•····")
  "Five-column fetching spark.  Porcelain, not an hour.
The hydrate refresh advances the frame; there is no motion animator.")

(defconst magpi-status-fetch-fallback
  '(".*..." "..*.." "...*." "....*" "*....")
  "ASCII fetching spark when the bullet will not hold.")

(defun magpi-status--fetch-index ()
  (mod (floor (* (float-time) 3)) (length magpi-status-fetch-cast)))

(defun magpi-status--fetch-motion ()
  "Return the 5-column fetching mark.  Still word when reduced-motion."
  (if magpi-status-reduced-motion
      "load."
    (let* ((cast (if (seq-every-p #'magpi-status--cast-displayable-p
                                  magpi-status-fetch-cast)
                     magpi-status-fetch-cast
                   magpi-status-fetch-fallback))
           (frame (nth (magpi-status--fetch-index) cast)))
      (or frame (car cast)))))

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
  "Return ACTION's attention cell.  Fault outranks pending judgment."
  (let ((observation (magpi-action-observation action)))
    (cond
     ((eq (magpi-observation-auspice observation) 'blood)
      (cons "!" 'magpi-status-alert))
     ((magpi-status--pending-ask-p observation)
      (cons "?" 'magpi-status-pending)))))

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

(defun magpi-status--age-label (unix)
  "Quiet age for UNIX seconds.  Orient; do not mint identity."
  (when unix
    (let ((delta (max 0 (- (magpi-status--now) unix))))
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

(defun magpi-status--git-fact-present-p (intention &optional facts)
  (plist-get (or facts (magpi-intention-git-facts intention)) :exists))

(defun magpi-status--subject-glance (intention actions &optional facts)
  "Empty or quiescent for the subject.  Not Action auspice, not stored."
  (cond
   ((and (null intention) (null actions)) 'empty)
   ((and (magpi-intention-active-p intention)
         (not (seq-some #'magpi-status--in-flight-p actions))
         (magpi-status--git-fact-present-p intention facts))
    'quiescent)))

(defun magpi-status--generated-title (action)
  "Return durable title, prompt, context fallback, or New task."
  (or (magpi-action-title action)
      (magpi-action-prompt action)
      (when-let ((launch (magpi-action-launch action)))
        (magpi-launch-context-title (magpi-launch-spec-context launch)))
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

(defun magpi-status--heading (action)
  "Return ACTION's glance title.

Order: observed title → title callback → generated fallback.
The callback may supply a historical session label.  Never manufactures
Observation or changes auspice."
  (let* ((observation (magpi-action-observation action))
         (observed (and observation
                        (magpi-observation-display-title observation)))
         (historical (car (magpi-status--supplied-peek action)))
         (title (or observed historical (magpi-status--generated-title action))))
    (magpi-status--face
     (truncate-string-to-width title 60 nil nil "…")
     (if observed 'magpi-status-title 'magpi-status-identity))))

(defun magpi-status--action-loading (action)
  "Return a porcelain loading label for ACTION, or nil.

Never manufactures Observation or recodes auspice."
  (when magpi-status-action-loading-function
    (let ((loading (funcall magpi-status-action-loading-function action)))
      (and (stringp loading) (not (string-empty-p loading)) loading))))

(defun magpi-status--heading-parts (action)
  "Return motion first, then quiet action meta.

Attention and retention are independent projections in the HeadingView.
History fill is a quiet fetching spark in meta, not an hour."
  (let* ((launch (magpi-action-launch action))
         (observation (magpi-action-observation action))
         (auspice (magpi-observation-auspice observation))
         (running (and observation (magpi-observation-running-model observation)))
         (role (and launch (magpi-launch-spec-role launch)))
         (loading (magpi-status--action-loading action)))
    (delq nil
          (list (cons (magpi-status--auspice-motion auspice)
                      (magpi-status--auspice-face auspice))
                (when loading
                  (cons (magpi-status--fetch-motion) 'magpi-status-quiet))
                (when (and (stringp running) (not (string-empty-p running)))
                  (cons running 'magpi-status-quiet))
                (when launch
                  (cons (magpi-launch-thinking-label
                         (magpi-launch-spec-thinking launch))
                        'magpi-status-quiet))
                (when role
                  (cons (magpi-launch-role-label role)
                        'magpi-status-quiet))
                (when-let ((age (magpi-status--age-label
                                 (magpi-status--action-unix action))))
                  (cons age 'magpi-status-quiet))))))

(defun magpi-status--heading-suffix (action)
  "Return scannable motion then quiet meta for ACTION."
  (mapconcat #'car (magpi-status--heading-parts action) " · "))

(defun magpi-status--format-heading-parts (parts)
  (let ((sep (magpi-status--face " · " 'magpi-status-quiet)))
    (mapconcat (lambda (part)
                 (magpi-status--face (car part) (cdr part)))
               parts
               sep)))

(defun magpi-status--paint-cell (cell)
  (when cell
    (magpi-status--face (car cell) (cdr cell))))

(defun magpi-status--fit-cell (text width)
  "Truncate and pad TEXT to WIDTH display columns, preserving properties."
  (let* ((text (or text ""))
         (fitted (truncate-string-to-width text (max 0 width) nil nil "…"))
         (padding (max 0 (- width (string-width fitted)))))
    (concat fitted (make-string padding ?\s))))

(defun magpi-status--render-width ()
  "Return one width profile shared by every row in the current buffer."
  (or magpi-status-layout-width
      (when-let ((windows (get-buffer-window-list (current-buffer) nil t)))
        (apply #'min (mapcar #'window-body-width windows)))
      80))

(defun magpi-status--render-heading (view)
  "Render ephemeral heading VIEW into stable semantic seats.

The title absorbs width first; meta truncates first on a narrow surface.
Attention, motion, and retention never wander or exchange meaning."
  (let* ((width (max 24 (magpi-status--render-width)))
         (fixed (+ magpi-status-attention-width 1
                   magpi-status-motion-width 1
                   magpi-status-retention-width 2))
         (available (max 0 (- width fixed)))
         (title-min (min available (max 1 magpi-status-title-min-width)))
         (title-width (min (max title-min
                                (- available magpi-status-meta-target-width))
                           magpi-status-title-max-width))
         (meta-width (max 0 (- available title-width)))
         (attention (magpi-status--paint-cell
                     (magpi-status-heading-view-attention view)))
         (title (magpi-status--paint-cell
                 (magpi-status-heading-view-title view)))
         (motion (magpi-status--paint-cell
                  (magpi-status-heading-view-motion view)))
         (retention (magpi-status--paint-cell
                     (magpi-status-heading-view-retention view)))
         (meta (magpi-status--format-heading-parts
                (magpi-status-heading-view-meta view))))
    (concat
     (magpi-status--fit-cell attention magpi-status-attention-width)
     (magpi-status--fit-cell title title-width)
     (magpi-status--face " " 'magpi-status-quiet)
     (magpi-status--fit-cell motion magpi-status-motion-width)
     (magpi-status--face " " 'magpi-status-quiet)
     (magpi-status--fit-cell retention magpi-status-retention-width)
     (when (> meta-width 0)
       (concat (magpi-status--face "  " 'magpi-status-quiet)
               (magpi-status--fit-cell meta meta-width))))))

(defun magpi-status--action-heading-view (action)
  "Project ACTION to a pure heading presentation value."
  (let* ((parts (magpi-status--heading-parts action))
         (retention (magpi-status--action-retention-state action)))
    (magpi-status--make-heading-view
     :attention (magpi-status--action-attention action)
     :title (cons (magpi-status--heading action) nil)
     :motion (car parts)
     :retention (cons (magpi-status--retention-mark retention)
                      (magpi-status--retention-face retention))
     :meta (cdr parts))))

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
           (context (magpi-status--count-label (plist-get usage :context)))
           (window (magpi-status--count-label (plist-get usage :window)))
           (cost (plist-get usage :cost))
           (parts (delq nil
                        (list
                         (cond
                          ((and input output)
                           (format "%s in · %s out" input output))
                          (input (format "%s in" input))
                          (output (format "%s out" output))
                          (total total))
                         (when (and context window)
                           (format "%s/%s ctx" context window))
                         (when (and (numberp cost) (> cost 0))
                           (format "$%.4f" cost))))))
      (when parts (mapconcat #'identity parts " · ")))))

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

(defun magpi-status--insert-pair (user last heading)
  "Insert USER with LAST indented beneath it.  USER matching HEADING is omitted."
  (let* ((user (magpi--one-line user 110))
         (last (magpi--one-line last 110))
         (heading (and (stringp heading) (substring-no-properties heading)))
         (show-user (and user
                         (not (magpi-status--same-glance-p user heading))
                         (not (magpi-status--same-glance-p user last)))))
    (when show-user
      (insert "    " (magpi-status--face user 'magpi-status-identity) "\n"))
    (when (and last (not (magpi-status--same-glance-p last heading)))
      (insert (if show-user "      " "    ")
              (magpi-status--face last 'magpi-status-quiet)
              "\n"))))

(defun magpi-status--ask-state-presentation (state)
  "Return the glance symbol, label, and face for Pi-ask STATE."
  (pcase state
    ('approved '("✓" "answered" magpi-status-live))
    ('rejected '("✕" "rejected" magpi-status-alert))
    ('dismissed '("–" "dismissed" magpi-status-quiet))
    (_ '("?" "ask" magpi-status-pending))))

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
  "Insert ACTION as a typed, foldable Magit section.

The heading is attention, title, motion, retention, then quiet meta.  Fold
is Magit.  The body holds role, activity detail, connection, problem, tokens,
the user/last pair, asks, and observed files."
  (let* ((id (magpi-action-id action))
         (launch (magpi-action-launch action))
         (observation (magpi-action-observation action))
         (role (and launch (magpi-launch-spec-role launch)))
         (heading (magpi-status--heading action)))
    (magit-insert-section (magpi-action id nil)
      (magit-insert-heading
       (magpi-status--render-heading
        (magpi-status--action-heading-view action)))
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
       heading)
      (when observation
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

(defun magpi-status--intention-suffix (intention &optional facts)
  "Return branch and checkout glance for INTENTION.  Missing stays missing."
  (let* ((facts (or facts (magpi-intention-git-facts intention)))
         (branch (magpi-intention-branch intention))
         (checkout (plist-get facts :checkout))
         (ahead (plist-get facts :ahead))
         (behind (plist-get facts :behind)))
    (cond
     ((null branch) "not started")
     ((and ahead behind)
      (format "%s · %s · +%s -%s" branch checkout ahead behind))
     (checkout (format "%s · %s" branch checkout))
     (t branch))))

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
                (equal (car (magpi-status--action-attention action)) "!"))
              actions)
    (cons "!" 'magpi-status-alert))
   ((seq-some (lambda (action)
                (equal (car (magpi-status--action-attention action)) "?"))
              actions)
    (cons "?" 'magpi-status-pending))))

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

(defun magpi-status--intention-heading-parts (intention &optional actions facts)
  "Return stable motion first, then quiet lease, Git, and age meta."
  (let* ((facts (or facts (magpi-intention-git-facts intention)))
         (glance (magpi-status--subject-glance intention actions facts))
         (motion (magpi-status--auspice-motion (or glance 'cold)))
         (git (magpi-status--intention-suffix intention facts)))
    (delq nil
          (list (cons motion (magpi-status--auspice-face (or glance 'cold)))
                (when (magpi-intention-writer-lease intention)
                  (cons "w" 'magpi-status-quiet))
                (when (and (stringp git) (not (string-empty-p git)))
                  (cons git 'magpi-status-quiet))
                (when-let ((age (magpi-status--age-label
                                 (magpi-status--intention-unix intention actions))))
                  (cons age 'magpi-status-quiet))))))

(defun magpi-status--intention-heading-view (intention actions)
  "Project INTENTION and its child ACTIONS to one heading value."
  (let* ((facts (magpi-intention-git-facts intention))
         (parts (magpi-status--intention-heading-parts intention actions facts))
         (retention (magpi-status--intention-retention-state
                     intention actions facts)))
    (magpi-status--make-heading-view
     :attention (magpi-status--intention-attention actions)
     :title (cons (magpi-status--face (magpi-intention-objective intention)
                                      'magpi-status-identity)
                  nil)
     :motion (car parts)
     :retention (cons (magpi-status--retention-mark retention)
                      (magpi-status--retention-face retention))
     :meta (cdr parts))))

(defun magpi-status--insert-intention (intention actions)
  "Insert persisted INTENTION with nested independent ACTIONS."
  (magit-insert-section (magpi-intention (magpi-intention-id intention) nil)
    (magit-insert-heading
     (magpi-status--render-heading
      (magpi-status--intention-heading-view intention actions)))
    (magpi-status--insert-bindings intention)
    (mapc #'magpi-status--insert-action (magpi-status--sort-actions actions))))

(defun magpi-status--record-unix (record)
  (pcase (car record)
    ('action (magpi-status--action-unix (cdr record)))
    ('intention (magpi-status--intention-unix (nth 1 record) (nth 2 record)))))

(defun magpi-status--insert-records (intentions actions)
  "Insert Perch then Cold.  Latest live work at the top.

Perch holds intentions and actions with theatre.  Cold holds standalone
chats without theatre.  Nested actions stay with their intention."
  (let ((groups (make-hash-table :test #'equal))
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
            ('action (magpi-status--insert-action (cdr record)))
            ('intention (magpi-status--insert-intention
                         (nth 1 record) (nth 2 record)))))))
    (when cold-actions
      (magit-insert-section (magpi-cold nil)
        (magit-insert-heading (magpi-status--group-heading 'cold))
        (mapc #'magpi-status--insert-action cold-actions)))
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
    (concat (magpi-status--face huginn
                                (if aloft 'magpi-status-live 'magpi-status-quiet))
            (magpi-status--face " " 'magpi-status-quiet)
            (magpi-status--face (magpi-status--retention-mark retention)
                                (magpi-status--retention-face retention)))))

(defun magpi-status-refresh-buffer ()
  "Render the concise persisted-intention dashboard.

Event paint and Magit refresh call this.  They never snapshot (`g')."
  (let ((actions (and magpi-status-actions-function
                      (funcall magpi-status-actions-function)))
        (intentions (and magpi-status-intentions-function
                         (funcall magpi-status-intentions-function))))
    (magit-insert-section (magpi-status magpi-status-root)
      (magit-insert-heading
       (concat
        (magpi-status--face
         (format "MAGPI · %s"
                 (file-name-nondirectory
                  (directory-file-name magpi-status-root)))
         'magpi-status-header)
        (magpi-status--face "  " 'magpi-status-quiet)
        (magpi-status--root-roost intentions actions)))
      (if (or intentions actions)
          (magpi-status--insert-records intentions actions)
        (insert (magpi-status--face
                 "    i  create intention
    @  bind context
    s  spawn action
    a  react
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

(defun magpi-status-refresh ()
  "Snapshot transport state, then refresh the current Magpi status buffer.

This is `g'.  Event paints must call `magit-refresh-buffer' instead."
  (interactive)
  (when magpi-status-prepare-function
    (funcall magpi-status-prepare-function))
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

(defvar-keymap magpi-status-mode-map
  :parent special-mode-map
  "RET" #'magpi-visit
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
  "Magpi glance composed from Magit sections.  Depth is Magit; React intervenes."
  (setq-local truncate-lines nil)
  (setq-local truncate-partial-width-windows nil)
  (setq-local word-wrap t)
  (when (boundp 'magit-setup-buffer-hook)
    (setq-local magit-setup-buffer-hook
                (remove 'magit-set-buffer-margins magit-setup-buffer-hook)))
  (when (boundp 'magit-region-highlight-hook)
    (setq-local magit-region-highlight-hook nil)))

(defun magpi-status-open (root actions-function intentions-function
                               &optional prepare-function action-title-function
                               action-loading-function)
  "Show ROOT using Magit's transactional status-buffer machinery.

PREPARE-FUNCTION, when non-nil, is the explicit snapshot pull (`g').  Event
paints never call it; orchestration may invoke it once after the buffer opens.
ACTION-TITLE-FUNCTION, when non-nil, supplies a historical session label for an
action without manufacturing Observation.  A cons is (LABEL . LAST).
ACTION-LOADING-FUNCTION, when non-nil, supplies a porcelain loading mark
without manufacturing Observation or recoding auspice."
  (let* ((root (file-name-as-directory (expand-file-name root)))
         (name (format "*Magpi:%s*"
                       (file-name-nondirectory (directory-file-name root)))))
    (magit-setup-buffer #'magpi-status-mode nil
      :buffer name
      :directory root
      (magpi-status-root root)
      (magpi-status-actions-function actions-function)
      (magpi-status-intentions-function intentions-function)
      (magpi-status-prepare-function prepare-function)
      (magpi-status-action-title-function action-title-function)
      (magpi-status-action-loading-function action-loading-function))))

(provide 'magpi-status)
;;; magpi-status.el ends here
