;;; magpi-action.el --- Reduce events into an immutable action -*- lexical-binding: t; -*-

;; One job: reduce semantic events into an immutable action.
;; No Pimacs, Magit, catalog, or launch-menu dependency.

(require 'cl-lib)
(require 'seq)
(require 'subr-x)

(cl-defstruct magpi-ask
  "Structured Pi-ask fact inside an observation (not a Magpi being).
React answers it; status only glances.  `question' is the ask text."
  id parent-id requester question detail state affected-paths)

(cl-defstruct magpi-observation
  display-title activity-state connection-state activity
  running-model observed-files last-response problem asks)

(cl-defstruct magpi-action
  id title prompt intention-id launch observation started-at)

(defun magpi-observation-initial ()
  "Return the initial observation for a newly declared action."
  (make-magpi-observation :activity-state 'starting
                          :connection-state 'connected))

(defun magpi-observation-same-p (left right)
  "Return non-nil when LEFT and RIGHT carry the same observed facts.

Restated telemetry is not a new observation.  The reducer uses this so a
duplicate event cannot look like progress and re-enter the refresh loop."
  (or (eq left right)
      (and left right
           (equal (magpi-observation-display-title left)
                  (magpi-observation-display-title right))
           (eq (magpi-observation-activity-state left)
               (magpi-observation-activity-state right))
           (eq (magpi-observation-connection-state left)
               (magpi-observation-connection-state right))
           (equal (magpi-observation-activity left)
                  (magpi-observation-activity right))
           (equal (magpi-observation-running-model left)
                  (magpi-observation-running-model right))
           (equal (magpi-observation-observed-files left)
                  (magpi-observation-observed-files right))
           (equal (magpi-observation-last-response left)
                  (magpi-observation-last-response right))
           (equal (magpi-observation-problem left)
                  (magpi-observation-problem right))
           (equal (magpi-observation-asks left)
                  (magpi-observation-asks right)))))

(defun magpi-observation--add-file (observation path)
  "Add already-normalized project-relative PATH to OBSERVATION once."
  (setf (magpi-observation-observed-files observation)
        (seq-uniq (append (magpi-observation-observed-files observation) (list path))
                  #'string=))
  observation)
(defun magpi--ask-state (state)
  "Normalize structured Pi-ask STATE without inventing a terminal result."
  (let ((state (if (stringp state) (intern (downcase state)) state)))
    (pcase state
      ((or 'approved 'rejected 'dismissed 'pending) state)
      (_ 'pending))))

(defun magpi--ask-value (event existing)
  "Build an immutable Pi-ask value from EVENT, preserving EXISTING facts."
  (let* ((data (or (plist-get event :ask) event))
         (id (plist-get data :id))
         (ask (if existing (copy-magpi-ask existing)
                     (make-magpi-ask :id id))))
    (when (and (stringp id) (not (string-empty-p id)))
      (setf (magpi-ask-id ask) id)
      (when (plist-member data :parent-id)
        (setf (magpi-ask-parent-id ask) (plist-get data :parent-id)))
      (when (plist-member data :requester)
        (setf (magpi-ask-requester ask) (plist-get data :requester)))
      (when (or (plist-member data :question) (plist-member data :ask))
        (setf (magpi-ask-question ask)
              (or (plist-get data :question) (plist-get data :ask))))
      (when (plist-member data :detail)
        (setf (magpi-ask-detail ask) (plist-get data :detail)))
      (when (plist-member data :affected-paths)
        (setf (magpi-ask-affected-paths ask)
              (plist-get data :affected-paths)))
      (when (plist-member data :state)
        (setf (magpi-ask-state ask)
              (magpi--ask-state (plist-get data :state))))
      (unless (magpi-ask-state ask)
        (setf (magpi-ask-state ask) 'pending))
      (setf (magpi-ask-affected-paths ask)
            (seq-uniq (seq-filter #'stringp
                                 (magpi-ask-affected-paths ask))
                      #'string=))
      ask)))

(defun magpi-observation--upsert-ask (observation event)
  "Replace or append the structured Pi-ask in EVENT on OBSERVATION."
  (let* ((data (or (plist-get event :ask) event))
         (id (plist-get data :id))
         (asks (magpi-observation-asks observation))
         (existing (seq-find (lambda (ask)
                              (equal id (magpi-ask-id ask)))
                            asks))
         (next (magpi--ask-value event existing)))
    (when next
      (setf (magpi-observation-asks observation)
            (if existing
                (mapcar (lambda (ask)
                          (if (equal id (magpi-ask-id ask)) next ask))
                        asks)
              (append asks (list next)))))
    observation))
(defun magpi--one-line (text &optional width)
  "Collapse nonblank TEXT to one line, limited to WIDTH columns, or nil."
  (let ((line (string-trim
               (replace-regexp-in-string "[\n\r\t ]+" " " (or text "")))))
    (unless (string-empty-p line)
      (truncate-string-to-width line (or width 100) nil nil "…"))))

(defun magpi-action-reduce (action event)
  "Return a new ACTION after reducing one semantic EVENT.

Neither ACTION nor its observation is mutated. EVENT is a small semantic plist
already normalized by the adapter.  If EVENT restates the current
observation, return ACTION itself so listeners can treat reduction as
identity-preserving and skip effects.  Unknown event types are ignored."
  (let* ((current (or (magpi-action-observation action)
                      (magpi-observation-initial)))
         (observation (copy-magpi-observation current)))
    (pcase (plist-get event :type)
      ('activity-started
       (setf (magpi-observation-activity-state observation) 'running
             (magpi-observation-activity observation)
             (plist-get event :activity)))
      ('activity-ended
       (setf (magpi-observation-activity observation) nil)
       ;; Pi settling establishes current idleness, not action completion.
       (when (plist-get event :idle)
         (setf (magpi-observation-activity-state observation) 'idle)))
      ('file-observed
       ;; The adapter has already normalized PATH.  Do not consult the filesystem
       ;; while reducing immutable data.
       (when-let ((path (plist-get event :path)))
         (setq observation (magpi-observation--add-file observation path))))
      ('response-observed
       (setf (magpi-observation-last-response observation)
             (magpi--one-line (plist-get event :text))))
      ('prompt-observed
       (unless (magpi-observation-display-title observation)
         (let ((prompt (plist-get event :prompt)))
           (when (and (stringp prompt)
                      (not (string-empty-p (string-trim prompt))))
             (setf (magpi-observation-display-title observation) prompt)))))
      ('title-observed
       (let ((title (plist-get event :title)))
         ;; Blank titles are malformed telemetry, not an implicit clear.
         (when (and (stringp title)
                    (not (string-empty-p (string-trim title))))
           (setf (magpi-observation-display-title observation) title))))
      ('model-observed
       (setf (magpi-observation-running-model observation)
             (plist-get event :model)))
      ('problem-observed
       (setf (magpi-observation-problem observation)
             (plist-get event :problem)))
      ((or 'ask-requested 'ask-updated 'ask-resolved)
       ;; Pi-ask facts are structured observation data; chat prose never changes them.
       (setq observation (magpi-observation--upsert-ask observation event)))
      ('disconnected
       (setf (magpi-observation-connection-state observation) 'disconnected
             (magpi-observation-activity-state observation) 'unknown
             (magpi-observation-activity observation) nil)))
    (if (magpi-observation-same-p current observation)
        action
      (let ((next (copy-magpi-action action)))
        (setf (magpi-action-observation next) observation)
        next))))

(provide 'magpi-action)
;;; magpi-action.el ends here
