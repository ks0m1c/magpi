;;; magpi-action.el --- Reduce events into an immutable action -*- lexical-binding: t; -*-

;; Copyright (C) 2026 ks0m1c_dharma
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is part of Magpi.

;; One job: reduce semantic events into an immutable action.
;; No Pimacs, Magit, catalog, or launch-menu dependency.

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'magpi-store)

(cl-defstruct magpi-ask
  "Structured Pi-ask fact inside an observation (not a Magpi being).
React answers it; status only glances.  `question' is the ask text."
  id parent-id requester question detail state affected-paths)

(cl-defstruct magpi-observation
  display-title activity-state connection-state activity
  running-model observed-files last-response problem asks
  last-prompt usage)

(cl-defstruct magpi-action
  id title prompt intention-id launch observation started-at
  chat-ref created-at source-root spawn-oid extras)
(defun magpi-observation-initial ()
  "Return the initial observation for a newly declared action.

Birth declares this.  Ordinary telemetry reduction must not."
  (make-magpi-observation :activity-state 'starting
                          :connection-state 'connected))

(defun magpi-observation-auspice (observation)
  "Project OBSERVATION to a glance auspice.  Not stored.

Nil is cold.  Disconnect, unknown activity, or a problem is blood.
Starting is lift; running is aloft; idle is rest; otherwise cold.
Pending asks do not recode.  A kernel without theatre is cold, never lift."
  (cond
   ((null observation) 'cold)
   ((or (eq (magpi-observation-connection-state observation) 'disconnected)
        (eq (magpi-observation-activity-state observation) 'unknown)
        (magpi-observation-problem observation))
    'blood)
   ((eq (magpi-observation-activity-state observation) 'starting) 'lift)
   ((eq (magpi-observation-activity-state observation) 'running) 'aloft)
   ((eq (magpi-observation-activity-state observation) 'idle) 'rest)
   (t 'cold)))

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
           (equal (magpi-observation-last-prompt left)
                  (magpi-observation-last-prompt right))
           (equal (magpi-observation-problem left)
                  (magpi-observation-problem right))
           (equal (magpi-observation-asks left)
                  (magpi-observation-asks right))
           (equal (magpi-observation-usage left)
                  (magpi-observation-usage right)))))

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
identity-preserving and skip effects.  Unknown event types are ignored.
A kernel without Observation stays cold until an explicit connection or
activity fact; model and other telemetry do not mint starting/connected."
  (let* ((current (or (magpi-action-observation action)
                      (make-magpi-observation)))
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
       (let ((prompt (plist-get event :prompt)))
         (when (and (stringp prompt)
                    (not (string-empty-p (string-trim prompt))))
           (setf (magpi-observation-last-prompt observation)
                 (magpi--one-line prompt))
           (unless (magpi-observation-display-title observation)
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
      ('usage-observed
       (when-let ((usage (plist-get event :usage)))
         (when (listp usage)
           (setf (magpi-observation-usage observation) usage))))
      ('disconnected
       (setf (magpi-observation-connection-state observation) 'disconnected
             (magpi-observation-activity-state observation) 'unknown
             (magpi-observation-activity observation) nil))
      ('reconnected
       ;; A new live attempt, not a restated disconnect.  Absent or unknown
       ;; activity returns to lift; a prior problem belongs to the failed connection.
       (setf (magpi-observation-connection-state observation) 'connected
             (magpi-observation-problem observation) nil)
       (when (memq (magpi-observation-activity-state observation)
                   '(nil unknown))
         (setf (magpi-observation-activity-state observation) 'starting
               (magpi-observation-activity observation) nil))))
    (if (magpi-observation-same-p current observation)
        action
      (let ((next (copy-magpi-action action)))
        (setf (magpi-action-observation next) observation)
        next))))

(defconst magpi-action-fields
  '(:id :intention-id :chat-ref :title :created-at :spawn-oid)
  "Coordinates Magpi persists.  Names keep their meaning.")

(defconst magpi-action-absorbed
  '(:version :type :source-root)
  "Classified envelope leftovers.  Not extras; never re-emitted.")

(defconst magpi-action-keys
  (append magpi-action-fields magpi-action-absorbed))

(defun magpi-action--plist (action)
  "Return ACTION as inert persisted data.  Observation and launch stay RAM."
  (let ((intention-id (magpi-action-intention-id action)))
    (magpi-store-plist
     (append
      (list :id (magpi-action-id action)
            :intention-id intention-id
            :chat-ref (magpi-action-chat-ref action)
            :created-at (magpi-action-created-at action))
      (unless intention-id
        (list :title (magpi-action-title action)
              :spawn-oid (magpi-action-spawn-oid action))))
     (magpi-action-extras action))))

(defun magpi-action--from-plist (data root file)
  (when (magpi-unreadable-p data)
    (error "%s" (magpi-unreadable-error data)))
  (let ((id (plist-get data :id)))
    (unless (magpi-store-id-ok id file)
      (error "Invalid Magpi action record"))
    (make-magpi-action
     :id id
     :intention-id (plist-get data :intention-id)
     :chat-ref (plist-get data :chat-ref)
     :title (plist-get data :title)
     :created-at (plist-get data :created-at)
     :source-root (file-name-as-directory (expand-file-name root))
     :spawn-oid (plist-get data :spawn-oid)
     :extras (magpi-store-extras data magpi-action-keys))))

(defun magpi-action-save (action)
  "Persist ACTION when it belongs to a Git repository.  No-op otherwise.

Does not freeze identity: chat-ref and created-at must already be set."
  (unless (and (stringp (magpi-action-chat-ref action))
               (not (string-empty-p (magpi-action-chat-ref action))))
    (error "chat-ref must be frozen before save"))
  (unless (integerp (magpi-action-created-at action))
    (error "created-at must be frozen before save"))
  (when-let ((root (magpi-action-source-root action)))
    (when (magpi-store-common-dir root)
      (magpi-store-write (magpi-store-file root 'actions (magpi-action-id action))
                         (magpi-action--plist action))))
  action)

(defun magpi-action-load (root id)
  (let* ((file (magpi-store-file root 'actions id))
         (data (magpi-store-read file)))
    (cond
     ((and (magpi-unreadable-p data)
           (equal (magpi-unreadable-error data) "absent"))
      nil)
     ((magpi-unreadable-p data) data)
     (t (magpi-action--from-plist data root file)))))

(defun magpi-action-list (root)
  "Return persisted actions for ROOT.  Unreadable files stay in the list."
  (let (records)
    (dolist (entry (magpi-store-list root 'actions))
      (let ((file (car entry))
            (data (cdr entry)))
        (push (if (magpi-unreadable-p data)
                  data
                (condition-case err
                    (magpi-action--from-plist data root file)
                  (error (make-magpi-unreadable
                          :path file :error (error-message-string err)))))
              records)))
    (nreverse records)))

(defun magpi-action-set-chat-ref (action ref)
  "Set ACTION's chat-ref monotonically: nil → REF, never silently REF-a → REF-b."
  (unless (and (stringp ref) (not (string-empty-p (string-trim ref))))
    (error "chat-ref must be a nonempty string"))
  (let ((current (magpi-action-chat-ref action)))
    (cond
     ((equal current ref) action)
     (current
      (error "chat-ref is frozen at %s" current))
     (t
      (let ((next (copy-magpi-action action)))
        (setf (magpi-action-chat-ref next) ref)
        (magpi-action-save next))))))

(provide 'magpi-action)
;;; magpi-action.el ends here
