;;; magpi-core.el --- Functional Magpi domain model -*- lexical-binding: t; -*-

;; This file deliberately has no Pimacs or Magit dependency.

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'magpi-launch)

(cl-defstruct magpi-observation
  display-title activity-state connection-state activity
  running-model observed-files last-response problem usage)

(cl-defstruct magpi-attempt
  id intent launch observation started-at)

(defun magpi-observation-initial ()
  "Return the initial observation for a newly declared attempt."
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
           (equal (magpi-observation-usage left)
                  (magpi-observation-usage right)))))

(defun magpi-observation--add-file (observation path)
  "Add already-normalized project-relative PATH to OBSERVATION once."
  (setf (magpi-observation-observed-files observation)
        (seq-uniq (append (magpi-observation-observed-files observation) (list path))
                  #'string=))
  observation)

(defun magpi--one-line (text &optional width)
  "Collapse nonblank TEXT to one line, limited to WIDTH columns, or nil."
  (let ((line (string-trim
               (replace-regexp-in-string "[\n\r\t ]+" " " (or text "")))))
    (unless (string-empty-p line)
      (truncate-string-to-width line (or width 100) nil nil "…"))))

(defun magpi-attempt-reduce (attempt event)
  "Return a new ATTEMPT after reducing one semantic EVENT.

Neither ATTEMPT nor its observation is mutated. EVENT is a small semantic plist
already normalized by the backend adapter.  If EVENT restates the current
observation, return ATTEMPT itself so listeners can treat reduction as
identity-preserving and skip effects."
  (let* ((current (or (magpi-attempt-observation attempt)
                      (magpi-observation-initial)))
         (observation (copy-magpi-observation current)))
    (pcase (plist-get event :type)
      ('activity-started
       (setf (magpi-observation-activity-state observation) 'running
             (magpi-observation-activity observation)
             (plist-get event :activity)))
      ('activity-ended
       (setf (magpi-observation-activity observation) nil)
       ;; Pi settling establishes current idleness, not attempt completion.
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
      ('usage-observed
       ;; Session totals and context pressure are restated snapshots.  A nil
       ;; :usage is malformed telemetry, not an implicit clear.
       (when-let ((usage (plist-get event :usage)))
         (setf (magpi-observation-usage observation) usage)))
      ('disconnected
       (setf (magpi-observation-connection-state observation) 'disconnected
             (magpi-observation-activity-state observation) 'unknown
             (magpi-observation-activity observation) nil)))
    (if (magpi-observation-same-p current observation)
        attempt
      (let ((next (copy-magpi-attempt attempt)))
        (setf (magpi-attempt-observation next) observation)
        next))))

(provide 'magpi-core)
;;; magpi-core.el ends here
