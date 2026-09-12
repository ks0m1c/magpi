;;; magpi-status-hours.el --- Lifecycle fixtures and heading seats -*- lexical-binding: t; -*-

;; Not a Magpi surface.  Hours are fixtures; helpers read seats, not paint.

(require 'cl-lib)
(require 'seq)
(require 'magpi-launch)
(require 'magpi-action)
(require 'magpi-intention)
(require 'magpi-status)

(defun magpi-status-test-launch (&optional thinking role)
  (magpi-launch-build "/tmp/" (or thinking 'medium) (or role 'writer)
                      '(:kind none)))

(defun magpi-status-hour-empty ()
  nil)

(defun magpi-status-hour-cold (&optional id)
  (make-magpi-action :id (or id "cold-1") :prompt "Persisted" :created-at 1))

(defun magpi-status-hour-lift (&optional id)
  (make-magpi-action
   :id (or id "lift-1") :prompt "Lift" :launch (magpi-status-test-launch)
   :observation (magpi-observation-initial) :created-at 10))

(defun magpi-status-hour-aloft (&optional id)
  (make-magpi-action
   :id (or id "aloft-1") :prompt "Aloft" :launch (magpi-status-test-launch)
   :observation (make-magpi-observation
                 :activity-state 'running :connection-state 'connected)
   :created-at 20))

(defun magpi-status-hour-rest (&optional id)
  (make-magpi-action
   :id (or id "rest-1") :prompt "Rest" :launch (magpi-status-test-launch)
   :observation (make-magpi-observation
                 :activity-state 'idle :connection-state 'connected)
   :created-at 30))

(defun magpi-status-hour-blood (&optional id)
  (make-magpi-action
   :id (or id "blood-1") :prompt "Blood" :launch (magpi-status-test-launch)
   :observation (make-magpi-observation
                 :activity-state 'running :connection-state 'disconnected)
   :created-at 40))

(defun magpi-status-hour-ask ()
  (make-magpi-action
   :id "ask-1" :prompt "Review" :launch (magpi-status-test-launch)
   :observation (make-magpi-observation
                 :activity-state 'running :connection-state 'connected
                 :asks (list (make-magpi-ask :id "q1" :question "Apply?"
                                             :state 'pending)))))

(defun magpi-status-hour-bound ()
  (make-magpi-action
   :id "bound-1" :prompt "Bound"
   :launch (magpi-launch-build "/tmp/" 'medium 'writer
                               '(:kind point :file "lib/auth.ex" :line 12))
   :observation (magpi-observation-initial)))

(defun magpi-status-hour-history ()
  (make-magpi-action :id "hist-1" :chat-ref "hist-1" :prompt "Persisted"))

(defun magpi-status-hour-crowded ()
  "Action occupying every quiet seat an Action may hold."
  (make-magpi-action
   :id "crowd-1" :prompt "Crowded flight" :created-at 100
   :launch (magpi-status-test-launch 'medium)
   :observation (make-magpi-observation
                 :activity-state 'running :connection-state 'connected
                 :running-model "provider/very-specific-model-id"
                 :usage '(:context 9000 :window 200000)
                 :observed-files '("lib/a.ex"))))

(defun magpi-status-hour-intention (&optional lease)
  (make-magpi-intention
   :id "intent-1" :objective "Ship auth" :state 'active :created-at 50
   :writer-lease (and lease '(:action-id "a1"))))

(defun magpi-status-hour-git-dirty ()
  '(:checkout dirty :dirty t :ahead 3 :behind 1 :exists t))

(defun magpi-status-hour-git-unstarted ()
  '(:checkout unstarted))

(defun magpi-status-hour-quiescent (&optional lease)
  "Active Intention whose change exists and no Action is in flight."
  (magpi-status-hour-intention lease))

(defun magpi-status-view-slot (view slot)
  "Read semantic SLOT from VIEW.  Never parses paint."
  (pcase slot
    ('attention (magpi-status-heading-view-attention view))
    ('title (magpi-status-heading-view-title view))
    ('title-kind (magpi-status-heading-view-title-kind view))
    ('motion (magpi-status-heading-view-motion view))
    ('retention (magpi-status-heading-view-retention view))
    ('lease (magpi-status-heading-view-lease view))
    ('history (magpi-status-heading-view-history view))
    ('git (magpi-status-heading-view-git view))
    ('model (magpi-status-heading-view-model view))
    ('effort (magpi-status-heading-view-effort view))
    ('context (magpi-status-heading-view-context view))
    ('age (magpi-status-heading-view-age view))))

(defun magpi-status-heading-slice (heading start width)
  "Take WIDTH display columns of HEADING from START."
  (let ((heading (or heading ""))
        (from nil)
        (i 0)
        (col 0)
        (end (+ start width)))
    (while (and (< i (length heading)) (< col end))
      (when (= col start)
        (setq from i))
      (setq col (+ col (char-width (aref heading i)))
            i (1+ i)))
    (substring-no-properties heading (or from (length heading)) i)))

(defun magpi-status-plan-slots (plan)
  (mapcar #'car plan))

(defun magpi-status-plan-start (plan slot)
  (nth 1 (magpi-status--plan-slot plan slot)))

(defun magpi-status-heading-seat (heading view slot &optional width)
  "Painted SLOT of HEADING by display column, using VIEW's allocator.
Title and quiet text drop inner pad.  Motion keeps its complete columns."
  (let* ((width (or width magpi-status-layout-width
                    (magpi-status--render-width)))
         (plan (magpi-status--heading-plan view width))
         (item (magpi-status--plan-slot plan slot)))
    (when item
      (let ((cell (magpi-status-heading-slice
                   heading (nth 1 item) (nth 2 item))))
        (cond
         ((eq (nth 3 item) 'right) (string-trim cell))
         ((eq slot 'motion) cell)
         (t (string-trim-right cell)))))))

(defun magpi-status-action-view (action &optional now)
  (magpi-status--action-heading-view action (or now 1000)))

(defun magpi-status-intention-view (intention actions &optional facts now)
  (magpi-status--intention-heading-view intention actions facts (or now 1000)))

(defun magpi-status-render-action (action &optional width now)
  (let ((magpi-status-layout-width (or width magpi-status-layout-width 80)))
    (magpi-status--render-heading (magpi-status-action-view action now))))

(defun magpi-status-render-intention (intention actions &optional facts width now)
  (let ((magpi-status-layout-width (or width magpi-status-layout-width 80)))
    (magpi-status--render-heading
     (magpi-status-intention-view intention actions facts now))))

(provide 'magpi-status-hours)
;;; magpi-status-hours.el ends here
