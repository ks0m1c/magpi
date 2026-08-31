;;; magpi-tests.el --- Tests for Magpi orchestration -*- lexical-binding: t; -*-

(require 'ert)
(require 'magpi)
(require 'magpi-test-repo)

(cl-defstruct magpi-test-backend
  listener initial-sent action fail-initial fail-spawn ask-response visits spawn-count)

(cl-defmethod magpi-backend-spawn ((backend magpi-test-backend) action listener)
  (setf (magpi-test-backend-action backend) action
        (magpi-test-backend-listener backend) listener
        (magpi-test-backend-spawn-count backend)
        (1+ (or (magpi-test-backend-spawn-count backend) 0)))
  ;; A synchronous observation must reduce the registry's stored value, not a
  ;; listener-captured action object.
  (funcall listener '(:type activity-started :activity "thinking"))
  (if (magpi-test-backend-fail-spawn backend)
      (progn
        ;; A process can emit facts before its launch RPC reports failure.
        (funcall listener '(:type response-observed :text "Process started"))
        (error "spawn outcome uncertain"))
    'test-handle))

(cl-defmethod magpi-backend-send-initial ((backend magpi-test-backend) _handle _action)
  (if (magpi-test-backend-fail-initial backend)
      (error "initial delivery failed")
    (setf (magpi-test-backend-initial-sent backend)
          (1+ (or (magpi-test-backend-initial-sent backend) 0)))))

(cl-defmethod magpi-backend-ask-supported-p ((_backend magpi-test-backend))
  t)

(cl-defmethod magpi-backend-respond-ask
    ((backend magpi-test-backend) _handle ask-id response)
  (setf (magpi-test-backend-ask-response backend)
        (list ask-id response)))

(cl-defmethod magpi-backend-visit ((backend magpi-test-backend) handle)
  (setf (magpi-test-backend-visits backend)
        (cons handle (magpi-test-backend-visits backend))))

(cl-defmethod magpi-backend-live-p ((_backend magpi-test-backend) handle)
  (and handle (not (eq handle 'dead))))

(defmacro magpi-test-without-store (&rest body)
  "Keep spawn tests from reading or writing the author's .git/magpi."
  (declare (indent 0) (debug t))
  `(cl-letf (((symbol-function 'magpi-store-common-dir) (lambda (&rest _) nil)))
     ,@body))

(ert-deftest magpi-event-listener-captures-only-id-and-replaces-registry-value ()
  (magpi-test-without-store
  (let* ((backend (make-magpi-test-backend))
         (magpi-backend backend)
         (magpi--actions (make-hash-table :test #'equal))
         (magpi--handles (make-hash-table :test #'equal))
         (magpi--refresh-timer nil)
         (launch (magpi-launch-build default-directory nil 'writer '(:kind none)))
         (action (magpi-spawn-spec "action-atomic" "Inspect this" launch)))
    (unwind-protect
        (let ((stored (gethash "action-atomic" magpi--actions)))
          (should (eq action stored))
          (should-not (eq (magpi-test-backend-action backend) stored))
          (should (eq (magpi-observation-activity-state
                       (magpi-action-observation stored))
                      'running))
          (should (equal (magpi-observation-activity
                          (magpi-action-observation stored))
                         "thinking"))
          (should (magpi-test-backend-initial-sent backend)))
      (when (timerp magpi--refresh-timer)
        (cancel-timer magpi--refresh-timer))))))

(ert-deftest magpi-spawn-failure-preserves-attributable-action-and-handle ()
  (magpi-test-without-store
  (let* ((backend (make-magpi-test-backend :fail-initial t))
         (magpi-backend backend)
         (magpi--actions (make-hash-table :test #'equal))
         (magpi--handles (make-hash-table :test #'equal))
         (magpi--refresh-timer nil)
         (action (magpi-spawn-spec
                   "action-failed" "Inspect this"
                   (magpi-launch-build default-directory nil 'writer
                                       '(:kind none)))))
    (unwind-protect
        (let ((observation (magpi-action-observation action)))
          (should (eq (gethash "action-failed" magpi--handles) 'test-handle))
          (should (equal (magpi-observation-problem observation)
                         "initial delivery failed"))
          (should (eq (magpi-observation-connection-state observation)
                      'disconnected)))
      (when (timerp magpi--refresh-timer)
        (cancel-timer magpi--refresh-timer))))))

(ert-deftest magpi-partial-spawn-failure-preserves-attributable-action ()
  (magpi-test-without-store
  (let* ((backend (make-magpi-test-backend :fail-spawn t))
         (magpi-backend backend)
         (magpi--actions (make-hash-table :test #'equal))
         (magpi--handles (make-hash-table :test #'equal))
         (magpi--refresh-timer nil)
         (action (magpi-spawn-spec
                   "action-uncertain" "Inspect this"
                   (magpi-launch-build default-directory nil 'writer
                                       '(:kind none)))))
    (unwind-protect
        (let ((observation (magpi-action-observation action)))
          (should (eq action (gethash "action-uncertain" magpi--actions)))
          (should-not (gethash "action-uncertain" magpi--handles))
          (should (equal (magpi-action-prompt action) "Inspect this"))
          (should (equal (magpi-observation-problem observation)
                         "spawn outcome uncertain"))
          (should (equal (magpi-observation-last-response observation)
                         "Process started"))
          (should (eq (magpi-observation-connection-state observation)
                      'disconnected)))
      (when (timerp magpi--refresh-timer)
        (cancel-timer magpi--refresh-timer))))))

(ert-deftest magpi-spawn-from-options-freezes-model-and-thinking ()
  (let* ((backend (make-magpi-test-backend))
         (magpi-backend backend)
         (magpi--actions (make-hash-table :test #'equal))
         (magpi--handles (make-hash-table :test #'equal))
         (magpi--refresh-timer nil)
         (magpi-launch--catalog (make-hash-table :test #'equal))
         (magpi-launch--last-model (make-hash-table :test #'equal))
         action)
    (unwind-protect
        (cl-letf (((symbol-function 'magpi--root)
                   (lambda () "/tmp/project/"))
                  ((symbol-function 'magpi--capture-bind)
                   (lambda (_kind) '(:kind none)))
                  ((symbol-function 'magpi-store-new-id)
                   (lambda () "action-options")))
          (setq action
                (magpi-spawn-from-options
                 '(:task "Use model"
                   :thinking high
                   :model "openai/gpt-4.1"
                   :role writer
                   :context-kind none)))
          (let ((launch (magpi-action-launch action)))
            (should (equal (magpi-launch-spec-requested-model launch)
                           "openai/gpt-4.1"))
            (should (eq (magpi-launch-spec-thinking launch) 'high))
            (should (equal (magpi-launch-last-model "/tmp/project/")
                           "openai/gpt-4.1"))))
      (when (timerp magpi--refresh-timer)
        (cancel-timer magpi--refresh-timer)))))

(ert-deftest magpi-spawn-requires-clear-objective-task-semantics ()
  (should-error (magpi-spawn-from-options '(:intent "Repair auth")))
  (should-error (magpi-spawn-from-options '(:intention-id "intent-1")))
  (should-error (magpi-spawn-from-options
                 '(:intention-id "intent-1" :intent "Replace its objective"
                   :task "Implement it"))))

(ert-deftest magpi-restated-events-do-not-reschedule-refresh ()
  (magpi-test-without-store
  (let* ((backend (make-magpi-test-backend))
         (magpi-backend backend)
         (magpi--actions (make-hash-table :test #'equal))
         (magpi--handles (make-hash-table :test #'equal))
         (magpi--refresh-timer nil)
         (action (magpi-spawn-spec
                   "action-idempotent" "Inspect this"
                   (magpi-launch-build default-directory nil 'writer
                                       '(:kind none)))))
    (unwind-protect
        (progn
          (when (timerp magpi--refresh-timer)
            (cancel-timer magpi--refresh-timer)
            (setq magpi--refresh-timer nil))
          (let ((stored (gethash "action-idempotent" magpi--actions)))
            (magpi--handle-event "action-idempotent"
                                 '(:type activity-started :activity "thinking"))
            (should (eq stored (gethash "action-idempotent" magpi--actions)))
            (should-not magpi--refresh-timer)))
      (when (timerp magpi--refresh-timer)
        (cancel-timer magpi--refresh-timer))))))

(ert-deftest magpi-capture-bind-rejects-porcelain ()
  (with-temp-buffer
    (let ((buffer-file-name nil))
      (should-error (magpi--capture-bind 'point))))
  (with-temp-buffer
    (setq buffer-file-name "/tmp/lib/auth.ex")
    (insert "refresh()")
    (goto-char (point-min))
    (should (equal (plist-get (magpi--capture-bind 'point) :text)
                   "refresh()"))))

(ert-deftest magpi-actions-for-root-newest-first ()
  (let* ((root (file-name-as-directory (expand-file-name default-directory)))
         (older (make-magpi-action
                 :id "old"
                 :prompt "Older"
                 :launch (magpi-launch-build root 'medium 'writer
                                             '(:kind none))
                 :started-at '(1 1 0 0)))
         (newer (make-magpi-action
                 :id "new"
                 :prompt "Newer"
                 :launch (magpi-launch-build root 'medium 'writer
                                             '(:kind none))
                 :started-at '(2 2 0 0)))
         (magpi--actions (make-hash-table :test #'equal)))
    (cl-letf (((symbol-function 'magpi-action-list) (lambda (_root) nil)))
      (puthash "old" older magpi--actions)
      (puthash "new" newer magpi--actions)
      (should (equal (mapcar #'magpi-action-id (magpi--actions-for-root root))
                     '("new" "old"))))))

(ert-deftest magpi-routes-explicit-ask-answers-to-the-owning-handle ()
  (let* ((backend (make-magpi-test-backend))
         (magpi-backend backend)
         (magpi--handles (make-hash-table :test #'equal)))
    (puthash "action-1" 'test-handle magpi--handles)
    (magpi--react-answer
     'approved '(:kind ask :action-id "action-1" :ask-id "approval-1"))
    (should (equal (magpi-test-backend-ask-response backend)
                   '("approval-1" approved)))))

(ert-deftest magpi-root-target-visits-backend-root ()
  (let (visited)
    (cl-letf (((symbol-function 'magpi-backend-visit-root)
               (lambda (_backend root) (setq visited root))))
      (let ((magpi-backend (make-magpi-test-backend)))
        (magpi--visit-status-target '(:kind root :root "/tmp/project/"))
        (should (equal visited "/tmp/project/"))))))


(ert-deftest magpi-spawn-global-uses-status-target-at-point ()
  (let (received)
    (with-temp-buffer
      (magpi-status-mode)
      (cl-letf (((symbol-function 'magpi-status-target-at-point)
                 (lambda () '(:kind root :root "/tmp/focused/")))
                ((symbol-function 'magpi-launch)
                 (lambda () (setq received 'launched))))
        (magpi-spawn)
        (should (eq received 'launched))))))

(ert-deftest magpi-command-map-is-the-global-return ()
  (should (eq (lookup-key magpi-command-map "m") #'magpi-status))
  (should (eq (lookup-key magpi-command-map "s") #'magpi-spawn))
  (should (eq (lookup-key magpi-command-map "i") #'magpi-intention-create))
  (should (eq (lookup-key magpi-command-map "@")
              #'magpi-bind))
  ;; Extra I/R/M stay off the primary map.
  (dolist (key '("I" "R" "M"))
    (should-not (lookup-key magpi-command-map key)))
  (should (eq (lookup-key (current-global-map) (kbd "C-c m"))
              magpi-command-map)))

(ert-deftest magpi-disconnection-does-not-release-uncertain-writer-lease ()
  (let* ((id "writer-1")
         (intention (make-magpi-intention
                     :id "intent-1" :objective "Repair auth"
                     :state 'active :writer-lease (list :action-id id)))
         (action (make-magpi-action
                   :id id :intention-id "intent-1"
                   :launch (magpi-launch-build "/tmp/" nil 'writer '(:kind none))
                   :observation (magpi-observation-initial)
                   :started-at (current-time)))
         (magpi--actions (make-hash-table :test #'equal))
         (magpi--intentions (make-hash-table :test #'equal))
         (magpi--refresh-timer nil))
    (unwind-protect
        (progn
          (puthash id action magpi--actions)
          (puthash "intent-1" intention magpi--intentions)
          (magpi--handle-event id '(:type disconnected))
          (should (equal (plist-get (magpi-intention-writer-lease intention) :action-id)
                         id))
          (should (eq (magpi-intention-state intention) 'active)))
      (when (timerp magpi--refresh-timer)
        (cancel-timer magpi--refresh-timer)))))

(ert-deftest magpi-writer-release-is-explicit-and-exact ()
  (let* ((intention (make-magpi-intention
                     :id "intent-1" :objective "Repair auth" :state 'active
                     :worktree-path "/tmp/work/" :branch "magpi/auth" :base-ref "main"
                     :writer-lease '(:action-id "writer-1")))
         (magpi--intentions (make-hash-table :test #'equal))
         released)
    (puthash "intent-1" intention magpi--intentions)
    (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_prompt) t))
              ((symbol-function 'magpi-intention-release-writer)
               (lambda (owner action-id reason)
                 (setq released (list owner action-id reason))
                 owner))
              ((symbol-function 'magpi--schedule-refresh) #'ignore))
      (should-error
       (magpi--react-release
        '(:kind action :action-id "reader-1" :intention-id "intent-1")))
      (magpi--react-release '(:kind intention :intention-id "intent-1"))
      (should (equal released (list intention "writer-1" "operator recovery"))))))

(ert-deftest magpi-standalone-launch-leaves-task-for-chat ()
  (let* ((backend (make-magpi-test-backend))
         (magpi-backend backend)
         (magpi--actions (make-hash-table :test #'equal))
         (magpi--handles (make-hash-table :test #'equal))
         (magpi--refresh-timer nil)
         (magpi-launch--catalog (make-hash-table :test #'equal))
         (magpi-launch--last-model (make-hash-table :test #'equal))
         action)
    (unwind-protect
        (cl-letf (((symbol-function 'magpi--root)
                   (lambda () "/tmp/project/"))
                  ((symbol-function 'magpi--capture-bind)
                   (lambda (_kind) '(:kind none)))
                  ((symbol-function 'magpi-store-new-id)
                   (lambda () "standalone-task")))
          (setq action (magpi-spawn-from-options
                         '(:thinking high :role writer :context-kind none)))
          (should-not (magpi-action-intention-id action))
          (should-not (magpi-action-prompt action))
          (should (equal (magpi-action-chat-ref action) "standalone-task"))
          (should (magpi-test-backend-initial-sent backend)))
      (when (timerp magpi--refresh-timer)
        (cancel-timer magpi--refresh-timer)))))

(ert-deftest magpi-spawn-in-intention-creates-its-worktree-lazily ()
  (let* ((backend (make-magpi-test-backend))
         (magpi-backend backend)
         (magpi--actions (make-hash-table :test #'equal))
         (magpi--handles (make-hash-table :test #'equal))
         (magpi--refresh-timer nil)
         (magpi-launch--catalog (make-hash-table :test #'equal))
         (magpi-launch--last-model (make-hash-table :test #'equal))
         (intention (make-magpi-intention
                     :id "intent-1" :objective "Keep context" :state 'active))
         ensured action)
    (unwind-protect
        (cl-letf (((symbol-function 'magpi--intention) (lambda (_id) intention))
                  ((symbol-function 'magpi-intention-ensure-worktree)
                   (lambda (record)
                     (setq ensured t)
                     (setf (magpi-intention-worktree-path record) "/tmp/project/work/")
                     record))
                  ((symbol-function 'magpi-intention-add-action) #'ignore)
                  ((symbol-function 'magpi--capture-bind)
                   (lambda (_kind) '(:kind none)))
                  ((symbol-function 'magpi-store-new-id) (lambda () "attached-task")))
          (setq action (magpi-spawn-from-options
                         '(:intention-id "intent-1" :role writer :context-kind none)))
          (should ensured)
          (should (equal (magpi-launch-spec-root (magpi-action-launch action))
                         "/tmp/project/work/")))
      (when (timerp magpi--refresh-timer)
        (cancel-timer magpi--refresh-timer)))))
(ert-deftest magpi-react-intention-offers-release-then-settle ()
  (let* ((intention (make-magpi-intention
                     :id "intent-1" :objective "Repair auth" :state 'active
                     :writer-lease '(:action-id "writer-1")))
         (magpi--intentions (make-hash-table :test #'equal)))
    (puthash "intent-1" intention magpi--intentions)
    (should (equal (mapcar #'cdr (magpi-react--choices
                                  '(:kind intention :intention-id "intent-1")))
                   '(release)))
    (setf (magpi-intention-writer-lease intention) nil)
    (should (equal (mapcar #'cdr (magpi-react--choices
                                  '(:kind intention :intention-id "intent-1")))
                   '(merge discard)))))

(ert-deftest magpi-react-merge-lands-in-magit-on-failure ()
  (let* ((intention (make-magpi-intention
                     :id "intent-1" :objective "Repair auth" :state 'active
                     :source-root "/tmp/project/" :base-ref "main"))
         (magpi--intentions (make-hash-table :test #'equal))
         landed)
    (puthash "intent-1" intention magpi--intentions)
    (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_prompt) t))
              ((symbol-function 'magpi-intention-merge)
               (lambda (_id) (user-error "Git merge conflict")))
              ((symbol-function 'magpi--intentions-for-root) #'ignore)
              ((symbol-function 'magpi--schedule-refresh) #'ignore)
              ((symbol-function 'magpi--land-magit)
               (lambda (directory) (setq landed directory))))
      (should-error
       (magpi--react-merge '(:kind intention :intention-id "intent-1")))
      (should (equal landed "/tmp/project/")))))

(ert-deftest magpi-react-discard-names-force ()
  (let* ((intention (make-magpi-intention
                     :id "intent-1" :objective "Throw away" :state 'active
                     :worktree-path "/tmp/work/"))
         (magpi--intentions (make-hash-table :test #'equal))
         prompt discarded)
    (puthash "intent-1" intention magpi--intentions)
    (cl-letf (((symbol-function 'magpi-intention-git-facts)
               (lambda (_intention) '(:checkout dirty)))
              ((symbol-function 'yes-or-no-p)
               (lambda (p) (setq prompt p) t))
              ((symbol-function 'magpi-intention-discard-record)
               (lambda (record) (setq discarded t) record))
              ((symbol-function 'magpi--schedule-refresh) #'ignore))
      (magpi--react-discard '(:kind intention :intention-id "intent-1"))
      (should discarded)
      (should (string-match-p "force-removes" prompt))
      (should (string-match-p "dirty" prompt)))))

(ert-deftest magpi-spawn-spec-freezes-chat-ref-to-action-id ()
  (magpi-test-without-store
  (let* ((backend (make-magpi-test-backend))
         (magpi-backend backend)
         (magpi--actions (make-hash-table :test #'equal))
         (magpi--handles (make-hash-table :test #'equal))
         (magpi--refresh-timer nil)
         (action (magpi-spawn-spec
                   "action-ref" "Inspect this"
                   (magpi-launch-build default-directory nil 'writer
                                       '(:kind none)))))
    (unwind-protect
        (progn
          (should (equal (magpi-action-chat-ref action) "action-ref"))
          (should (magpi-test-backend-initial-sent backend)))
      (when (timerp magpi--refresh-timer)
        (cancel-timer magpi--refresh-timer))))))


(ert-deftest magpi-cold-ret-resumes-same-spawn-path-without-send-initial ()
  (let* ((backend (make-magpi-test-backend))
         (magpi-backend backend)
         (magpi--actions (make-hash-table :test #'equal))
         (magpi--handles (make-hash-table :test #'equal))
         (magpi--refresh-timer nil)
         (action (make-magpi-action
                  :id "cold-1"
                  :chat-ref "cold-1"
                  :source-root default-directory
                  :launch (magpi-launch-build default-directory nil 'writer
                                              '(:kind none)))))
    (unwind-protect
        (progn
          (puthash "cold-1" action magpi--actions)
          (magpi--visit-status-target '(:kind action :action-id "cold-1"))
          (should (eq (gethash "cold-1" magpi--handles) 'test-handle))
          (should-not (magpi-test-backend-initial-sent backend))
          (should-not (magpi-action-started-at (gethash "cold-1" magpi--actions)))
          (should (equal (magpi-action-chat-ref (gethash "cold-1" magpi--actions))
                         "cold-1"))
          (should (equal (magpi-test-backend-visits backend) '(test-handle))))
      (when (timerp magpi--refresh-timer)
        (cancel-timer magpi--refresh-timer)))))

(ert-deftest magpi-live-ret-visits-without-respawn ()
  (let* ((backend (make-magpi-test-backend))
         (magpi-backend backend)
         (magpi--actions (make-hash-table :test #'equal))
         (magpi--handles (make-hash-table :test #'equal)))
    (puthash "live-1" (make-magpi-action :id "live-1") magpi--actions)
    (puthash "live-1" 'test-handle magpi--handles)
    (magpi--visit-status-target '(:kind action :action-id "live-1"))
    (should-not (magpi-test-backend-action backend))
    (should (equal (magpi-test-backend-visits backend) '(test-handle)))))

(ert-deftest magpi-dead-handle-ret-respawns-without-send-initial ()
  (let* ((backend (make-magpi-test-backend))
         (magpi-backend backend)
         (magpi--actions (make-hash-table :test #'equal))
         (magpi--handles (make-hash-table :test #'equal))
         (magpi--refresh-timer nil)
         (action (make-magpi-action
                  :id "dead-1"
                  :chat-ref "dead-1"
                  :launch (magpi-launch-build default-directory nil 'writer
                                              '(:kind none)))))
    (unwind-protect
        (progn
          (puthash "dead-1" action magpi--actions)
          (puthash "dead-1" 'dead magpi--handles)
          (magpi--visit-status-target '(:kind action :action-id "dead-1"))
          (should (eq (gethash "dead-1" magpi--handles) 'test-handle))
          (should-not (magpi-test-backend-initial-sent backend))
          (should-not (magpi-action-started-at (gethash "dead-1" magpi--actions)))
          (should (equal (magpi-test-backend-visits backend) '(test-handle))))
      (when (timerp magpi--refresh-timer)
        (cancel-timer magpi--refresh-timer)))))


(ert-deftest magpi-known-process-state-is-live-dead-or-absent ()
  (let* ((backend (make-magpi-test-backend))
         (magpi-backend backend)
         (magpi--handles (make-hash-table :test #'equal)))
    (should (eq (magpi--known-process-state "missing") 'absent))
    (puthash "a" 'test-handle magpi--handles)
    (should (eq (magpi--known-process-state "a") 'live))
    (puthash "a" 'dead magpi--handles)
    (should (eq (magpi--known-process-state "a") 'dead))))

(ert-deftest magpi-spawn-spec-live-retry-visits-without-second-birth ()
  (magpi-test-without-store
  (let* ((backend (make-magpi-test-backend))
         (magpi-backend backend)
         (magpi--actions (make-hash-table :test #'equal))
         (magpi--handles (make-hash-table :test #'equal))
         (magpi--refresh-timer nil)
         (launch (magpi-launch-build default-directory nil 'writer '(:kind none)))
         first second)
    (unwind-protect
        (progn
          (setq first (magpi-spawn-spec "retry-live" "Inspect this" launch))
          (setq second (magpi-spawn-spec "retry-live" "Inspect this" launch))
          (should (eq first second))
          (should (eq (magpi--known-process-state "retry-live") 'live))
          (should (equal (magpi-test-backend-spawn-count backend) 1))
          (should (equal (magpi-test-backend-initial-sent backend) 1))
          (should (equal (magpi-test-backend-visits backend) '(test-handle))))
      (when (timerp magpi--refresh-timer)
        (cancel-timer magpi--refresh-timer))))))

(ert-deftest magpi-spawn-spec-dead-retry-respawns-without-send-initial ()
  (magpi-test-without-store
  (let* ((backend (make-magpi-test-backend))
         (magpi-backend backend)
         (magpi--actions (make-hash-table :test #'equal))
         (magpi--handles (make-hash-table :test #'equal))
         (magpi--refresh-timer nil)
         (launch (magpi-launch-build default-directory nil 'writer '(:kind none)))
         first second)
    (unwind-protect
        (progn
          (setq first (magpi-spawn-spec "retry-dead" "Inspect this" launch))
          (puthash "retry-dead" 'dead magpi--handles)
          (should (eq (magpi--known-process-state "retry-dead") 'dead))
          (setq second (magpi-spawn-spec "retry-dead" "Inspect this" launch))
          (should (eq first second))
          (should (eq (magpi--known-process-state "retry-dead") 'live))
          (should (equal (magpi-test-backend-spawn-count backend) 2))
          (should (equal (magpi-test-backend-initial-sent backend) 1))
          (should (magpi-action-started-at second)))
      (when (timerp magpi--refresh-timer)
        (cancel-timer magpi--refresh-timer))))))
(ert-deftest magpi-existing-kernel-ensure-process-does-not-reborn-theatre ()
  "Promise: a hydrated kernel is process-retry only; no second birth."
  (let* ((backend (make-magpi-test-backend))
         (magpi-backend backend)
         (magpi--actions (make-hash-table :test #'equal))
         (magpi--handles (make-hash-table :test #'equal))
         (magpi--refresh-timer nil)
         (launch (magpi-launch-build default-directory nil 'writer '(:kind none)))
         (kernel (make-magpi-action
                  :id "kernel-1"
                  :chat-ref "kernel-1"
                  :created-at 42
                  :source-root default-directory)))
    (unwind-protect
        (progn
          (puthash "kernel-1" kernel magpi--actions)
          (magpi-spawn-spec "kernel-1" "must not become prompt" launch)
          (let ((action (gethash "kernel-1" magpi--actions)))
            (should-not (magpi-action-started-at action))
            (should-not (magpi-action-prompt action))
            (should (equal (magpi-action-created-at action) 42))
            (should (equal (magpi-action-chat-ref action) "kernel-1"))
            (should-not (magpi-test-backend-initial-sent backend))
            (should (eq (magpi--known-process-state "kernel-1") 'live))))
      (when (timerp magpi--refresh-timer)
        (cancel-timer magpi--refresh-timer)))))

(ert-deftest magpi-spawn-from-options-births-once-before-process ()
  "Promise: create freezes theatre once; ensure-process does not rebuild."
  (let* ((backend (make-magpi-test-backend))
         (magpi-backend backend)
         (magpi--actions (make-hash-table :test #'equal))
         (magpi--handles (make-hash-table :test #'equal))
         (magpi--refresh-timer nil)
         (magpi-launch--catalog (make-hash-table :test #'equal))
         (magpi-launch--last-model (make-hash-table :test #'equal))
         action born-at)
    (unwind-protect
        (cl-letf (((symbol-function 'magpi--root)
                   (lambda () default-directory))
                  ((symbol-function 'magpi--capture-bind)
                   (lambda (_kind) '(:kind none)))
                  ((symbol-function 'magpi-store-new-id)
                   (lambda () "once-birth"))
                  ((symbol-function 'magpi-action-save)
                   (lambda (a) a)))
          (setq action (magpi-spawn-from-options
                        '(:thinking low :role reader :context-kind none)))
          (setq born-at (magpi-action-started-at action))
          (should born-at)
          (should (eq (magpi-launch-spec-role (magpi-action-launch action)) 'reader))
          (should (equal (magpi-test-backend-spawn-count backend) 1))
          (should (equal (magpi-test-backend-initial-sent backend) 1))
          ;; Process retry must not rebake started-at or re-send initial.
          (puthash "once-birth" 'dead magpi--handles)
          (setq action (magpi-spawn-spec "once-birth" nil
                                         (magpi-action-launch action)))
          (should (eq (magpi-action-started-at action) born-at))
          (should (equal (magpi-test-backend-spawn-count backend) 2))
          (should (equal (magpi-test-backend-initial-sent backend) 1)))
      (when (timerp magpi--refresh-timer)
        (cancel-timer magpi--refresh-timer)))))


(ert-deftest magpi-action-miss-loads-kernel-without-birth ()
  "Promise: table miss loads that id; process retry does not send-initial."
  (magpi-test-with-repo (repository "magpi-action-miss-")
    (let* ((backend (make-magpi-test-backend))
           (magpi-backend backend)
           (magpi--actions (make-hash-table :test #'equal))
           (magpi--handles (make-hash-table :test #'equal))
           (magpi--refresh-timer nil)
           (default-directory repository)
           loaded)
      (unwind-protect
          (cl-letf (((symbol-function 'magpi--root) (lambda () repository)))
            (magpi-action-save
             (make-magpi-action
              :id "miss-1" :chat-ref "miss-1" :created-at 7
              :source-root repository))
            (should-not (gethash "miss-1" magpi--actions))
            (setq loaded (magpi--action "miss-1"))
            (should (magpi-action-p loaded))
            (should-not (magpi-action-observation loaded))
            (should (equal (magpi-action-created-at loaded) 7))
            (magpi-spawn-spec "miss-1" "must not become prompt"
                              (magpi-launch-build repository nil 'writer
                                                  '(:kind none)))
            (should-not (magpi-test-backend-initial-sent backend))
            (should (eq (magpi--known-process-state "miss-1") 'live))
            (should-not (magpi-action-prompt (gethash "miss-1" magpi--actions)))
            (should (equal (magpi-action-created-at
                            (gethash "miss-1" magpi--actions))
                           7)))
        (when (timerp magpi--refresh-timer)
          (cancel-timer magpi--refresh-timer))))))

(ert-deftest magpi-actions-for-root-joins-theatre-over-cold-disk ()
  "Promise: glance joins RAM theatre; cold disk does not write the registry."
  (magpi-test-with-repo (repository "magpi-join-")
    (let* ((magpi--actions (make-hash-table :test #'equal))
           (magpi--handles (make-hash-table :test #'equal))
           (id "join1")
           disk ram joined)
      (setq disk (make-magpi-action
                  :id id :chat-ref id :created-at 1
                  :source-root repository))
      (magpi-action-save disk)
      (setq ram (copy-magpi-action disk))
      (setf (magpi-action-observation ram) (magpi-observation-initial)
            (magpi-action-launch ram)
            (magpi-launch-build repository nil 'writer '(:kind none)))
      (puthash id ram magpi--actions)
      (setq joined (car (magpi--actions-for-root repository)))
      (should (eq ram joined))
      (clrhash magpi--actions)
      (setq joined (car (magpi--actions-for-root repository)))
      (should-not (eq ram joined))
      (should-not (magpi-action-observation joined))
      (should-not (gethash id magpi--actions))
      (should (string-match-p "\\bcold\\b"
                              (magpi-status--heading-suffix joined))))))

(ert-deftest magpi-intention-commands-reread-disk ()
  "Promise: effects load Intention coordinates; stale RAM does not win."
  (magpi-test-with-repo (repository "magpi-intention-reread-")
    (let ((magpi--intentions (make-hash-table :test #'equal))
          (intention (magpi-intention-create-record "Disk why" repository "reread")))
      (cl-letf (((symbol-function 'magpi--root) (lambda () repository)))
        (puthash "reread"
                 (let ((stale (copy-magpi-intention intention)))
                   (setf (magpi-intention-objective stale) "Stale RAM")
                   stale)
                 magpi--intentions)
        (should (equal (magpi-intention-objective (magpi--intention "reread"))
                       "Disk why"))))))

(provide 'magpi-tests)
;;; magpi-tests.el ends here
