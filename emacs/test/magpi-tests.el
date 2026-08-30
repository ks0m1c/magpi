;;; magpi-tests.el --- Tests for Magpi orchestration -*- lexical-binding: t; -*-

(require 'ert)
(require 'magpi)

(cl-defstruct magpi-test-backend
  listener initial-sent action fail-initial fail-spawn ask-response)

(cl-defmethod magpi-backend-spawn ((backend magpi-test-backend) action listener)
  (setf (magpi-test-backend-action backend) action
        (magpi-test-backend-listener backend) listener)
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
          (not (null (magpi-test-backend-listener backend))))))

(cl-defmethod magpi-backend-ask-supported-p ((_backend magpi-test-backend))
  t)

(cl-defmethod magpi-backend-respond-ask
    ((backend magpi-test-backend) _handle ask-id response)
  (setf (magpi-test-backend-ask-response backend)
        (list ask-id response)))
(ert-deftest magpi-event-listener-captures-only-id-and-replaces-registry-value ()
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
        (cancel-timer magpi--refresh-timer)))))

(ert-deftest magpi-spawn-failure-preserves-attributable-action-and-handle ()
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
        (cancel-timer magpi--refresh-timer)))))

(ert-deftest magpi-partial-spawn-failure-preserves-attributable-action ()
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
        (cancel-timer magpi--refresh-timer)))))

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
                  ((symbol-function 'magpi--new-id)
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
        (cancel-timer magpi--refresh-timer)))))

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
    (puthash "old" older magpi--actions)
    (puthash "new" newer magpi--actions)
    (should (equal (mapcar #'magpi-action-id (magpi--actions-for-root root))
                   '("new" "old")))))

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
                  ((symbol-function 'magpi--new-id)
                   (lambda () "standalone-task")))
          (setq action (magpi-spawn-from-options
                         '(:thinking high :role writer :context-kind none)))
          (should-not (magpi-action-intention-id action))
          (should-not (magpi-action-prompt action))
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
                  ((symbol-function 'magpi--new-id) (lambda () "attached-task")))
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

(provide 'magpi-tests)
;;; magpi-tests.el ends here
