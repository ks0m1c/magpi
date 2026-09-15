;;; magpi-tests.el --- Tests for Magpi orchestration -*- lexical-binding: t; -*-

(require 'ert)
(require 'magpi)
(require 'magpi-test-repo)

(cl-defstruct magpi-test-backend
  listener initial-sent action fail-initial fail-spawn ask-response visits
  spawn-count terminated)

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

(cl-defmethod magpi-backend-terminate ((backend magpi-test-backend) handle)
  (setf (magpi-test-backend-terminated backend)
        (cons handle (magpi-test-backend-terminated backend))))

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

(ert-deftest magpi-history-pending-paints-without-minting-observation ()
  (let* ((magpi--actions (make-hash-table :test #'equal))
         (magpi--listener-epochs (make-hash-table :test #'equal))
         (magpi--refresh-timer nil)
         (action (make-magpi-action :id "cold-1" :chat-ref "cold-1")))
    (unwind-protect
        (progn
          (puthash "cold-1" action magpi--actions)
          (magpi--revoke-listeners "cold-1")
          (funcall (magpi--listener "cold-1") '(:type history-pending))
          (should (eq (gethash "cold-1" magpi--actions) action))
          (should-not (magpi-action-observation action))
          (should (timerp magpi--refresh-timer)))
      (when (timerp magpi--refresh-timer)
        (cancel-timer magpi--refresh-timer)))))

(ert-deftest magpi-paint-glance-clock-follows-listener ()
  "Promise: operator paint is now; adapter listeners coalesce."
  (let (now magpi--refresh-timer magpi--coalesce-paint)
    (cl-letf (((symbol-function 'magpi--refresh-visible-buffers)
               (lambda () (setq now t))))
      (magpi--paint-glance)
      (should now)
      (should-not magpi--refresh-timer)
      (setq now nil)
      (let ((magpi--coalesce-paint t))
        (unwind-protect
            (progn
              (magpi--paint-glance)
              (should (timerp magpi--refresh-timer))
              (should-not now))
          (when (timerp magpi--refresh-timer)
            (cancel-timer magpi--refresh-timer)))))))

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

(ert-deftest magpi-returned-handle-does-not-clear-spawn-failure-evidence ()
  "Promise: problem and disconnect from this spawn survive a returned handle."
  (magpi-test-without-store
  (let* ((backend (make-magpi-test-backend))
         (magpi-backend backend)
         (magpi--actions (make-hash-table :test #'equal))
         (magpi--handles (make-hash-table :test #'equal))
         (magpi--refresh-timer nil)
         action)
    (unwind-protect
        (cl-letf (((symbol-function 'magpi-backend-spawn)
                   (lambda (_backend action listener)
                     (setf (magpi-test-backend-action backend) action
                           (magpi-test-backend-listener backend) listener
                           (magpi-test-backend-spawn-count backend)
                           (1+ (or (magpi-test-backend-spawn-count backend) 0)))
                     (funcall listener
                              '(:type problem-observed :problem "extension error"))
                     (funcall listener '(:type disconnected))
                     'test-handle)))
          (setq action
                (magpi-spawn-spec
                 "spawn-blood" "Inspect this"
                 (magpi-launch-build default-directory nil 'writer
                                     '(:kind none))))
          (let ((observation (magpi-action-observation action)))
            (should (eq (gethash "spawn-blood" magpi--handles) 'test-handle))
            (should (equal (magpi-observation-problem observation)
                           "extension error"))
            (should (eq (magpi-observation-connection-state observation)
                        'disconnected))
            (should (eq (magpi-observation-auspice observation) 'blood))))
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
         (magpi--actions (make-hash-table :test #'equal))
         (magpi--handles (make-hash-table :test #'equal)))
    (puthash "action-1"
             (make-magpi-action
              :id "action-1"
              :observation (make-magpi-observation
                            :activity-state 'running
                            :connection-state 'connected
                            :asks (list (make-magpi-ask
                                         :id "approval-1"
                                         :question "Apply?"
                                         :state 'pending))))
             magpi--actions)
    (puthash "action-1" 'test-handle magpi--handles)
    (cl-letf (((symbol-function 'magpi--section-action-id)
               (lambda (&optional _) "action-1"))
              ((symbol-function 'magpi--section-ask-id)
               (lambda (&optional _) "approval-1"))
              ((symbol-function 'magpi-section-action-id)
               (lambda (&optional _) "action-1"))
              ((symbol-function 'magpi-section-ask-id)
               (lambda (&optional _) "approval-1")))
      (magpi--react-answer 'approved nil))
    (should (equal (magpi-test-backend-ask-response backend)
                   '("approval-1" approved)))))

(defun magpi-test--surface-from-spec (spec)
  "Infer bind surface from SPEC the way glance selectors would."
  (cond
   ((or (plist-get spec :path) (plist-get spec :ask) (plist-get spec :action))
    'action)
   ((plist-get spec :intention) 'intention)
   (t 'root)))

(defmacro magpi-test-at-section (spec &rest body)
  "Bind Magpi effect-side section selectors from SPEC while BODY runs.

Mocks magpi--section-* helpers so orchestration tests do not depend on
eager glance load or Magit.  Public magpi-section-* symbols are mocked
too for any direct calls."
  (declare (indent 1))
  (let ((surface (magpi-test--surface-from-spec spec)))
    `(cl-letf (((symbol-function 'magpi--section-intention-id)
                (lambda (&optional _) ,(plist-get spec :intention)))
               ((symbol-function 'magpi--section-action-id)
                (lambda (&optional _) ,(plist-get spec :action)))
               ((symbol-function 'magpi--section-ask-id)
                (lambda (&optional _) ,(plist-get spec :ask)))
               ((symbol-function 'magpi--section-path)
                (lambda (&optional _) ,(plist-get spec :path)))
               ((symbol-function 'magpi--section-root)
                (lambda (&optional _) ,(plist-get spec :root)))
               ((symbol-function 'magpi--section-bind-surface)
                (lambda (&optional _) ',surface))
               ((symbol-function 'magpi-section-intention-id)
                (lambda (&optional _) ,(plist-get spec :intention)))
               ((symbol-function 'magpi-section-action-id)
                (lambda (&optional _) ,(plist-get spec :action)))
               ((symbol-function 'magpi-section-ask-id)
                (lambda (&optional _) ,(plist-get spec :ask)))
               ((symbol-function 'magpi-section-path)
                (lambda (&optional _) ,(plist-get spec :path)))
               ((symbol-function 'magpi-section-root)
                (lambda (&optional _) ,(plist-get spec :root)))
               ((symbol-function 'magpi-section-bind-surface)
                (lambda (&optional _) ',surface)))
       ,@body)))

(defun magpi-tests-require-status ()
  "Load glance paint helpers with the unit Magit shim when a test needs them.

Orchestration no longer eagerly requires status; tests that call
`magpi-status-view-slot' must opt in explicitly."
  (unless (boundp 'special-mode-map)
    (defvar special-mode-map (make-sparse-keymap)))
  (unless (require 'magit-mode nil t)
    (defvar magit-mode-map (make-sparse-keymap))
    (define-derived-mode magit-mode fundamental-mode "Magit")
    (defun magit-refresh-buffer () nil)
    (defmacro magit-setup-buffer (&rest _arguments) nil)
    (provide 'magit-mode))
  (unless (require 'magit-section nil t)
    (defun magit-section-value-if (_type) nil)
    (defun magit-current-section () nil)
    (defun magit-section-toggle (_section) nil)
    (defun magit-section-toggle-children (_section) nil)
    (defun magit-section-show-headings (_section) nil)
    (defmacro magit-insert-section (&rest body) `(progn ,@(cdr body)))
    (defun magit-insert-heading (&rest arguments)
      (insert (mapconcat #'identity arguments "") "\n"))
    (provide 'magit-section))
  (require 'magpi-status)
  (require 'magpi-status-hours))

(ert-deftest magpi-root-visits-backend-root ()
  (let (visited)
    (cl-letf (((symbol-function 'magpi-backend-visit-root)
               (lambda (_backend root) (setq visited root))))
      (let ((magpi-backend (make-magpi-test-backend)))
        (magpi-test-at-section (:root "/tmp/project/")
          (magpi-visit))
        (should (equal visited "/tmp/project/"))))))

(ert-deftest magpi-spawn-passes-intention-to-launch-scope ()
  "Promise: spawn hands lineage membership to Transient as an argument."
  (let (scope validated)
    (cl-letf (((symbol-function 'magpi--intention)
               (lambda (id) (setq validated id) id))
              ((symbol-function 'magpi-launch)
               (lambda (&optional id) (setq scope id))))
      (magpi-spawn "intent-1")
      (should (equal validated "intent-1"))
      (should (equal scope "intent-1"))
      (setq validated nil)
      (magpi-spawn)
      (should-not validated)
      (should-not scope))))

(ert-deftest magpi-contextual-bind-surface-matrix ()
  "Effect-side bind surface follows selectors; outside glance defaults to root."
  (magpi-test-at-section (:intention "i")
    (should (eq (magpi--section-bind-surface) 'intention)))
  (magpi-test-at-section (:intention "i" :action "a")
    (should (eq (magpi--section-bind-surface) 'action)))
  (magpi-test-at-section (:action "a" :ask "q")
    (should (eq (magpi--section-bind-surface) 'action)))
  (magpi-test-at-section (:root "/tmp/")
    (should (eq (magpi--section-bind-surface) 'root)))
  (should (eq (magpi--section-bind-surface) 'root)))

(ert-deftest magpi-contextual-visit-matrix ()
  (let (route)
    (cl-letf (((symbol-function 'magpi--changes-action)
               (lambda (action &optional _section)
                 (setq route (list 'magit action))))
              ((symbol-function 'magpi-backend-visit-root)
               (lambda (_backend root) (setq route (list 'root root))))
              ((symbol-function 'magpi-react)
               (lambda (&optional _section) (setq route '(react))))
              ((symbol-function 'magpi--visit-or-open-action)
               (lambda (id) (setq route (list 'chat id))))
              ((symbol-function 'magpi--action)
               (lambda (id) (make-magpi-action :id id :source-root "/tmp/project/")))
              ((symbol-function 'magpi--action-root)
               (lambda (_action) "/tmp/project/"))
              ((symbol-function 'file-in-directory-p) (lambda (_file _root) t))
              ((symbol-function 'find-file)
               (lambda (file) (setq route (list 'file file)))))
      (magpi-test-at-section (:intention "intent-1")
        (magpi-visit)
        (should (equal route '(magit status))))
      (magpi-test-at-section (:root "/tmp/project/")
        (magpi-visit)
        (should (equal route '(root "/tmp/project/"))))
      (magpi-test-at-section (:action "a1" :ask "q1")
        (magpi-visit)
        (should (equal route '(react))))
      (magpi-test-at-section (:action "a1" :ask "q1" :path "lib/auth.ex")
        (magpi-visit)
        (should (equal route '(react))))
      (magpi-test-at-section (:action "a1")
        (magpi-visit)
        (should (equal route '(chat "a1"))))
      (magpi-test-at-section (:action "a1" :path "lib/auth.ex")
        (magpi-visit)
        (should (eq (car route) 'file))
        (should (string-suffix-p "lib/auth.ex" (cadr route)))))))

(ert-deftest magpi-status-from-change-worktree-opens-primary ()
  "Promise: Magpi opened in a change worktree is the repository Magpi."
  (magpi-test-with-repo (repository "magpi-status-glance-root-")
    (let* ((intention (magpi-intention-create-record "Why" repository "why"))
           opened)
      (setq intention (magpi-intention-ensure-worktree intention))
      (cl-letf (((symbol-function 'magpi-status-open)
                 (lambda (root &rest _) (setq opened root)))
                ((symbol-function 'magpi-backend-chat-candidates)
                 (lambda (&rest _) nil))
                ((symbol-function 'magpi--reconcile-actions-for-root)
                 (lambda (&rest _) nil))
                ((symbol-function 'magpi-launch-refresh-catalog)
                 (lambda (&rest _) nil)))
        (let ((default-directory (magpi-intention-worktree-path intention)))
          (magpi-status)
          (should (file-equal-p opened repository)))))))

(ert-deftest magpi-status-from-change-worktree-culls-to-that-branch ()
  "Promise: glance opened in a change worktree keeps that change's branch."
  (magpi-test-with-repo (repository "magpi-status-glance-origin-")
    (let* ((intention (magpi-intention-create-record "Why" repository "why"))
           opened origin)
      (setq intention (magpi-intention-ensure-worktree intention))
      (cl-letf (((symbol-function 'magpi-status-open)
                 (lambda (root &rest rest)
                   (setq opened root origin (nth 5 rest))))
                ((symbol-function 'magpi-backend-chat-candidates)
                 (lambda (&rest _) nil))
                ((symbol-function 'magpi--reconcile-actions-for-root)
                 (lambda (&rest _) nil))
                ((symbol-function 'magpi-launch-refresh-catalog)
                 (lambda (&rest _) nil)))
        (let ((default-directory (magpi-intention-worktree-path intention)))
          (magpi-status)
          (should (file-equal-p opened repository))
          (should (file-equal-p origin default-directory)))))))

(ert-deftest magpi-visit-worktree-opens-intention-checkout ()
  "Promise: v lands in the linked worktree; v again returns to the source checkout."
  (let (route
        (magpi--standing nil)
        (source (file-name-as-directory (file-truename "/tmp/project/")))
        (work (file-name-as-directory (file-truename "/tmp/work/intent-1/"))))
    (cl-letf (((symbol-function 'magpi--intention)
               (lambda (id)
                 (make-magpi-intention :id id :source-root "/tmp/project/")))
              ((symbol-function 'magpi--root) (lambda () "/tmp/project/"))
              ((symbol-function 'magpi-intention-ensure-worktree)
               (lambda (intention &optional _invoking)
                 (push 'ensure route)
                 intention))
              ((symbol-function 'magpi-intention-worktree-path)
               (lambda (_intention) "/tmp/work/intent-1/"))
              ((symbol-function 'dired)
               (lambda (dir) (push (list 'dired dir) route))))
      (magpi-test-at-section (:intention "intent-1")
        (magpi-visit-worktree)
        (should (equal magpi--standing (cons "intent-1" 'work)))
        (should (equal route (list (list 'dired work) 'ensure)))
        (setq route nil)
        (magpi-visit-worktree)
        (should (equal magpi--standing (cons "intent-1" 'source)))
        (should (equal route (list (list 'dired source))))
        (setq route nil)
        (magpi-visit-worktree)
        (should (equal (car route) (list 'dired work))))
      (setq magpi--standing nil route nil)
      (magpi-test-at-section (:intention "intent-1" :action "a1")
        (magpi-visit-worktree)
        (should (equal (car route) (list 'dired work))))
      (magpi-test-at-section (:action "a1")
        (should-error (magpi-visit-worktree)))
      (setq magpi--standing nil route nil)
      (let ((default-directory work))
        (magpi-test-at-section (:intention "intent-1")
          (magpi-visit-worktree)
          (should (equal route (list (list 'dired source)))))))))


(ert-deftest magpi-standing-is-shared-open-context ()
  "Promise: v toggles standing; o reads it; neither git-checkouts."
  (let ((work (file-name-as-directory (file-truename "/tmp/work/intent-1/")))
        (source (file-name-as-directory (file-truename "/tmp/project/")))
        (magpi--standing nil)
        (ensured nil)
        (git nil)
        opened found)
    (cl-letf (((symbol-function 'magpi--intention)
               (lambda (id)
                 (make-magpi-intention :id id :source-root "/tmp/project/")))
              ((symbol-function 'magpi--root) (lambda () "/tmp/project/"))
              ((symbol-function 'magpi-intention-worktree-path)
               (lambda (_intention) "/tmp/work/intent-1/"))
              ((symbol-function 'magpi-intention-ensure-worktree)
               (lambda (intention &optional _)
                 (setq ensured t)
                 intention))
              ((symbol-function 'magpi-git)
               (lambda (&rest args) (push args git) ""))
              ((symbol-function 'dired) #'ignore)
              ((symbol-function 'magpi--orient) #'ignore)
              ((symbol-function 'magpi--open-shell)
               (lambda (dir) (setq opened dir)))
              ((symbol-function 'read-file-name)
               (lambda (_prompt dir &rest _)
                 (expand-file-name "lib.el" dir)))
              ((symbol-function 'find-file)
               (lambda (file) (setq found file))))
      (magpi-test-at-section (:intention "intent-1")
        (should (equal (magpi--change-checkout) work))
        (magpi-shell)
        (should (equal opened work))
        (magpi-visit-worktree)
        (should (equal magpi--standing (cons "intent-1" 'work)))
        (should (eq ensured t))
        (setq ensured nil)
        (magpi-visit-worktree)
        (should (equal magpi--standing (cons "intent-1" 'source)))
        (should (equal (magpi--change-checkout) source))
        (magpi-shell)
        (should (equal opened source))
        (magpi-find-file)
        (should (string-prefix-p source (file-name-directory found)))
        (should-not ensured)
        (should-not git)))))

(ert-deftest magpi-change-checkout-unborn-does-not-ensure-worktree ()
  "Promise: o on an unborn why errors; first action still births the tree."
  (let (ensured)
    (cl-letf (((symbol-function 'magpi--intention)
               (lambda (id)
                 (make-magpi-intention :id id :source-root "/tmp/project/")))
              ((symbol-function 'magpi-intention-worktree-path)
               (lambda (_intention) nil))
              ((symbol-function 'magpi-intention-ensure-worktree)
               (lambda (&rest _) (setq ensured t))))
      (magpi-test-at-section (:intention "intent-1")
        (should-error (magpi--change-checkout))
        (should-not ensured)))))

(ert-deftest magpi-visit-worktree-writer-occupies-source-checkout ()
  "Promise: exclusive writer v checks out magpi/<id> in source; v again restores."
  (let* ((head "main")
         (git nil)
         (route nil)
         (magpi--standing nil)
         (magpi--refresh-timer nil)
         (source (file-name-as-directory (file-truename "/tmp/project/")))
         (intention (make-magpi-intention
                     :id "intent-1"
                     :source-root "/tmp/project/"
                     :target-ref "main"
                     :writer-lease '(:action-id "w1"))))
    (cl-letf (((symbol-function 'magpi--intention) (lambda (_) intention))
              ((symbol-function 'magpi-git--maybe)
               (lambda (_dir &rest args)
                 (pcase args
                   (`("status" "--porcelain" . ,_) "")
                   ('("symbolic-ref" "--quiet" "--short" "HEAD") head)
                   (_ nil))))
              ((symbol-function 'magpi-git)
               (lambda (_dir &rest args)
                 (push args git)
                 (when (eq (car args) 'checkout))
                 (when (and (stringp (car args)) (equal (car args) "checkout"))
                   (setq head (cadr args)))
                 ""))
              ((symbol-function 'magpi-intention-ensure-worktree)
               (lambda (next &optional _) next))
              ((symbol-function 'magpi-intention-worktree-path)
               (lambda (_) "/tmp/work/intent-1/"))
              ((symbol-function 'magpi--intention-live-agent-p) (lambda (_) nil))
              ((symbol-function 'dired) (lambda (dir) (push dir route))))
      (magpi-test-at-section (:intention "intent-1")
        (magpi-visit-worktree)
        (should (equal (car git) '("checkout" "magpi/intent-1")))
        (should (equal (car route) source))
        (should (equal head "magpi/intent-1"))
        (magpi-visit-worktree)
        (should (equal head "main"))
        (should (equal (car route) source))))
    (when (timerp magpi--refresh-timer)
      (cancel-timer magpi--refresh-timer))))

(ert-deftest magpi-visit-worktree-query-blocks-before-git ()
  "Promise: visit query is a fact; blocked never checks out."
  (let* ((git nil)
         (intention (make-magpi-intention
                     :id "intent-1"
                     :source-root "/tmp/project/"
                     :target-ref "main"
                     :writer-lease '(:action-id "w1"))))
    (cl-letf (((symbol-function 'magpi--intention) (lambda (_) intention))
              ((symbol-function 'magpi-git--maybe)
               (lambda (_dir &rest args)
                 (pcase args
                   (`("status" "--porcelain" . ,_) " M lisp.el")
                   ('("symbolic-ref" "--quiet" "--short" "HEAD") "main")
                   (_ nil))))
              ((symbol-function 'magpi-git)
               (lambda (&rest args) (push args git) ""))
              ((symbol-function 'magpi--intention-live-agent-p) (lambda (_) nil)))
      (magpi-test-at-section (:intention "intent-1")
        (let ((state (magpi--visit-worktree-query)))
          (should (eq (plist-get state :action) 'blocked))
          (should (string-match-p "Uncommitted" (plist-get state :error))))
        (should-error (magpi-visit-worktree))
        (should-not git)))))

(ert-deftest magpi-visit-worktree-lifecycle-waits-for-writer ()
  "Promise: live writer visits the garden; occupy runs only after theatre drops."
  (magpi-test-with-repo (repository "magpi-visit-v-life-")
    (let* ((backend (make-magpi-test-backend))
           (magpi-backend backend)
           (magpi--actions (make-hash-table :test #'equal))
           (magpi--handles (make-hash-table :test #'equal))
           (magpi--listener-epochs (make-hash-table :test #'equal))
           (magpi--intentions (make-hash-table :test #'equal))
           (magpi--standing nil)
           (magpi--refresh-timer nil)
           (magpi-launch--catalog (make-hash-table :test #'equal))
           (magpi-launch--last-model (make-hash-table :test #'equal))
           (default-directory repository)
           (dired-at nil)
           (ids '("writer-1")))
      (unwind-protect
          (cl-letf (((symbol-function 'dired)
                     (lambda (dir) (setq dired-at dir)))
                    ((symbol-function 'magpi--root) (lambda () repository))
                    ((symbol-function 'magpi--capture-bind)
                     (lambda (_kind) '(:kind none)))
                    ((symbol-function 'magpi-store-new-id)
                     (lambda () (pop ids))))
            (magpi-intention-create-record "V flow" repository "lease")
            (magpi-spawn-from-options
             '(:intention-id "lease" :role writer :lease t :context-kind none))
            (let* ((intention (magpi--intention "lease"))
                   (work (magpi-intention-worktree-path intention))
                   (branch (magpi-intention-branch intention))
                   (head (lambda ()
                           (magpi-git--maybe repository
                                             "symbolic-ref" "--quiet"
                                             "--short" "HEAD"))))
              (should work)
              (should (file-directory-p work))
              (should (magpi--live-handle "writer-1"))
              (magpi-test-at-section (:intention "lease")
                (should (eq (plist-get (magpi--visit-worktree-query) :action)
                            'open-worktree))
                (magpi-visit-worktree)
                (should (file-equal-p dired-at work))
                (should (magpi-intention--refs-same-p (funcall head) "master"))
                (should (file-directory-p work)))
              (magpi-discard--drop-theatre "writer-1")
              (should-not (magpi--live-handle "writer-1"))
              (magpi-test-at-section (:intention "lease")
                (should (eq (plist-get (magpi--visit-worktree-query) :action)
                            'occupy))
                (magpi-visit-worktree)
                (should (magpi-intention--refs-same-p (funcall head) branch))
                (should (file-equal-p dired-at repository))
                (should-not (file-directory-p work))
                (should (eq (plist-get (magpi--visit-worktree-query) :action)
                            'restore))
                (magpi-visit-worktree)
                (should (magpi-intention--refs-same-p (funcall head) "master"))
                (should (file-directory-p
                         (magpi-intention-worktree-path
                          (magpi--intention "lease")))))))
        (when (timerp magpi--refresh-timer)
          (cancel-timer magpi--refresh-timer))))))
(ert-deftest magpi-contextual-react-choices-matrix ()
  (magpi-test-without-store
    (let* ((magpi--actions (make-hash-table :test #'equal))
           (magpi--intentions (make-hash-table :test #'equal))
           (leased (make-magpi-intention
                    :id "intent-1" :objective "Why" :state 'active
                    :writer-lease '(:action-id "writer-1")))
           (quiet (make-magpi-intention
                   :id "intent-2" :objective "Done" :state 'active))
           (ask (make-magpi-ask :id "q1" :question "Apply?" :state 'pending))
           (asked (make-magpi-action
                   :id "asked" :intention-id "intent-1"
                   :observation (make-magpi-observation
                                 :activity-state 'running
                                 :connection-state 'connected
                                 :asks (list ask))))
           (answered (make-magpi-action
                      :id "answered" :intention-id "intent-1"
                      :observation (make-magpi-observation
                                    :activity-state 'idle
                                    :connection-state 'connected
                                    :asks (list (make-magpi-ask
                                                 :id "q2" :question "Done?"
                                                 :state 'approved)))))
           (blood (make-magpi-action
                   :id "blood" :intention-id "intent-1"
                   :observation (make-magpi-observation
                                 :activity-state 'running
                                 :connection-state 'disconnected
                                 :asks (list ask)))))
      (puthash "intent-1" leased magpi--intentions)
      (puthash "intent-2" quiet magpi--intentions)
      (puthash "asked" asked magpi--actions)
      (puthash "answered" answered magpi--actions)
      (puthash "blood" blood magpi--actions)
      (cl-letf (((symbol-function 'magpi--intention)
                 (lambda (id) (or (gethash id magpi--intentions)
                                  (user-error "missing %s" id)))))
        (magpi-test-at-section (:action "asked" :ask "q1" :intention "intent-1")
          (should (equal (mapcar #'cdr (magpi-react--choices))
                         '(approved rejected))))
        (magpi-test-at-section (:action "asked" :intention "intent-1")
          (should (equal (mapcar #'cdr (magpi-react--choices))
                         '(approved rejected))))
        (magpi-test-at-section (:action "answered" :ask "q2" :intention "intent-1")
          (should-not (magpi-react--choices)))
        (magpi-test-at-section (:action "blood" :ask "q1" :intention "intent-1")
          (should (equal (mapcar #'cdr (magpi-react--choices))
                         '(uncertainty))))
        (magpi-test-at-section (:action "blood" :intention "intent-1")
          (should (equal (mapcar #'cdr (magpi-react--choices))
                         '(uncertainty))))
        (magpi-test-at-section (:intention "intent-1")
          (should (equal (mapcar #'cdr (magpi-react--choices))
                         '(release))))
        (magpi-test-at-section (:intention "intent-2")
          (should (equal (mapcar #'cdr (magpi-react--choices))
                         '(merge open-merge discard))))
        (should-not (magpi-react--choices))
        (should-error (magpi-react) :type 'user-error)))))

(ert-deftest magpi-react-refuses-stale-ask-dead-handle-and-invented-success ()
  (let* ((backend (make-magpi-test-backend))
         (magpi-backend backend)
         (magpi--actions (make-hash-table :test #'equal))
         (magpi--handles (make-hash-table :test #'equal))
         (ask (make-magpi-ask :id "q1" :question "Apply?" :state 'pending))
         (action (make-magpi-action
                  :id "action-1"
                  :observation (make-magpi-observation
                                :activity-state 'running
                                :connection-state 'connected
                                :asks (list ask)))))
    (puthash "action-1" action magpi--actions)
    (magpi-test-at-section (:action "action-1" :ask "q1")
      (should-error (magpi--react-answer 'approved nil) :type 'user-error)
      (should-not (magpi-test-backend-ask-response backend)))
    (puthash "action-1" 'dead magpi--handles)
    (magpi-test-at-section (:action "action-1" :ask "q1")
      (should-error (magpi--react-answer 'approved nil) :type 'user-error)
      (should-not (magpi-test-backend-ask-response backend)))
    (puthash "action-1" 'test-handle magpi--handles)
    (setf (magpi-ask-state ask) 'approved)
    (magpi-test-at-section (:action "action-1" :ask "q1")
      (should-error (magpi--react-answer 'approved nil) :type 'user-error)
      (should-not (magpi-test-backend-ask-response backend)))
    (setf (magpi-ask-state ask) 'pending)
    (magpi-test-at-section (:action "action-1" :ask "q1")
      (magpi--react-answer 'approved nil)
      (should (equal (magpi-test-backend-ask-response backend)
                     '("q1" approved)))
      (should (eq (magpi-ask-state ask) 'pending)))))
(ert-deftest magpi-contextual-changes-matrix ()
  (magpi-test-without-store
    (let* ((magpi--actions (make-hash-table :test #'equal))
           (magpi--intentions (make-hash-table :test #'equal))
           (intention (make-magpi-intention
                       :id "intent-1" :objective "Why" :state 'active
                       :source-root "/tmp/project/"
                       :target-ref "main"))
           (alone (make-magpi-action :id "alone" :source-root "/tmp/project/"
                                     :spawn-oid "abc"))
           magit-root diff-range)
      (puthash "intent-1" intention magpi--intentions)
      (puthash "alone" alone magpi--actions)
      (cl-letf (((symbol-function 'magpi--intention)
                 (lambda (id) (gethash id magpi--intentions)))
                ((symbol-function 'magpi-intention-worktree-path)
                 (lambda (_intention) "/tmp/work/"))
                ((symbol-function 'magpi-intention-work-range)
                 (lambda (_intention) "main..magpi/why"))
                ((symbol-function 'magit-status)
                 (lambda (path) (setq magit-root path)))
                ((symbol-function 'magit-diff-range)
                 (lambda (range _args) (setq diff-range range)))
                ((symbol-function 'magit-log-range) #'ignore)
                ((symbol-function 'magpi--bind-magit-metadata) #'ignore)
                ((symbol-function 'magpi-store-frozen-range)
                 (lambda (_root oid _strict) (concat oid "..HEAD"))))
        (magpi-test-at-section (:intention "intent-1")
          (magpi--changes-action 'status)
          (should (equal magit-root "/tmp/work/")))
        (magpi-test-at-section (:intention "intent-1" :action "nested")
          (magpi--changes-action 'diff)
          (should (equal diff-range "main..magpi/why")))
        (magpi-test-at-section (:action "alone")
          (magpi--changes-action 'status)
          (should (equal magit-root "/tmp/project/"))
          (magpi--changes-action 'diff)
          (should (equal diff-range "abc..HEAD"))
          (should-error (magpi--changes-action 'commit) :type 'user-error))
        (should-error (magpi--changes-action 'status) :type 'user-error)))))

(ert-deftest magpi-command-map-is-the-global-return ()
  (should (eq (lookup-key magpi-command-map "m") #'magpi-status))
  (should (eq (lookup-key magpi-command-map "s") #'magpi-spawn))
  (should (eq (lookup-key magpi-command-map "i") #'magpi-intention-create))
  (should (eq (lookup-key magpi-command-map "@")
              #'magpi-bind))
  (should (eq (lookup-key magpi-command-map "k") #'magpi-discard))
  (should (eq (lookup-key magpi-command-map "v") #'magpi-visit-worktree))
  (should (eq (lookup-key magpi-command-map "o") #'magpi-open))
  (should (eq (lookup-key magpi-open-map "v") #'magpi-visit-worktree))
  (should (eq (lookup-key magpi-open-map "o") #'magpi-shell))
  (should (eq (lookup-key magpi-open-map ".") #'magpi-find-file))
  ;; Extra I/R/M stay off the primary map.
  (dolist (key '("I" "R" "M"))
    (should-not (lookup-key magpi-command-map key)))
  (should (eq (lookup-key (current-global-map) (kbd "C-c m"))
              magpi-command-map)))

(defun magpi-test--chat-buffer (name)
  (with-current-buffer (get-buffer-create name)
    (setq major-mode 'pimacs-chat-mode)
    (current-buffer)))

(ert-deftest magpi-last-seen-action-ids-follow-live-chats ()
  (let ((magpi--handles (make-hash-table :test #'equal))
        a b hidden)
    (unwind-protect
        (progn
          (setq a (magpi-test--chat-buffer "*magpi-chat-a*")
                b (magpi-test--chat-buffer "*magpi-chat-b*")
                hidden (magpi-test--chat-buffer " *magpi-hidden-chat*"))
          (puthash "a" (make-magpi-pimacs-handle :id "a" :chat-buffer a)
                   magpi--handles)
          (puthash "b" (make-magpi-pimacs-handle :id "b" :chat-buffer b)
                   magpi--handles)
          (puthash "h" (make-magpi-pimacs-handle :id "h" :chat-buffer hidden)
                   magpi--handles)
          (cl-letf (((symbol-function 'magpi--live-chats)
                     (lambda () (seq-filter #'buffer-live-p (list b a)))))
            (should (equal (magpi-last-seen-action-ids) '("b" "a")))))
      (dolist (buf (list a b hidden))
        (when (buffer-live-p buf)
          (kill-buffer buf))))))
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
                     :target-ref "main"
                     :writer-lease '(:action-id "writer-1")))
         (magpi--intentions (make-hash-table :test #'equal))
         released)
    (puthash "intent-1" intention magpi--intentions)
    (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_prompt) t))
              ((symbol-function 'magpi-intention-release-writer)
               (lambda (owner action-id reason)
                 (setq released (list owner action-id reason))
                 owner))
              ((symbol-function 'magpi--paint-glance) #'ignore))
      (should-error
       (magpi-test-at-section (:action "reader-1" :intention "intent-1")
         (magpi--react-release)))
      (magpi-test-at-section (:intention "intent-1")
        (magpi--react-release))
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
                   (lambda (record &optional _invoking)
                     (setq ensured t)
                     record))
                  ((symbol-function 'magpi-intention-worktree-path)
                   (lambda (_intention) "/tmp/project/work/"))
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
    (magpi-test-at-section (:intention "intent-1")
      (should (equal (mapcar #'cdr (magpi-react--choices))
                     '(release))))
    (setf (magpi-intention-writer-lease intention) nil)
    (magpi-test-at-section (:intention "intent-1")
      (should (equal (mapcar #'cdr (magpi-react--choices))
                     '(merge open-merge discard))))))

(ert-deftest magpi-react-open-merge-queries-destination-when-vacant ()
  "Promise: vacant checkout of target-ref opens Magit log of that branch."
  (let* ((intention (make-magpi-intention
                     :id "intent-1" :objective "Repair auth" :state 'active
                     :source-root "/tmp/project/" :target-ref "main"))
         (magpi--intentions (make-hash-table :test #'equal))
         surface directory subject)
    (puthash "intent-1" intention magpi--intentions)
    (cl-letf (((symbol-function 'file-directory-p) (lambda (_) t))
              ((symbol-function 'magpi-intention-worktree-path)
               (lambda (_intention) "/tmp/work/"))
              ((symbol-function 'magpi-git)
               (lambda (_dir &rest args)
                 (if (equal (car args) "status") "" "abc")))
              ((symbol-function 'magpi-intention--worktree-for-branch)
               (lambda (&rest _) nil))
              ((symbol-function 'magpi-intention--integrated-p)
               (lambda (&rest _) nil))
              ((symbol-function 'magpi--land)
               (lambda (surf dir &optional subj)
                 (setq surface surf directory dir subject subj))))
      (magpi-test-at-section (:intention "intent-1")
        (magpi--react-open-merge))
      (should (eq surface 'log))
      (should (equal directory "/tmp/work/"))
      (should (equal subject "main..magpi/intent-1"))
      (should (eq (magpi-intention-state intention) 'active)))))

(ert-deftest magpi-react-open-merge-lands-on-dirty-worktree ()
  "Promise: dirty worktree opens Magit there, not the source checkout."
  (let* ((intention (make-magpi-intention
                     :id "intent-1" :objective "Repair auth" :state 'active
                     :source-root "/tmp/project/" :target-ref "main"))
         (magpi--intentions (make-hash-table :test #'equal))
         landed)
    (puthash "intent-1" intention magpi--intentions)
    (cl-letf (((symbol-function 'file-directory-p) (lambda (_) t))
              ((symbol-function 'magpi-intention-worktree-path)
               (lambda (_intention) "/tmp/work/"))
              ((symbol-function 'magpi-git)
               (lambda (dir &rest args)
                 (if (and (equal dir "/tmp/work/")
                          (equal (seq-take args 2) '("status" "--porcelain")))
                     " M file"
                   "")))
              ((symbol-function 'magpi--intentions-for-root) #'ignore)
              ((symbol-function 'magpi--paint-glance) #'ignore)
              ((symbol-function 'magpi--land-magit)
               (lambda (directory) (setq landed directory))))
      (should-error
       (magpi-test-at-section (:intention "intent-1")
         (magpi--react-open-merge)))
      (should (equal landed "/tmp/work/")))))

(ert-deftest magpi-react-open-merge-opens-magit-merge-not-status ()
  "Promise: ready to integrate opens Magit merge on the target checkout."
  (let* ((intention (make-magpi-intention
                     :id "intent-1" :objective "Repair auth" :state 'active
                     :source-root "/tmp/project/" :target-ref "main"))
         (magpi--intentions (make-hash-table :test #'equal))
         surface directory subject)
    (puthash "intent-1" intention magpi--intentions)
    (cl-letf (((symbol-function 'file-directory-p) (lambda (_) t))
              ((symbol-function 'magpi-git) (lambda (&rest _) ""))
              ((symbol-function 'magpi-intention--worktree-for-branch)
               (lambda (&rest _) '(:path "/tmp/project/" :head "abc")))
              ((symbol-function 'magpi-intention--merge-in-progress-p)
               (lambda (_) nil))
              ((symbol-function 'magpi-intention--integrated-p)
               (lambda (&rest _) nil))
              ((symbol-function 'magpi--land)
               (lambda (surf dir &optional subj)
                 (setq surface surf directory dir subject subj))))
      (magpi-test-at-section (:intention "intent-1")
        (magpi--react-open-merge))
      (should (eq surface 'merge))
      (should (equal directory "/tmp/project/"))
      (should (equal subject "magpi/intent-1"))
      (should (eq (magpi-intention-state intention) 'active)))))

(ert-deftest magpi-react-open-merge-does-not-record-when-git-shows-it ()
  "Promise: already-integrated Git does not dispose the why; Merge does."
  (let* ((intention (make-magpi-intention
                     :id "intent-1" :objective "Repair auth" :state 'active
                     :source-root "/tmp/project/" :target-ref "main"))
         (magpi--intentions (make-hash-table :test #'equal))
         landed)
    (puthash "intent-1" intention magpi--intentions)
    (cl-letf (((symbol-function 'file-directory-p) (lambda (_) t))
              ((symbol-function 'magpi-git) (lambda (&rest _) ""))
              ((symbol-function 'magpi-intention--worktree-for-branch)
               (lambda (&rest _) '(:path "/tmp/project/" :head "abc")))
              ((symbol-function 'magpi-intention--merge-in-progress-p)
               (lambda (_) nil))
              ((symbol-function 'magpi-intention--integrated-p)
               (lambda (&rest _) t))
              ((symbol-function 'magpi--paint-glance) #'ignore)
              ((symbol-function 'magpi--land)
               (lambda (&rest _) (setq landed t)))
              ((symbol-function 'magpi--land-magit)
               (lambda (directory) (setq landed directory))))
      (magpi-test-at-section (:intention "intent-1")
        (magpi--react-open-merge))
      (should (eq (magpi-intention-state
                   (gethash "intent-1" magpi--intentions))
                  'active))
      (should-not landed))))

(ert-deftest magpi-react-merge-lands-on-dirty-worktree ()
  "Promise: dirty change opens Magit status there, not the target checkout."
  (let* ((intention (make-magpi-intention
                     :id "intent-1" :objective "Repair auth" :state 'active
                     :source-root "/tmp/project/" :target-ref "main"))
         (magpi--intentions (make-hash-table :test #'equal))
         landed)
    (puthash "intent-1" intention magpi--intentions)
    (cl-letf (((symbol-function 'file-directory-p) (lambda (_) t))
              ((symbol-function 'magpi-intention-worktree-path)
               (lambda (_intention) "/tmp/work/"))
              ((symbol-function 'magpi-git)
               (lambda (dir &rest args)
                 (if (and (equal dir "/tmp/work/")
                          (equal (seq-take args 2) '("status" "--porcelain")))
                     " M file"
                   "")))
              ((symbol-function 'magpi--intentions-for-root) #'ignore)
              ((symbol-function 'magpi--paint-glance) #'ignore)
              ((symbol-function 'magpi--land-magit)
               (lambda (directory) (setq landed directory))))
      (should-error
       (magpi-test-at-section (:intention "intent-1")
         (magpi--react-merge)))
      (should (equal landed "/tmp/work/")))))

(ert-deftest magpi-react-merge-lands-on-dirty-target ()
  "Promise: dirty target checkout opens Magit status there."
  (let* ((intention (make-magpi-intention
                     :id "intent-1" :objective "Repair auth" :state 'active
                     :source-root "/tmp/project/" :target-ref "main"))
         (magpi--intentions (make-hash-table :test #'equal))
         landed)
    (puthash "intent-1" intention magpi--intentions)
    (cl-letf (((symbol-function 'file-directory-p) (lambda (_) t))
              ((symbol-function 'magpi-intention-worktree-path)
               (lambda (_intention) "/tmp/work/"))
              ((symbol-function 'magpi-intention--worktree-for-branch)
               (lambda (&rest _) '(:path "/tmp/project/" :head "abc")))
              ((symbol-function 'magpi-intention--merge-in-progress-p)
               (lambda (_) nil))
              ((symbol-function 'magpi-git)
               (lambda (dir &rest args)
                 (if (and (equal dir "/tmp/project/")
                          (equal (seq-take args 2) '("status" "--porcelain")))
                     " M file"
                   "")))
              ((symbol-function 'magpi--intentions-for-root) #'ignore)
              ((symbol-function 'magpi--paint-glance) #'ignore)
              ((symbol-function 'magpi--land-magit)
               (lambda (directory) (setq landed directory))))
      (should-error
       (magpi-test-at-section (:intention "intent-1")
         (magpi--react-merge)))
      (should (equal landed "/tmp/project/")))))

(ert-deftest magpi-react-merge-lands-status-when-git-fails ()
  "Promise: Git merge failure opens Magit status on the target checkout."
  (let* ((intention (make-magpi-intention
                     :id "intent-1" :objective "Repair auth" :state 'active
                     :source-root "/tmp/project/" :target-ref "main"))
         (magpi--intentions (make-hash-table :test #'equal))
         landed)
    (puthash "intent-1" intention magpi--intentions)
    (cl-letf (((symbol-function 'file-directory-p) (lambda (_) t))
              ((symbol-function 'magpi-intention-worktree-path)
               (lambda (_intention) "/tmp/work/"))
              ((symbol-function 'magpi-intention--worktree-for-branch)
               (lambda (&rest _) '(:path "/tmp/project/" :head "abc")))
              ((symbol-function 'magpi-intention--merge-in-progress-p)
               (lambda (_) nil))
              ((symbol-function 'magpi-intention--integrated-p)
               (lambda (&rest _) nil))
              ((symbol-function 'magpi-git)
               (lambda (_dir &rest args)
                 (when (equal (car args) "merge")
                   (user-error "Git CONFLICT (content)"))
                 ""))
              ((symbol-function 'magpi--intentions-for-root) #'ignore)
              ((symbol-function 'magpi--paint-glance) #'ignore)
              ((symbol-function 'magpi--land-magit)
               (lambda (directory) (setq landed directory))))
      (should-error
       (magpi-test-at-section (:intention "intent-1")
         (magpi--react-merge)))
      (should (equal landed "/tmp/project/"))
      (should (eq (magpi-intention-state intention) 'active)))))

(ert-deftest magpi-react-merge-follows-when-clean ()
  "Promise: clean target receives git merge; Magpi records merged; no Magit lobby."
  (let* ((intention (make-magpi-intention
                     :id "intent-1" :objective "Repair auth" :state 'active
                     :source-root "/tmp/project/" :target-ref "main"))
         (magpi--intentions (make-hash-table :test #'equal))
         merged-at landed)
    (puthash "intent-1" intention magpi--intentions)
    (cl-letf (((symbol-function 'file-directory-p) (lambda (_) t))
              ((symbol-function 'magpi-intention-worktree-path)
               (lambda (_intention) "/tmp/work/"))
              ((symbol-function 'magpi-intention--worktree-for-branch)
               (lambda (&rest _) '(:path "/tmp/project/" :head "abc")))
              ((symbol-function 'magpi-intention--merge-in-progress-p)
               (lambda (_) nil))
              ((symbol-function 'magpi-intention--integrated-p)
               (lambda (&rest _) (and merged-at t)))
              ((symbol-function 'magpi-git)
               (lambda (dir &rest args)
                 (when (equal args '("merge" "--no-edit" "magpi/intent-1"))
                   (setq merged-at dir))
                 ""))
              ((symbol-function 'magpi-intention-set-state)
               (lambda (record state &optional _reason)
                 (setf (magpi-intention-state record) state)
                 record))
              ((symbol-function 'magpi--paint-glance) #'ignore)
              ((symbol-function 'magpi--land-magit)
               (lambda (directory) (setq landed directory))))
      (magpi-test-at-section (:intention "intent-1")
        (magpi--react-merge))
      (should (equal merged-at "/tmp/project/"))
      (should (eq (magpi-intention-state
                   (gethash "intent-1" magpi--intentions))
                  'merged))
      (should-not landed))))

(ert-deftest magpi-react-merge-untracked-on-target-is-not-uncommitted ()
  "Promise: untracked files on the target are not uncommitted change."
  (let* ((intention (make-magpi-intention
                     :id "intent-1" :objective "Repair auth" :state 'active
                     :source-root "/tmp/project/" :target-ref "main"))
         (magpi--intentions (make-hash-table :test #'equal))
         merged-at landed)
    (puthash "intent-1" intention magpi--intentions)
    (cl-letf (((symbol-function 'file-directory-p) (lambda (_) t))
              ((symbol-function 'magpi-intention-worktree-path)
               (lambda (_intention) "/tmp/work/"))
              ((symbol-function 'magpi-intention--worktree-for-branch)
               (lambda (&rest _) '(:path "/tmp/project/" :head "abc")))
              ((symbol-function 'magpi-intention--merge-in-progress-p)
               (lambda (_) nil))
              ((symbol-function 'magpi-intention--integrated-p)
               (lambda (&rest _) (and merged-at t)))
              ((symbol-function 'magpi-git)
               (lambda (dir &rest args)
                 (cond
                  ((equal args '("status" "--porcelain" "--untracked-files=no"))
                   "")
                  ((equal (seq-take args 2) '("status" "--porcelain"))
                   (error "must not treat untracked as uncommitted"))
                  ((equal args '("merge" "--no-edit" "magpi/intent-1"))
                   (setq merged-at dir)
                   "")
                  (t ""))))
              ((symbol-function 'magpi-intention-set-state)
               (lambda (record state &optional _reason)
                 (setf (magpi-intention-state record) state)
                 record))
              ((symbol-function 'magpi--paint-glance) #'ignore)
              ((symbol-function 'magpi--land-magit)
               (lambda (directory) (setq landed directory))))
      (magpi-test-at-section (:intention "intent-1")
        (magpi--react-merge))
      (should (equal merged-at "/tmp/project/"))
      (should (eq (magpi-intention-state
                   (gethash "intent-1" magpi--intentions))
                  'merged))
      (should-not landed))))

(ert-deftest magpi-react-merge-follows-real-git-onto-target ()
  "Promise: React merge fast-forwards the target checkout and records merged."
  (magpi-test-with-repo (repository "magpi-react-merge-")
    (let* ((intention (magpi-intention-create-record "Ship it" repository "ship"))
           (magpi--intentions (make-hash-table :test #'equal))
           work landed)
      (setq intention (magpi-intention-ensure-worktree intention))
      (setq work (magpi-intention-worktree-path intention))
      (puthash "ship" intention magpi--intentions)
      (with-temp-file (expand-file-name "done" work)
        (insert "done\n"))
      (magpi-test-repo-git work "add" "done")
      (magpi-test-repo-git work "commit" "-m" "done")
      (with-temp-file (expand-file-name "todo.org" repository)
        (insert "scratch\n"))
      (cl-letf (((symbol-function 'magpi--land-magit)
                 (lambda (directory) (setq landed directory)))
                ((symbol-function 'magpi--paint-glance) #'ignore))
        (magpi--merge intention)
        (should (eq (magpi-intention-state
                     (gethash "ship" magpi--intentions))
                    'merged))
        (should-not landed)
        (should (file-exists-p (expand-file-name "done" repository)))
        (should (magpi-intention--integrated-p
                 repository "magpi/ship"
                 (magpi-intention-target-ref intention)))))))

(ert-deftest magpi-react-merge-fast-forwards-vacant-destination ()
  "Promise: no checkout of target-ref still fast-forwards that branch."
  (magpi-test-with-repo (repository "magpi-react-merge-vacant-")
    (let* ((intention (magpi-intention-create-record "Ship it" repository "ship"))
           (magpi--intentions (make-hash-table :test #'equal))
           work landed dest)
      (setq intention (magpi-intention-ensure-worktree intention))
      (setq work (magpi-intention-worktree-path intention))
      (puthash "ship" intention magpi--intentions)
      (with-temp-file (expand-file-name "done" work)
        (insert "done\n"))
      (magpi-test-repo-git work "add" "done")
      (magpi-test-repo-git work "commit" "-m" "done")
      (magpi-test-repo-git repository "checkout" "-b" "other")
      (setq dest (magpi-intention-destination intention))
      (should (plist-get dest :oid))
      (should-not (plist-get dest :path))
      (cl-letf (((symbol-function 'magpi--land)
                 (lambda (&rest _) (setq landed t)))
                ((symbol-function 'magpi--paint-glance) #'ignore))
        (magpi--merge intention)
        (should (eq (magpi-intention-state
                     (gethash "ship" magpi--intentions))
                    'merged))
        (should-not landed)
        (should (magpi-intention--integrated-p
                 work "magpi/ship"
                 (magpi-intention-target-ref intention)))
        (should-not (magpi-intention--integrated-p
                     work "magpi/ship" "refs/heads/other"))))))

(ert-deftest magpi-react-open-merge-logs-vacant-destination ()
  "Promise: open merge on a vacant target-ref queries Magit log, stays active."
  (magpi-test-with-repo (repository "magpi-react-open-merge-vacant-")
    (let* ((intention (magpi-intention-create-record "Ship it" repository "ship"))
           (magpi--intentions (make-hash-table :test #'equal))
           work surface directory subject)
      (setq intention (magpi-intention-ensure-worktree intention))
      (setq work (magpi-intention-worktree-path intention))
      (puthash "ship" intention magpi--intentions)
      (with-temp-file (expand-file-name "done" work)
        (insert "done\n"))
      (magpi-test-repo-git work "add" "done")
      (magpi-test-repo-git work "commit" "-m" "done")
      (magpi-test-repo-git repository "checkout" "-b" "other")
      (cl-letf (((symbol-function 'magpi--land)
                 (lambda (surf dir &optional subj)
                   (setq surface surf directory dir subject subj)))
                ((symbol-function 'magpi--paint-glance) #'ignore))
        (magpi--open-merge intention)
        (should (eq surface 'log))
        (should (file-equal-p directory work))
        (should (equal subject "master..magpi/ship"))
        (should (eq (magpi-intention-state
                     (gethash "ship" magpi--intentions))
                    'active))))))

(ert-deftest magpi-react-merge-vacant-non-ff-opens-destination-log ()
  "Promise: a merge commit without a checkout of target-ref stays Magit log."
  (magpi-test-with-repo (repository "magpi-react-merge-vacant-div-")
    (let* ((intention (magpi-intention-create-record "Ship it" repository "ship"))
           (magpi--intentions (make-hash-table :test #'equal))
           work surface subject)
      (setq intention (magpi-intention-ensure-worktree intention))
      (setq work (magpi-intention-worktree-path intention))
      (puthash "ship" intention magpi--intentions)
      (with-temp-file (expand-file-name "done" work)
        (insert "done\n"))
      (magpi-test-repo-git work "add" "done")
      (magpi-test-repo-git work "commit" "-m" "done")
      (with-temp-file (expand-file-name "elsewhere" repository)
        (insert "other\n"))
      (magpi-test-repo-git repository "add" "elsewhere")
      (magpi-test-repo-git repository "commit" "-m" "elsewhere")
      (magpi-test-repo-git repository "checkout" "-b" "other")
      (cl-letf (((symbol-function 'magpi--land)
                 (lambda (surf _dir &optional subj)
                   (setq surface surf subject subj)))
                ((symbol-function 'magpi--paint-glance) #'ignore))
        (should-error (magpi--merge intention) :type 'magpi-blocked)
        (should (eq surface 'log))
        (should (equal subject "master..magpi/ship"))
        (should (eq (magpi-intention-state
                     (gethash "ship" magpi--intentions))
                    'active))
        (should-not (magpi-intention--integrated-p
                     work "magpi/ship"
                     (magpi-intention-target-ref intention)))))))
(ert-deftest magpi-react-discard-names-force ()
  (let* ((intention (make-magpi-intention
                     :id "intent-1" :objective "Throw away" :state 'active))
         (magpi--intentions (make-hash-table :test #'equal))
         prompt discarded)
    (puthash "intent-1" intention magpi--intentions)
    (cl-letf (((symbol-function 'magpi-intention-git-facts)
               (lambda (_intention) '(:checkout dirty)))
              ((symbol-function 'yes-or-no-p)
               (lambda (p) (setq prompt p) t))
              ((symbol-function 'magpi-intention-discard-record)
               (lambda (record) (setq discarded t) record))
              ((symbol-function 'magpi--paint-glance) #'ignore))
      (magpi-test-at-section (:intention "intent-1")
        (magpi--react-discard))
      (should discarded)
      (should (string-match-p "force-removes" prompt))
      (should (string-match-p "dirty" prompt)))))

(ert-deftest magpi-discard-scope-is-contextual ()
  "Promise: discard depth is innermost, like Magit hunk vs file vs list."
  (magpi-test-at-section (:action "cold-1")
    (should (eq (magpi-discard-scope) 'action)))
  (magpi-test-at-section (:action "a1" :intention "intent-1")
    (should (eq (magpi-discard-scope) 'action)))
  (magpi-test-at-section (:intention "intent-1")
    (should (eq (magpi-discard-scope) 'intention)))
  (magpi-test-at-section (:action "a1" :ask "q1" :intention "intent-1")
    (should (eq (magpi-discard-scope) 'ask)))
  (cl-letf (((symbol-function 'derived-mode-p)
             (lambda (&rest modes) (memq 'pimacs-chat-mode modes))))
    (magpi-test-at-section (:action "a1" :intention "intent-1")
      (should (eq (magpi-discard-scope) 'chat))))
  (let ((magpi-intention-metadata '(:intention-id "intent-1")))
    (cl-letf (((symbol-function 'magpi-discard--git-scope-p) (lambda () nil)))
      (should (eq (magpi-discard-scope) 'worktree)))
    (cl-letf (((symbol-function 'magpi-discard--git-scope-p) (lambda () t)))
      (should-not (magpi-discard-scope)))))

(ert-deftest magpi-discard-refuses-ask ()
  (magpi-test-at-section (:action "a1" :ask "q1" :intention "intent-1")
    (should-error (magpi-discard) :type 'user-error)))

(ert-deftest magpi-discard-chat-keeps-writer ()
  "Promise: chat depth terminates the agent and does not release the lease."
  (let* ((backend (make-magpi-test-backend))
         (magpi-backend backend)
         (magpi--actions (make-hash-table :test #'equal))
         (magpi--handles (make-hash-table :test #'equal))
         (magpi--intentions (make-hash-table :test #'equal))
         (intention (make-magpi-intention
                     :id "intent-1" :objective "Keep why" :state 'active
                     :writer-lease '(:action-id "writer-1")))
         (action (make-magpi-action
                  :id "writer-1" :intention-id "intent-1"
                  :observation (make-magpi-observation :activity-state 'running))))
    (puthash "intent-1" intention magpi--intentions)
    (puthash "writer-1" action magpi--actions)
    (puthash "writer-1" 'test-handle magpi--handles)
    (cl-letf (((symbol-function 'derived-mode-p)
               (lambda (&rest modes) (memq 'pimacs-chat-mode modes)))
              ((symbol-function 'yes-or-no-p) (lambda (_prompt) t))
              ((symbol-function 'magpi--paint-glance) #'ignore))
      (magpi-test-at-section (:action "writer-1" :intention "intent-1")
        (should (eq (magpi-discard-scope) 'chat))
        (magpi-discard)))
    (should (equal (magpi-test-backend-terminated backend) '(test-handle)))
    (should-not (gethash "writer-1" magpi--handles))
    (should-not (magpi-action-observation (gethash "writer-1" magpi--actions)))
    (should (equal (plist-get (magpi-intention-writer-lease
                               (gethash "intent-1" magpi--intentions))
                              :action-id)
                   "writer-1"))))

(ert-deftest magpi-discard-revokes-saved-listener ()
  "Promise: a saved adapter callback cannot restore Observation after discard."
  (magpi-test-without-store
  (let* ((backend (make-magpi-test-backend))
         (magpi-backend backend)
         (magpi--actions (make-hash-table :test #'equal))
         (magpi--handles (make-hash-table :test #'equal))
         (magpi--listener-epochs (make-hash-table :test #'equal))
         (magpi--refresh-timer nil)
         (launch (magpi-launch-build default-directory nil 'writer '(:kind none)))
         stale)
    (unwind-protect
        (progn
          (magpi-spawn-spec "stale-1" "Inspect this" launch)
          (setq stale (magpi-test-backend-listener backend))
          (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_prompt) t)))
            (magpi-test-at-section (:action "stale-1")
              (magpi-discard)))
          (should-not (magpi-action-observation (gethash "stale-1" magpi--actions)))
          (funcall stale '(:type model-observed :model "openai/gpt-4.1"))
          (funcall stale '(:type activity-started :activity "thinking"))
          (should-not (magpi-action-observation (gethash "stale-1" magpi--actions)))
          (should-not (gethash "stale-1" magpi--handles)))
      (when (timerp magpi--refresh-timer)
        (cancel-timer magpi--refresh-timer))))))
(ert-deftest magpi-discard-action-releases-writer ()
  "Promise: action depth drops chat and releases this doing's writer."
  (let* ((backend (make-magpi-test-backend))
         (magpi-backend backend)
         (magpi--actions (make-hash-table :test #'equal))
         (magpi--handles (make-hash-table :test #'equal))
         (magpi--intentions (make-hash-table :test #'equal))
         (intention (make-magpi-intention
                     :id "intent-1" :objective "Repair auth" :state 'active
                     :writer-lease '(:action-id "writer-1")))
         (action (make-magpi-action
                  :id "writer-1" :intention-id "intent-1"
                  :observation (make-magpi-observation :activity-state 'running)))
         released)
    (puthash "intent-1" intention magpi--intentions)
    (puthash "writer-1" action magpi--actions)
    (puthash "writer-1" 'test-handle magpi--handles)
    (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_prompt) t))
              ((symbol-function 'magpi--paint-glance) #'ignore)
              ((symbol-function 'magpi--intention)
               (lambda (id) (gethash id magpi--intentions)))
              ((symbol-function 'magpi-intention-release-writer)
               (lambda (record action-id reason)
                 (setq released (list action-id reason))
                 (let ((next (copy-magpi-intention record)))
                   (setf (magpi-intention-writer-lease next) nil)
                   next))))
      (magpi-test-at-section (:action "writer-1" :intention "intent-1")
        (magpi-discard)))
    (should (equal (magpi-test-backend-terminated backend) '(test-handle)))
    (should (equal released '("writer-1" "discard")))
    (should-not (magpi-intention-writer-lease
                 (gethash "intent-1" magpi--intentions)))
    (should (eq (magpi-intention-state (gethash "intent-1" magpi--intentions))
                'active))))

(ert-deftest magpi-discard-intention-terminates-children ()
  "Promise: intention depth is deep: nested theatre first, then force-remove."
  (let* ((backend (make-magpi-test-backend))
         (magpi-backend backend)
         (magpi--actions (make-hash-table :test #'equal))
         (magpi--handles (make-hash-table :test #'equal))
         (magpi--intentions (make-hash-table :test #'equal))
         (intention (make-magpi-intention
                     :id "intent-1" :objective "Throw away" :state 'active
                     :writer-lease '(:action-id "writer-1")))
         (writer (make-magpi-action :id "writer-1" :intention-id "intent-1"))
         (reader (make-magpi-action :id "reader-1" :intention-id "intent-1"))
         prompt discarded released)
    (puthash "intent-1" intention magpi--intentions)
    (puthash "writer-1" writer magpi--actions)
    (puthash "reader-1" reader magpi--actions)
    (puthash "writer-1" 'writer-handle magpi--handles)
    (puthash "reader-1" 'reader-handle magpi--handles)
    (cl-letf (((symbol-function 'magpi-intention-git-facts)
               (lambda (_intention) '(:checkout dirty)))
              ((symbol-function 'yes-or-no-p)
               (lambda (p) (setq prompt p) t))
              ((symbol-function 'magpi-intention-release-writer)
               (lambda (record action-id _reason)
                 (push action-id released)
                 (let ((next (copy-magpi-intention record)))
                   (setf (magpi-intention-writer-lease next) nil)
                   next)))
              ((symbol-function 'magpi-intention-discard-record)
               (lambda (record)
                 (setq discarded t)
                 (let ((next (copy-magpi-intention record)))
                   (setf (magpi-intention-state next) 'discarded)
                   next)))
              ((symbol-function 'magpi--paint-glance) #'ignore)
              ((symbol-function 'magpi--intention)
               (lambda (id) (gethash id magpi--intentions))))
      (magpi-test-at-section (:intention "intent-1")
        (magpi-discard)))
    (should discarded)
    (should (member "writer-1" released))
    (should (memq 'reader-handle (magpi-test-backend-terminated backend)))
    (should (memq 'writer-handle (magpi-test-backend-terminated backend)))
    (should (= 2 (length (magpi-test-backend-terminated backend))))
    (should-not (gethash "writer-1" magpi--handles))
    (should (eq (magpi-intention-state (gethash "intent-1" magpi--intentions))
                'discarded))
    (should (string-match-p "force-removes" prompt))
    (should (string-match-p "2 actions" prompt))))

(ert-deftest magpi-discard-leftover-worktree-terminates-children ()
  "Promise: leftover checkout k is still nested: theatre first, then remove."
  (let* ((backend (make-magpi-test-backend))
         (magpi-backend backend)
         (magpi--actions (make-hash-table :test #'equal))
         (magpi--handles (make-hash-table :test #'equal))
         (magpi--intentions (make-hash-table :test #'equal))
         (intention (make-magpi-intention
                     :id "intent-1" :objective "Already merged" :state 'merged))
         (action (make-magpi-action :id "writer-1" :intention-id "intent-1"
                                    :observation (make-magpi-observation
                                                  :activity-state 'idle)))
         prompt removed discarded)
    (puthash "intent-1" intention magpi--intentions)
    (puthash "writer-1" action magpi--actions)
    (puthash "writer-1" 'writer-handle magpi--handles)
    (cl-letf (((symbol-function 'magpi-intention-git-facts)
               (lambda (_intention) '(:checkout dirty)))
              ((symbol-function 'yes-or-no-p)
               (lambda (p) (setq prompt p) t))
              ((symbol-function 'magpi-intention-remove-worktree)
               (lambda (record) (setq removed t) record))
              ((symbol-function 'magpi-intention-discard-record)
               (lambda (record) (setq discarded t) record))
              ((symbol-function 'magpi--paint-glance) #'ignore)
              ((symbol-function 'magpi--intention)
               (lambda (id) (gethash id magpi--intentions)))
              ((symbol-function 'magpi-discard--git-scope-p) (lambda () nil)))
      (let ((magpi-intention-metadata '(:intention-id "intent-1")))
        (magpi-test-at-section ()
          (should (eq (magpi-discard-scope) 'worktree))
          (magpi-discard))))
    (should removed)
    (should-not discarded)
    (should (equal (magpi-test-backend-terminated backend) '(writer-handle)))
    (should-not (gethash "writer-1" magpi--handles))
    (should-not (magpi-action-observation (gethash "writer-1" magpi--actions)))
    (should (eq (magpi-intention-state (gethash "intent-1" magpi--intentions))
                'merged))
    (should (string-match-p "leftover" prompt))
    (should (string-match-p "1 action" prompt))))

(ert-deftest magpi-discard-magit-hunk-delegates ()
  "Promise: Magit hunk/file/list stay Magit discard."
  (let (called)
    (cl-letf (((symbol-function 'derived-mode-p)
               (lambda (&rest _) nil))
              ((symbol-function 'magpi-discard--git-scope-p) (lambda () t))
              ((symbol-function 'magit-discard)
               (lambda () (setq called t)))
              ((symbol-function 'call-interactively)
               (lambda (fn) (funcall fn))))
      (magpi-discard)
      (should called))))

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
          (magpi-test-at-section (:action "cold-1") (magpi-visit))
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
    (magpi-test-at-section (:action "live-1") (magpi-visit))
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
          (magpi-test-at-section (:action "dead-1") (magpi-visit))
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

(ert-deftest magpi-dead-retry-after-disconnect-is-not-blood ()
  "Promise: successful retry produces reconnected; running activity is aloft."
  (magpi-test-without-store
  (let* ((backend (make-magpi-test-backend))
         (magpi-backend backend)
         (magpi--actions (make-hash-table :test #'equal))
         (magpi--handles (make-hash-table :test #'equal))
         (magpi--listener-epochs (make-hash-table :test #'equal))
         (magpi--refresh-timer nil)
         (launch (magpi-launch-build default-directory nil 'writer '(:kind none)))
         action)
    (unwind-protect
        (progn
          (setq action (magpi-spawn-spec "retry-blood" "Inspect this" launch))
          (magpi--handle-event "retry-blood" '(:type disconnected))
          (should (eq (magpi-observation-auspice
                       (magpi-action-observation
                        (gethash "retry-blood" magpi--actions)))
                      'blood))
          (puthash "retry-blood" 'dead magpi--handles)
          (setq action (magpi-spawn-spec "retry-blood" "Inspect this" launch))
          (let ((observation (magpi-action-observation action)))
            (should (eq (magpi-observation-connection-state observation)
                        'connected))
            (should (eq (magpi-observation-activity-state observation)
                        'running))
            (should (equal (magpi-observation-activity observation) "thinking"))
            (should-not (magpi-observation-problem observation))
            (should (eq (magpi-observation-auspice observation) 'aloft))))
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

(ert-deftest magpi-resume-root-grouped-requires-live-checkout ()
  "Promise: missing intention work is not source-root or the current repo."
  (let* ((magpi--intentions (make-hash-table :test #'equal))
         (magpi--actions (make-hash-table :test #'equal))
         (intention (make-magpi-intention
                     :id "intent-1" :objective "Why" :state 'active
                     :source-root "/tmp/project/"))
         (grouped (make-magpi-action
                   :id "grouped-1"
                   :intention-id "intent-1"
                   :source-root "/tmp/project/")))
    (puthash "intent-1" intention magpi--intentions)
    (puthash "grouped-1" grouped magpi--actions)
    (cl-letf (((symbol-function 'magpi-intention-worktree-path)
               (lambda (_intention) nil))
              ((symbol-function 'magpi--root)
               (lambda () "/tmp/elsewhere/"))
              ((symbol-function 'magpi-intention-ensure-worktree)
               (lambda (&rest _) (error "must not manufacture a checkout")))
              ((symbol-function 'magpi-action-save)
               (lambda (&rest _) (error "must not persist another locator"))))
      (should-error (magpi--resume-root grouped) :type 'user-error)
      (should-error (magpi--action-with-launch grouped) :type 'user-error)
      (should-not (magpi-action-launch (gethash "grouped-1" magpi--actions))))))

(ert-deftest magpi-ensure-process-grouped-retained-launch-requires-checkout ()
  "Promise: retained Launch does not attach without the live grouped checkout."
  (let* ((backend (make-magpi-test-backend))
         (magpi-backend backend)
         (magpi--actions (make-hash-table :test #'equal))
         (magpi--handles (make-hash-table :test #'equal))
         (magpi--intentions (make-hash-table :test #'equal))
         (magpi--refresh-timer nil)
         (intention (make-magpi-intention
                     :id "intent-1" :objective "Why" :state 'active
                     :source-root "/tmp/project/"))
         (launch (magpi-launch-build "/tmp/work/" nil 'writer '(:kind none)))
         (grouped (make-magpi-action
                   :id "grouped-1"
                   :intention-id "intent-1"
                   :source-root "/tmp/project/"
                   :launch launch)))
    (puthash "intent-1" intention magpi--intentions)
    (puthash "grouped-1" grouped magpi--actions)
    (unwind-protect
        (cl-letf (((symbol-function 'magpi-intention-worktree-path)
                   (lambda (_intention) nil))
                  ((symbol-function 'magpi-intention-ensure-worktree)
                   (lambda (&rest _) (error "must not manufacture a checkout")))
                  ((symbol-function 'magpi-action-save)
                   (lambda (&rest _) (error "must not persist another locator"))))
          (should-error (magpi--resume-root grouped) :type 'user-error)
          (should (eq grouped (magpi--action-with-launch grouped)))
          (should-error (magpi--ensure-process "grouped-1") :type 'user-error)
          (should-not (magpi-test-backend-spawn-count backend))
          (puthash "grouped-1" 'dead magpi--handles)
          (should-error (magpi--ensure-process "grouped-1" t) :type 'user-error)
          (should-not (magpi-test-backend-spawn-count backend))
          (should (eq (gethash "grouped-1" magpi--handles) 'dead)))
      (when (timerp magpi--refresh-timer)
        (cancel-timer magpi--refresh-timer)))))
(ert-deftest magpi-resume-root-grouped-uses-live-checkout ()
  "Promise: grouped reopen uses the live intention checkout."
  (let* ((magpi--intentions (make-hash-table :test #'equal))
         (intention (make-magpi-intention
                     :id "intent-1" :objective "Why" :state 'active
                     :source-root "/tmp/project/"))
         (grouped (make-magpi-action
                   :id "grouped-1"
                   :intention-id "intent-1"
                   :source-root "/tmp/project/")))
    (puthash "intent-1" intention magpi--intentions)
    (cl-letf (((symbol-function 'magpi-intention-worktree-path)
               (lambda (_intention) "/tmp/work/"))
              ((symbol-function 'magpi--root)
               (lambda () "/tmp/elsewhere/")))
      (should (equal (magpi--resume-root grouped) "/tmp/work/")))))

(ert-deftest magpi-resume-root-standalone-keeps-source-root ()
  "Promise: standalone reopen keeps source-root, not the current repository."
  (let ((alone (make-magpi-action :id "alone" :source-root "/tmp/project/")))
    (cl-letf (((symbol-function 'magpi--root)
               (lambda () "/tmp/elsewhere/")))
      (should (equal (magpi--resume-root alone) "/tmp/project/")))))
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

(ert-deftest magpi-refused-writer-is-not-launchable ()
  "Promise: a held lease refuses writers before birth; RET cannot attach them."
  (magpi-test-with-repo (repository "magpi-writer-admit-")
    (let* ((backend (make-magpi-test-backend))
           (magpi-backend backend)
           (magpi--actions (make-hash-table :test #'equal))
           (magpi--handles (make-hash-table :test #'equal))
           (magpi--intentions (make-hash-table :test #'equal))
           (magpi--refresh-timer nil)
           (magpi-launch--catalog (make-hash-table :test #'equal))
           (magpi-launch--last-model (make-hash-table :test #'equal))
           (default-directory repository)
           (ids '("holder" "refused")))
      (unwind-protect
          (progn
            (magpi-intention-create-record "Lease" repository "lease")
            (cl-letf (((symbol-function 'magpi--root) (lambda () repository))
                      ((symbol-function 'magpi--capture-bind)
                       (lambda (_kind) '(:kind none)))
                      ((symbol-function 'magpi-store-new-id)
                       (lambda () (pop ids))))
              (magpi-spawn-from-options
               '(:intention-id "lease" :role writer :lease t :context-kind none))
              (should (equal (plist-get (magpi-intention-writer-lease
                                         (magpi--intention "lease"))
                                        :action-id)
                             "holder"))
              (should-error
               (magpi-spawn-from-options
                '(:intention-id "lease" :role writer :context-kind none)))
              (should-not (magpi--lookup-action "refused"))
              (should-not (magpi-action-load repository "refused"))
              (let ((denied (car (last (magpi-intention-audit
                                        (magpi--intention "lease"))))))
                (should (eq (plist-get denied :type) 'writer-denied))
                (should (equal (plist-get denied :action-id) "refused")))
              (puthash "refused"
                       (make-magpi-action
                        :id "refused"
                        :intention-id "lease"
                        :chat-ref "refused"
                        :source-root repository
                        :launch (magpi-launch-build
                                 repository nil 'writer '(:kind none)))
                       magpi--actions)
              (magpi-test-at-section (:action "refused")
                (should-error (magpi-visit)))
              (should-not (gethash "refused" magpi--handles))
              (should (equal (magpi-test-backend-spawn-count backend) 1))))
        (when (timerp magpi--refresh-timer)
          (cancel-timer magpi--refresh-timer))))))

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
      ;; Heading paint is status-owned; opt in explicitly (no eager magpi require).
      (magpi-tests-require-status)
      (should (eq (magpi-status-view-slot (magpi-status-action-view joined) 'motion) 'cold)))))

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


(ert-deftest magpi-intention-create-always-mints-a-new-being ()
  "Promise: create always creates; matching objective is not identity."
  (magpi-test-with-repo (repository "magpi-intention-create-new-")
    (let ((magpi--intentions (make-hash-table :test #'equal)))
      (cl-letf (((symbol-function 'magpi--root) (lambda () repository)))
        (let ((first (magpi-intention-create "Same why"))
              (second (magpi-intention-create "Same why")))
          (should-not (equal (magpi-intention-id first)
                             (magpi-intention-id second)))
          (should (equal (magpi-intention-objective first) "Same why"))
          (should (equal (magpi-intention-objective second) "Same why"))
          (should (magpi-intention-load repository (magpi-intention-id first)))
          (should (magpi-intention-load repository (magpi-intention-id second))))))))

(ert-deftest magpi-chat-candidate-label-matches-action-id ()
  (let* ((action (make-magpi-action :id "hist-1" :chat-ref "hist-1"))
         (candidates '((:reference "pimacs:other" :label "Nope")
                       (:reference "pimacs:hist-1" :label "Historical why"))))
    (should (equal (magpi--chat-candidate-label-for-action action candidates)
                   "Historical why"))
    (should-not (magpi--chat-candidate-label-for-action
                 (make-magpi-action :id "missing")
                 candidates))))

(ert-deftest magpi-historical-action-titles-reconstruct-from-sessions ()
  "Promise: fresh Emacs rebuilds durable action titles from Pi sessions."
  (magpi-tests-require-status)
  (let* ((root (expand-file-name "/home/putra/.doom.d/magpi"))
         (store (expand-file-name "~/.pi/agent/sessions"))
         (actions-dir (expand-file-name ".git/magpi/actions" root)))
    (skip-unless (file-directory-p actions-dir))
    (skip-unless (file-directory-p store))
    (let* ((process-environment
            (cons (format "PI_CODING_AGENT_SESSION_DIR=%s" store)
                  process-environment))
           (pimacs-flags nil)
           (candidates (magpi-backend-chat-candidates
                        (make-magpi-pimacs-backend) root))
           (actions (magpi-action-list root))
           (titled 0))
      (should (consp candidates))
      (dolist (action actions)
        (when (magpi-action-p action)
          (let* ((label (magpi--chat-candidate-label-for-action action candidates))
                 (magpi-status-action-title-function (lambda (a) label))
                 (heading (magpi-status-view-slot (magpi-status-action-view action) 'title)))
            (should (eq (magpi-status-view-slot (magpi-status-action-view action) 'motion) 'cold))
            (should-not (magpi-action-observation action))
            (when label
              (setq titled (1+ titled))
              (should (or (equal heading label)
                          (and (string-suffix-p "…" heading)
                               (string-prefix-p (substring heading 0 -1) label))))
              ;; Live titles may start with those words; reject placeholders only.
              (should-not (magpi-pimacs--generated-session-name-p heading))))))
      ;; Eleven of twelve durable actions currently have matching sessions.
      (should (>= titled 11)))))
(provide 'magpi-tests)
;;; magpi-tests.el ends here
