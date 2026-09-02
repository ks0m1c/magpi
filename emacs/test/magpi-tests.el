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
         (magpi--refresh-timer nil)
         (action (make-magpi-action :id "cold-1" :chat-ref "cold-1")))
    (unwind-protect
        (progn
          (puthash "cold-1" action magpi--actions)
          (magpi--handle-event "cold-1" '(:type history-pending))
          (should (eq (gethash "cold-1" magpi--actions) action))
          (should-not (magpi-action-observation action))
          (should (timerp magpi--refresh-timer)))
      (when (timerp magpi--refresh-timer)
        (cancel-timer magpi--refresh-timer)))))
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
`magpi-status--heading*' must opt in explicitly."
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
  (require 'magpi-status))

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
                         '(merge discard))))
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
                       :worktree-path "/tmp/work/" :source-root "/tmp/project/"
                       :base-ref "main" :branch "magpi/why"))
           (alone (make-magpi-action :id "alone" :source-root "/tmp/project/"
                                     :spawn-oid "abc"))
           magit-root diff-range)
      (puthash "intent-1" intention magpi--intentions)
      (puthash "alone" alone magpi--actions)
      (cl-letf (((symbol-function 'magpi--intention)
                 (lambda (id) (gethash id magpi--intentions)))
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
    (magpi-test-at-section (:intention "intent-1")
      (should (equal (mapcar #'cdr (magpi-react--choices))
                     '(release))))
    (setf (magpi-intention-writer-lease intention) nil)
    (magpi-test-at-section (:intention "intent-1")
      (should (equal (mapcar #'cdr (magpi-react--choices))
                     '(merge discard))))))

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
       (magpi-test-at-section (:intention "intent-1")
         (magpi--react-merge)))
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
      (magpi-test-at-section (:intention "intent-1")
        (magpi--react-discard))
      (should discarded)
      (should (string-match-p "force-removes" prompt))
      (should (string-match-p "dirty" prompt)))))

(ert-deftest magpi-discard-scope-is-contextual ()
  "Promise: discard depth is innermost, like Magit hunk vs file vs list."
  (magpi-test-at-section (:action "cold-1")
    (should (eq (magpi-discard-scope) 'chat)))
  (magpi-test-at-section (:action "a1" :intention "intent-1")
    (should (eq (magpi-discard-scope) 'action)))
  (magpi-test-at-section (:intention "intent-1")
    (should (eq (magpi-discard-scope) 'intention)))
  (magpi-test-at-section (:action "a1" :ask "q1" :intention "intent-1")
    (should (eq (magpi-discard-scope) 'ask)))
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
                     :writer-lease '(:action-id "cold-1")))
         (action (make-magpi-action
                  :id "cold-1" :intention-id nil
                  :observation (make-magpi-observation :activity-state 'running))))
    (puthash "intent-1" intention magpi--intentions)
    (puthash "cold-1" action magpi--actions)
    (puthash "cold-1" 'test-handle magpi--handles)
    (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_prompt) t))
              ((symbol-function 'magpi--schedule-refresh) #'ignore))
      (magpi-test-at-section (:action "cold-1")
        (magpi-discard)))
    (should (equal (magpi-test-backend-terminated backend) '(test-handle)))
    (should-not (gethash "cold-1" magpi--handles))
    (should-not (magpi-action-observation (gethash "cold-1" magpi--actions)))
    (should (equal (plist-get (magpi-intention-writer-lease
                               (gethash "intent-1" magpi--intentions))
                              :action-id)
                   "cold-1"))))

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
              ((symbol-function 'magpi--schedule-refresh) #'ignore)
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
              ((symbol-function 'magpi--schedule-refresh) #'ignore)
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
      ;; Heading paint is status-owned; opt in explicitly (no eager magpi require).
      (magpi-tests-require-status)
      (should (equal (magpi-status--heading-suffix joined)
                     (mapconcat #'identity
                                (delq nil
                                      (list (magpi-status--auspice-motion 'cold)
                                            (magpi-status--age-label
                                             (magpi-action-created-at joined))))
                                " · "))))))

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
                 (heading (substring-no-properties (magpi-status--heading action)))
                 (suffix (magpi-status--heading-suffix action)))
            (should (string-prefix-p (magpi-status--auspice-motion 'cold) suffix))
            (should-not (magpi-action-observation action))
            (when label
              (setq titled (1+ titled))
              (should (or (equal heading label)
                          (and (string-suffix-p "…" heading)
                               (string-prefix-p (substring heading 0 -1) label))))
              (should-not (string-match-p "\\`New chat" heading))
              (should-not (string-match-p "\\`New task" heading))))))
      ;; Eleven of twelve durable actions currently have matching sessions.
      (should (>= titled 11)))))
(provide 'magpi-tests)
;;; magpi-tests.el ends here
