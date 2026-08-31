;;; magpi-status-tests.el --- Tests for Magpi's Magit adapter -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)

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

(defmacro magpi-test-with-section-values (values &rest body)
  `(let ((values ,values))
     (cl-letf (((symbol-function 'magit-section-value-if)
                (lambda (type) (cdr (assq type values)))))
       ,@body)))

(ert-deftest magpi-status-ask-target-is-typed-not-parsed ()
  (magpi-test-with-section-values
   '((magpi-ask . ("action-1" . "approval-2")))
   (should (equal (magpi-status-target-at-point)
                  '(:kind ask :action-id "action-1" :ask-id "approval-2")))))

(ert-deftest magpi-status-target-is-derived-from-section-identity ()
  (magpi-test-with-section-values
   '((magpi-observed-file . ("action-1" . "lib/auth.ex")))
   (should (equal (magpi-status-target-at-point)
                  '(:kind observed-file :action-id "action-1" :path "lib/auth.ex")))))

(ert-deftest magpi-status-action-target-is-not-parsed-from-heading-text ()
  (magpi-test-with-section-values
   '((magpi-action . "action-1"))
   (should (equal (magpi-status-target-at-point)
                  '(:kind action :action-id "action-1")))))

(ert-deftest magpi-status-intention-target-names-membership-not-a-copy ()
  (magpi-test-with-section-values
   '((magpi-intention . "intent-1"))
   (should (equal (magpi-status-target-at-point)
                  '(:kind intention :intention-id "intent-1")))))

(ert-deftest magpi-status-root-target-is-typed-project-root ()
  (magpi-test-with-section-values
   '((magpi-status . "/tmp/project/"))
   (should (equal (magpi-status-target-at-point)
                  '(:kind root :root "/tmp/project/")))))

(ert-deftest magpi-status-tab-on-root-previews-children ()
  (let (previewed)
    (cl-letf (((symbol-function 'magit-current-section) (lambda () 'root))
              ((symbol-function 'magit-section-value-if)
               (lambda (type) (when (eq type 'magpi-status) "/tmp/project/")))
              ((symbol-function 'magit-section-show-headings)
               (lambda (_section) (setq previewed t))))
      (magpi-status-toggle-section)
      (should previewed))))

(ert-deftest magpi-status-heading-prefers-observed-title-over-first-message ()
  (let* ((launch (magpi-launch-build "/tmp/" 'medium 'writer '(:kind none)))
         (action (make-magpi-action
                   :id "action-2" :prompt "First message" :launch launch
                   :observation (make-magpi-observation :display-title "Observed title"))))
    (should (equal (substring-no-properties (magpi-status--heading action))
                   "Observed title"))
    (setf (magpi-action-observation action) (magpi-observation-initial))
    (should (equal (substring-no-properties (magpi-status--heading action))
                   "First message"))))

(ert-deftest magpi-status-heading-falls-back-to-frozen-context ()
  (let* ((launch (magpi-launch-build "/tmp/" 'medium 'writer
                                    '(:kind point :file "lib/auth.ex" :line 12)))
         (action (make-magpi-action :id "action-1" :launch launch
                                      :observation (magpi-observation-initial))))
    (should (equal (substring-no-properties (magpi-status--heading action))
                   "lib/auth.ex:12"))))

(ert-deftest magpi-status-insert-action-uses-title-in-heading-not-id ()
  (let* ((launch (magpi-launch-build "/tmp/" 'medium 'writer '(:kind none)))
         (action (make-magpi-action
                   :id "action-1"
                   :prompt "Authored task"
                   :launch launch
                   :observation (make-magpi-observation
                                 :activity-state 'idle
                                 :connection-state 'connected
                                 :display-title "Model summary"))))
    (with-temp-buffer
      (magpi-status--insert-action action)
      (let* ((text (substring-no-properties (buffer-string)))
             (heading (car (split-string text "\n"))))
        (should (string-prefix-p "Model summary" heading))
        (should-not (string-match-p "action-1" heading))
        (should-not (string-match-p "Authored task" heading))
        (should-not (string-match-p "title      " text))))))
(ert-deftest magpi-status-renders-observation-activity-state ()
  (should (equal (magpi-status--activity-state-label 'starting) "starting"))
  (should (equal (magpi-status--activity-state-label 'running) "running"))
  (should (equal (magpi-status--activity-state-label 'idle) "idle"))
  (should (eq (magpi-status--activity-state-face 'running)
              'magpi-status-live))
  (should (eq (magpi-status--activity-state-face 'starting)
              'magpi-status-pending))
  (should (eq (magpi-status--activity-state-face 'idle)
              'magpi-status-quiet)))

(ert-deftest magpi-status-inherits-basic-navigation-not-magit-commands ()
  (should (eq (keymap-parent magpi-status-mode-map) special-mode-map))
  (should-not (lookup-key magpi-status-mode-map (kbd "b")))
  (should (eq (lookup-key magpi-status-mode-map (kbd "C-i"))
              #'magpi-status-toggle-section)))

(ert-deftest magpi-status-wraps-long-lines-by-default ()
  (with-temp-buffer
    (magpi-status-mode)
    (should-not truncate-lines)
    (should-not truncate-partial-width-windows)
    (should word-wrap)))
(ert-deftest magpi-status-heading-suffix-is-state-thinking-running-model ()
  (let* ((launch (magpi-launch-build "/tmp/" 'medium 'writer '(:kind none)))
         (action (make-magpi-action
                   :id "action-1"
                   :prompt "Refactor tokens"
                   :launch launch
                   :observation (make-magpi-observation
                                 :activity-state 'running
                                 :connection-state 'connected
                                 :running-model "anthropic/claude-sonnet"))))
    (should (equal (magpi-status--heading-suffix action)
                   "running · medium · anthropic/claude-sonnet"))
    (setf (magpi-observation-running-model (magpi-action-observation action))
          nil)
    (should (equal (magpi-status--heading-suffix action)
                   "running · medium"))))

(ert-deftest magpi-status-face-sets-font-lock-face-for-magit ()
  (let ((text (magpi-status--face "running" 'magpi-status-live)))
    (should (eq (get-text-property 0 'face text)
                'magpi-status-live))
    (should (eq (get-text-property 0 'font-lock-face text)
                'magpi-status-live))))

(ert-deftest magpi-status-insert-action-body-holds-what-heading-cannot ()
  (let* ((launch (magpi-launch-build "/tmp/" 'high 'writer '(:kind none)
                                     "openai/gpt-4.1"))
         (with-model (make-magpi-action
                      :id "action-1"
                      :prompt "Show model"
                      :launch launch
                      :observation (make-magpi-observation
                                    :activity-state 'idle
                                    :connection-state 'connected
                                    :running-model "openai/gpt-4.1"
                                    :activity "edit"
                                    :observed-files '("lib/auth.ex")
                                    :problem "needs review")))
         (without-model (make-magpi-action
                         :id "action-2"
                         :prompt "Pending model"
                         :launch (magpi-launch-build "/tmp/" 'high 'reader
                                                     '(:kind none))
                         :observation (magpi-observation-initial))))
    (with-temp-buffer
      (magpi-status--insert-action with-model)
      (let ((text (substring-no-properties (buffer-string))))
        (should (string-match-p "idle · high · openai/gpt-4.1" text))
        (should (string-match-p "role +w" text))
        (should (string-match-p "activity   edit" text))
        (should (string-match-p "problem    needs review" text))
        (should (string-match-p "lib/auth.ex" text))
        (should-not (string-match-p "effort" text))
        (should-not (string-match-p "usage" text))
        (should-not (string-match-p "intent     " text))
        (should-not (string-match-p "model      " text)))
      (goto-char (point-min))
      (should (text-property-any (point-min) (point-max)
                                 'face 'magpi-status-identity))
      (should (text-property-any (point-min) (point-max)
                                 'font-lock-face 'magpi-status-identity))
      (should (text-property-any (point-min) (point-max)
                                 'face 'magpi-status-alert))
      (should (text-property-any (point-min) (point-max)
                                 'face 'magpi-status-evidence)))
    (with-temp-buffer
      (magpi-status--insert-action without-model)
      (let ((text (substring-no-properties (buffer-string))))
        (should (string-match-p "starting · high" text))
        (should (string-match-p "role +r" text))
        (should-not (string-match-p "openai/gpt-4.1" text))
        (should-not (string-match-p "model      " text))))))

(ert-deftest magpi-status-renders-nested-asks-with-clear-text-and-paths ()
  (let* ((launch (magpi-launch-build "/tmp/" 'medium 'writer '(:kind none)))
         (action (make-magpi-action
                   :id "action-1" :prompt "Review schema" :launch launch
                   :observation
                   (make-magpi-observation
                    :asks
                    (list (make-magpi-ask
                           :id "parent" :requester "planner"
                           :question "Apply the schema migration?" :state 'pending
                           :affected-paths '("lib/schema.ex" "priv/migrate.ex"))
                          (make-magpi-ask
                           :id "child" :parent-id "parent" :requester "reviewer"
                           :question "Allow the validation subagent?" :state 'approved
                           :affected-paths '("test/schema_test.ex")))))))
    (with-temp-buffer
      (magpi-status--insert-action action)
      (let ((text (substring-no-properties (buffer-string))))
        (should (string-match-p "? ask  Apply the schema migration?" text))
        (should (string-match-p "✓ answered  Allow the validation subagent?" text))
        (should (string-match-p "affected paths (2)" text))
        (should (string-match-p "priv/migrate.ex" text))
        (should (string-match-p "test/schema_test.ex" text)))
      (should (text-property-any (point-min) (point-max)
                                 'face 'magpi-status-pending))
      (should (text-property-any (point-min) (point-max)
                                 'face 'magpi-status-live)))))

(ert-deftest magpi-status-event-paint-does-not-run-prepare-hook ()
  (let (order)
    (with-temp-buffer
      (setq magpi-status-root "/tmp/"
            magpi-status-prepare-function (lambda () (push 'prepare order))
            magpi-status-actions-function
            (lambda ()
              (push 'read order)
              nil))
      (magpi-status-refresh-buffer)
      (should (equal order '(read)))
      (should (text-property-any (point-min) (point-max)
                                 'face 'magpi-status-header))
      (should (text-property-any (point-min) (point-max)
                                 'face 'magpi-status-quiet)))))

(ert-deftest magpi-status-empty-surface-is-ordinary-loop ()
  (with-temp-buffer
    (setq magpi-status-root "/tmp/magpi-project/"
          magpi-status-actions-function (lambda () nil))
    (magpi-status-refresh-buffer)
    (let ((text (substring-no-properties (buffer-string))))
      (should (string-match-p "MAGPI · magpi-project" text))
      (should (string-match-p "i  create intention" text))
      (should (string-match-p "@  bind context" text))
      (should (string-match-p "s  spawn action" text))
      (should (string-match-p "a  react" text))
      (should-not (string-match-p "Planes" text))
      (should-not (string-match-p "intention surface" text))
      (should-not (string-match-p "Ready" text)))))

(ert-deftest magpi-status-manual-refresh-snapshots-then-paints ()
  (let (order)
    (cl-letf (((symbol-function 'magit-refresh-buffer)
               (lambda () (push 'paint order))))
      (setq magpi-status-prepare-function (lambda () (push 'prepare order)))
      (magpi-status-refresh)
      (should (equal (nreverse order) '(prepare paint))))))

(ert-deftest magpi-status-intention-header-is-concise-git-dashboard ()
  (let ((intention (make-magpi-intention
                    :id "intent-1" :objective "Ship auth repair"
                    :worktree-path "/tmp/work/" :branch "magpi/auth"
                    :base-ref "main" :state 'active
                    :writer-lease '(:action-id "t1"))))
    (cl-letf (((symbol-function 'magpi-intention-git-facts)
               (lambda (_intention) '(:checkout dirty :dirty t :ahead 3 :behind 1 :exists t))))
      (with-temp-buffer
        (magpi-status--insert-intention intention nil)
        (let ((text (substring-no-properties (buffer-string))))
          (should (string-match-p "Ship auth repair" text))
          (should (string-match-p "magpi/auth · dirty" text))
          (should (string-match-p "magpi/auth · dirty" text)))))))

(ert-deftest magpi-status-binds-ordinary-loop-and-changes ()
  (should (eq (lookup-key magpi-status-mode-map (kbd "l"))
              #'magpi-status-changes-log))
  (should (eq (lookup-key magpi-status-mode-map (kbd "c"))
              #'magpi-status-changes-commit))
  (should (eq (lookup-key magpi-status-mode-map (kbd "s"))
              #'magpi-status-spawn))
  (should (eq (lookup-key magpi-status-mode-map (kbd "i"))
              #'magpi-status-create))
  (should (eq (lookup-key magpi-status-mode-map (kbd "@"))
              #'magpi-status-bind))
  (should (eq (lookup-key magpi-status-mode-map (kbd "a"))
              #'magpi-status-react))
  (should (eq (lookup-key magpi-status-mode-map (kbd "m"))
              #'magpi-status-changes-open))
  ;; Other familiar Magit keys are not given unrelated Magpi meanings.
  (dolist (key '("r" "w" "k"))
    (should-not (lookup-key magpi-status-mode-map (kbd key))))
  (should (eq (lookup-key magpi-status-mode-map (kbd "d"))
              #'magpi-status-changes-diff)))

(ert-deftest magpi-status-create-delivers-intention-text ()
  "Promise: create yields the authored string to the owner, or user-errors."
  (let* ((collected nil)
         (magpi-status-create-function
          (lambda (intent)
            (interactive (list (read-string "Intention: ")))
            (setq collected intent))))
    (cl-letf (((symbol-function 'call-interactively)
               (lambda (fn &rest _) (funcall fn "Ship auth"))))
      (magpi-status-create)
      (should (equal collected "Ship auth"))))
  (let ((magpi-status-create-function nil))
    (should-error (magpi-status-create) :type 'user-error)))
(ert-deftest magpi-status-renders-persisted-file-and-chat-tags ()
  (let ((intention (make-magpi-intention
                    :id "intent-1" :objective "Keep context" :state 'active
                    :bindings '((:kind file :reference "notes.org" :label "notes"
                                    :tags ("design" "scope"))
                                   (:kind chat :reference "pimacs:session-1"
                                    :label "Earlier chat" :tags ("handoff"))))))
    (with-temp-buffer
      (magpi-status--insert-intention intention nil)
      (let ((text (substring-no-properties (buffer-string))))
        (should (string-match-p "@file notes  #design #scope" text))
        (should (string-match-p "@chat Earlier chat  #handoff" text))))))
(provide 'magpi-status-tests)
;;; magpi-status-tests.el ends here
