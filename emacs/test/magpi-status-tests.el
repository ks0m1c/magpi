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

(ert-deftest magpi-status-tab-on-root-previews-children ()
  (let (previewed)
    (cl-letf (((symbol-function 'magit-current-section) (lambda () 'root))
              ((symbol-function 'magpi-status--otype)
               (lambda (_section) 'magpi-status))
              ((symbol-function 'magit-section-show-headings)
               (lambda (_section) (setq previewed t))))
      (magpi-status-toggle-section)
      (should previewed))))

(ert-deftest magpi-facet-is-type-and-value-not-a-plist ()
  (cl-letf (((symbol-function 'magpi-status--otype)
             (lambda (section) (car section)))
            ((symbol-function 'magpi-status--ovalue)
             (lambda (section) (cadr section))))
    (should (equal (magpi-facet '(magpi-action "a1"))
                   '(magpi-action . "a1")))
    (should (eq (magpi-facet-type '(magpi-ask ("a1" . "q1"))) 'magpi-ask))
    (should (equal (magpi-facet-value '(magpi-observed-file ("a1" . "lib/auth.ex")))
                   '("a1" . "lib/auth.ex")))
    (should-not (keywordp (car-safe (magpi-facet '(magpi-intention "i1")))))
    (should (eq (magpi-section-bind-surface '(magpi-ask ("a1" . "q1"))) 'action))
    (should (eq (magpi-section-bind-surface '(magpi-bindings "i1")) 'intention))
    (should (eq (magpi-section-bind-surface '(magpi-perch nil)) 'root))
    (should (eq (magpi-section-bind-surface nil) 'root))))

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
        (should (string-prefix-p "Model summary" (string-trim-left heading)))
        (should-not (string-match-p "action-1" heading))
        (should-not (string-match-p "Authored task" heading))
        (should-not (string-match-p "title      " text))))))
(ert-deftest magpi-status-auspice-faces ()
  (should (eq (magpi-status--auspice-face 'lift) 'magpi-status-pending))
  (should (eq (magpi-status--auspice-face 'aloft) 'magpi-status-live))
  (should (eq (magpi-status--auspice-face 'rest) 'magpi-status-quiet))
  (should (eq (magpi-status--auspice-face 'cold) 'magpi-status-quiet))
  (should (eq (magpi-status--auspice-face 'empty) 'magpi-status-quiet))
  (should (eq (magpi-status--auspice-face 'blood) 'magpi-status-alert)))

(ert-deftest magpi-status-still-cast-is-five-columns ()
  (should (equal (magpi-status--auspice-motion 'cold) "·····"))
  (should (equal (magpi-status--auspice-motion 'lift) "◈····"))
  (should (equal (magpi-status--auspice-motion 'aloft) "·ˋ◈ˊ·"))
  (should (equal (magpi-status--auspice-motion 'rest) "ˏˋ◇ˎˊ"))
  (should (equal (magpi-status--auspice-motion 'blood) "  *  "))
  (should (equal (magpi-status--auspice-motion 'quiescent)
                 (magpi-status--auspice-motion 'cold)))
  (dolist (hour '(cold lift aloft rest blood empty quiescent))
    (should (= (string-width (magpi-status--auspice-motion hour))
               magpi-status-motion-width))))

(ert-deftest magpi-status-cast-fallbacks-preserve-seat-widths ()
  (cl-letf (((symbol-function 'char-displayable-p) (lambda (_) nil)))
    (dolist (hour '(cold lift aloft rest blood empty quiescent))
      (should (= (string-width (magpi-status--auspice-motion hour))
                 magpi-status-motion-width)))
    (dolist (state '(empty bound hears))
      (should (= (string-width (magpi-status--retention-mark state))
                 magpi-status-retention-width)))
    (should (equal (substring-no-properties (magpi-status--group-heading 'perch))
                   "Perch"))
    (should (equal (substring-no-properties (magpi-status--group-heading 'cold))
                   "Cold"))))

(ert-deftest magpi-status-group-marks-are-roost-and-bench ()
  (should (equal (alist-get 'perch magpi-status-group-cast) "⊤"))
  (should (equal (alist-get 'cold magpi-status-group-cast) "⊓"))
  (dolist (group '(perch cold))
    (let ((mark (alist-get group magpi-status-group-cast)))
      (should (= (string-width mark) 1))))
  (cl-letf (((symbol-function 'char-displayable-p) (lambda (_) t)))
    (should (equal (substring-no-properties (magpi-status--group-heading 'perch))
                   "⊤ Perch"))
    (should (equal (substring-no-properties (magpi-status--group-heading 'cold))
                   "⊓ Cold"))))

(ert-deftest magpi-status-heading-seats-do-not-wander-with-title-length ()
  (let* ((magpi-status-layout-width 60)
         (observation (make-magpi-observation
                       :activity-state 'running :connection-state 'connected
                       :observed-files '("lib/a.ex")))
         (short (make-magpi-action :id "s" :prompt "A" :observation observation))
         (long (make-magpi-action
                :id "l"
                :prompt "A deliberately long title that must yield to fixed seats"
                :observation observation))
         headings)
    (dolist (action (list short long))
      (with-temp-buffer
        (let ((magpi-status-layout-width magpi-status-layout-width))
          (magpi-status--insert-action action)
          (push (car (split-string (substring-no-properties (buffer-string)) "\n"))
                headings))))
    (pcase-let* ((`(,long-heading ,short-heading) headings)
                 (motion (regexp-quote (magpi-status--auspice-motion 'aloft)))
                 (retention (regexp-quote (magpi-status--retention-mark 'hears)))
                 (short-motion (string-match motion short-heading))
                 (long-motion (string-match motion long-heading)))
      (should (= short-motion long-motion))
      (should (= (string-match retention short-heading
                               (+ short-motion magpi-status-motion-width))
                 (string-match retention long-heading
                               (+ long-motion magpi-status-motion-width)))))))

(ert-deftest magpi-status-fault-attention-outranks-pending-ask ()
  (let ((action (make-magpi-action
                 :id "a"
                 :observation (make-magpi-observation
                               :activity-state 'running
                               :connection-state 'disconnected
                               :asks (list (make-magpi-ask
                                            :id "q" :state 'pending))))))
    (should (equal (car (magpi-status--action-attention action)) "!"))))
(ert-deftest magpi-status-faces-inherit-without-background ()
  (should (eq (face-attribute 'magpi-status-identity :inherit nil)
              'font-lock-function-name-face))
  (should (eq (face-attribute 'magpi-status-title :inherit nil)
              'font-lock-string-face))
  (should (eq (face-attribute 'magpi-status-pending :inherit nil) 'warning))
  (should (eq (face-attribute 'magpi-status-live :inherit nil) 'success))
  (should (eq (face-attribute 'magpi-status-quiet :inherit nil) 'shadow))
  (should (eq (face-attribute 'magpi-status-alert :inherit nil) 'error))
  (dolist (face '(magpi-status-identity magpi-status-title magpi-status-pending
                   magpi-status-live magpi-status-quiet magpi-status-alert))
    (should (eq (face-attribute face :background nil) 'unspecified))))

(ert-deftest magpi-status-blood-outranks-aloft ()
  (let* ((launch (magpi-launch-build "/tmp/" 'medium 'writer '(:kind none)))
         (make (lambda (observation)
                 (make-magpi-action :id "a1" :prompt "Go" :launch launch
                                    :observation observation)))
         (aloft (funcall make
                         (make-magpi-observation
                          :activity-state 'running
                          :connection-state 'connected)))
         (problem (funcall make
                           (make-magpi-observation
                            :activity-state 'running
                            :connection-state 'connected
                            :problem "boom")))
         (disconnected (funcall make
                                (make-magpi-observation
                                 :activity-state 'running
                                 :connection-state 'disconnected))))
    (should (eq (cdr (car (magpi-status--heading-parts aloft)))
                'magpi-status-live))
    (should (eq (cdr (car (magpi-status--heading-parts problem)))
                'magpi-status-alert))
    (should (eq (cdr (car (magpi-status--heading-parts disconnected)))
                'magpi-status-alert))))

(ert-deftest magpi-status-inherits-basic-navigation-not-magit-commands ()
  (should (eq (keymap-parent magpi-status-mode-map) special-mode-map))
  (should-not (lookup-key magpi-status-mode-map (kbd "b")))
  (should (eq (lookup-key magpi-status-mode-map (kbd "C-i"))
              #'magpi-status-toggle-section))
  (should (eq (lookup-key magpi-status-mode-map (kbd "n"))
              #'magpi-status-next-chat))
  (should (eq (lookup-key magpi-status-mode-map (kbd "p"))
              #'magpi-status-previous-chat)))

(ert-deftest magpi-status-n-p-cycle-last-seen-chats ()
  (let* ((a '("a")) (b '("b")) (c '("c"))
         (display (list a b c))
         (point nil)
         landed)
    (cl-letf (((symbol-function 'magpi-status--action-sections)
               (lambda (&optional _) display))
              ((symbol-function 'magpi-status--ovalue) #'car)
              ((symbol-function 'magpi-section-action-id)
               (lambda (&optional _) (car-safe point)))
              ((symbol-function 'magpi-last-seen-action-ids)
               (lambda () '("c" "b")))
              ((symbol-function 'magpi-status--goto-section)
               (lambda (section)
                 (setq landed section point section))))
      (magpi-status-next-chat)
      (should (eq landed c))
      (magpi-status-next-chat)
      (should (eq landed b))
      (magpi-status-next-chat)
      (should (eq landed a))
      (magpi-status-next-chat)
      (should (eq landed c))
      (setq point nil)
      (magpi-status-previous-chat)
      (should (eq landed a))
      (cl-letf (((symbol-function 'magpi-status--action-sections)
                 (lambda (&optional _) nil)))
        (should-error (magpi-status-next-chat) :type 'user-error)))))
(ert-deftest magpi-status-wraps-long-lines-by-default ()
  (with-temp-buffer
    (magpi-status-mode)
    (should-not truncate-lines)
    (should-not truncate-partial-width-windows)
    (should word-wrap)))
(ert-deftest magpi-status-heading-suffix-is-motion-then-meta ()
  (let* ((launch (magpi-launch-build "/tmp/" 'medium 'writer '(:kind none)))
         (aloft (magpi-status--auspice-motion 'aloft))
         (action (make-magpi-action
                  :id "action-1"
                  :prompt "Refactor tokens"
                  :launch launch
                  :observation (make-magpi-observation
                                :activity-state 'running
                                :connection-state 'connected
                                :running-model "anthropic/claude-sonnet"))))
    (should (equal (mapcar #'car (magpi-status--heading-parts action))
                   (list aloft "anthropic/claude-sonnet" "medium" "w")))
    (should (equal (magpi-status--heading-suffix action)
                   (format "%s · anthropic/claude-sonnet · medium · w" aloft)))
    (setf (magpi-observation-running-model (magpi-action-observation action)) nil)
    (should (equal (magpi-status--heading-suffix action)
                   (format "%s · medium · w" aloft)))))

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
                      :prompt "Show run"
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
        (should (string-match-p "^! Show run" text))
        (should (string-match-p
                 (regexp-quote (magpi-status--auspice-motion 'blood)) text))
        (should (string-match-p
                 (regexp-quote (magpi-status--retention-mark 'hears)) text))
        (should (string-match-p "openai/gpt-4\\.1" text))
        (should (string-match-p "role +w" text))
        (should (string-match-p "activity   edit" text))
        (should (string-match-p "problem    needs review" text))
        (should (string-match-p "lib/auth.ex" text))
        (should-not (string-match-p "effort" text))
        (should-not (string-match-p "usage" text))
        (should-not (string-match-p "intent     " text))
        (should-not (string-match-p "model      " text))
        (should-not (string-match-p "starting" text)))
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
        (should (string-match-p
                 (regexp-quote (magpi-status--auspice-motion 'lift)) text))
        (should (string-match-p
                 (regexp-quote (magpi-status--retention-mark 'empty)) text))
        (should (string-match-p "high" text))
        (should (string-match-p "r" text))
        (should (string-match-p "role +r" text))
        (should-not (string-match-p "openai/gpt-4.1" text))
        (should-not (string-match-p "^    model +" text))))))

(ert-deftest magpi-status-insert-action-body-shows-chat-tokens ()
  (let* ((launch (magpi-launch-build "/tmp/" 'medium 'writer '(:kind none)))
         (action (make-magpi-action
                  :id "action-1"
                  :prompt "Show run"
                  :launch launch
                  :observation (make-magpi-observation
                                :activity-state 'idle
                                :connection-state 'connected
                                :usage '(:input 12400 :output 3100
                                         :cost 0.042 :context 9000
                                         :window 200000)))))
    (with-temp-buffer
      (magpi-status--insert-action action)
      (let ((text (substring-no-properties (buffer-string))))
        (should (string-match-p "tokens +12.4k in · 3.1k out · 9k/200k ctx · \\$0.0420"
                                text))))))
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
        (should (string-match-p "test/schema_test.ex" text))
        (should (string-match-p "requested by  planner" text))
        (should-not (string-match-p "a  react" text)))
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
  (should (eq (magpi-status--subject-glance nil nil) 'empty))
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
      (should-not (string-match-p "Ready" text))
      (should-not (string-match-p "starting" text))
      (should-not (string-match-p "quiescent" text))
      (should-not (string-match-p "\\<rest\\>" text)))))

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
        (setq-local magpi-status-layout-width 100)
        (magpi-status--insert-intention intention nil)
        (let ((text (substring-no-properties (buffer-string))))
          (should (string-match-p "Ship auth repair" text))
          (should (string-match-p
                   (regexp-quote (magpi-status--auspice-motion 'quiescent)) text))
          (should (string-match-p
                   (regexp-quote (magpi-status--retention-mark 'hears)) text))
          (should (string-match-p "magpi/auth" text))
          (should (string-match-p "dirty" text))
          (should (string-match-p "\\bw\\b" text))
          (should-not (string-match-p "\\?" text))
          (should-not (string-match-p "\\<rest\\>" text)))))))

(ert-deftest magpi-status-unstarted-intention-says-not-started ()
  (let ((intention (make-magpi-intention
                    :id "intent-1" :objective "Ship auth" :state 'active)))
    (cl-letf (((symbol-function 'magpi-intention-git-facts)
               (lambda (_intention) '(:checkout unstarted))))
      (should (equal (magpi-status--intention-suffix intention) "not started"))
      (should-not (eq (magpi-status--subject-glance intention nil) 'quiescent))
      (with-temp-buffer
        (magpi-status--insert-intention intention nil)
        (let ((text (substring-no-properties (buffer-string))))
          (should (string-prefix-p "Ship auth" (string-trim-left text)))
          (should (string-match-p "not started" text))
          (should-not (string-match-p "quiescent" text))
          (should-not (string-match-p "—" text)))))))

(ert-deftest magpi-status-intention-why-does-not-yield-to-observed-title ()
  (let* ((intention (make-magpi-intention
                     :id "intent-1" :objective "Ship auth" :state 'active
                     :worktree-path "/tmp/work/" :branch "magpi/auth"))
         (action (make-magpi-action
                  :id "a1" :intention-id "intent-1" :prompt "task"
                  :observation (make-magpi-observation
                                :display-title "Observed flight"
                                :activity-state 'running
                                :connection-state 'connected))))
    (cl-letf (((symbol-function 'magpi-intention-git-facts)
               (lambda (_intention) '(:checkout dirty :exists t))))
      (with-temp-buffer
        (magpi-status--insert-intention intention (list action))
        (let* ((text (substring-no-properties (buffer-string)))
               (heading (car (split-string text "\n"))))
          (should (string-prefix-p "Ship auth" (string-trim-left heading)))
          (should-not (string-match-p "Observed flight" heading))
          (should (string-match-p "Observed flight" text)))))))
(ert-deftest magpi-status-binds-ordinary-loop-and-changes ()
  (should (eq (lookup-key magpi-status-mode-map (kbd "l"))
              #'magpi-changes-log))
  (should (eq (lookup-key magpi-status-mode-map (kbd "c"))
              #'magpi-changes-commit))
  (should (eq (lookup-key magpi-status-mode-map (kbd "s"))
              #'magpi-spawn))
  (should (eq (lookup-key magpi-status-mode-map (kbd "i"))
              #'magpi-intention-create))
  (should (eq (lookup-key magpi-status-mode-map (kbd "@"))
              #'magpi-bind))
  (should (eq (lookup-key magpi-status-mode-map (kbd "a"))
              #'magpi-react))
  (should (eq (lookup-key magpi-status-mode-map (kbd "m"))
              #'magpi-changes-status))
  (should (eq (lookup-key magpi-status-mode-map (kbd "k"))
              #'magpi-discard))
  ;; Other familiar Magit keys are not given unrelated Magpi meanings.
  (dolist (key '("r" "w"))
    (should-not (lookup-key magpi-status-mode-map (kbd key))))
  (should (eq (lookup-key magpi-status-mode-map (kbd "d"))
              #'magpi-changes-diff))
  (should (eq (lookup-key magpi-status-mode-map (kbd "RET"))
              #'magpi-visit))
  (should (eq (lookup-key magpi-status-mode-map (kbd "j"))
              #'magpi-status-jump)))

(ert-deftest magpi-status-jump-toggles-cold-then-perch ()
  (let (landed)
    (cl-letf (((symbol-function 'magpi-status--current-group)
               (lambda (&optional _) (car landed)))
              ((symbol-function 'magpi-status--jump-to-group)
               (lambda (type _name)
                 (push type landed)
                 type)))
      (magpi-status-jump)
      (should (equal landed '(magpi-cold)))
      (magpi-status-jump)
      (should (equal landed '(magpi-perch magpi-cold)))
      (magpi-status-jump)
      (should (eq (car landed) 'magpi-cold)))))
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

(ert-deftest magpi-status-cold-kernel-is-not-lift ()
  (let ((action (make-magpi-action :id "disk-1" :prompt "Persisted"))
        (cold (magpi-status--auspice-motion 'cold))
        (lift (magpi-status--auspice-motion 'lift)))
    (should (equal (magpi-status--heading-suffix action) cold))
    (should-not (equal (magpi-status--heading-suffix action) lift))
    (should-not (string-match-p "starting" (magpi-status--heading-suffix action)))
    (with-temp-buffer
      (magpi-status--insert-action action)
      (let ((text (substring-no-properties (buffer-string))))
        (should (string-match-p (regexp-quote cold) text))
        (should-not (string-match-p "starting" text))
        (should-not (string-match-p (regexp-quote lift) text))))))

(ert-deftest magpi-status-quiescent-is-not-rest ()
  (let* ((launch (magpi-launch-build "/tmp/" 'medium 'reader '(:kind none)))
         (resting (make-magpi-action
                   :id "a1" :prompt "Done" :launch launch
                   :observation (make-magpi-observation
                                 :activity-state 'idle
                                 :connection-state 'connected)))
         (lifting (make-magpi-action
                   :id "a2" :prompt "Go" :launch launch
                   :observation (magpi-observation-initial)))
         (intention (make-magpi-intention
                     :id "intent-1" :objective "Ship auth repair"
                     :worktree-path "/tmp/work/" :branch "magpi/auth"
                     :state 'active)))
    (should (equal (magpi-status--heading-suffix resting)
                   (format "%s · medium · r" (magpi-status--auspice-motion 'rest))))
    (should-not (string-match-p "quiescent" (magpi-status--heading-suffix resting)))
    (cl-letf (((symbol-function 'magpi-intention-git-facts)
               (lambda (_intention) '(:checkout dirty :exists t))))
      (should (eq (magpi-status--subject-glance intention (list resting)) 'quiescent))
      (should-not (eq (magpi-status--subject-glance intention (list lifting))
                      'quiescent))
      (with-temp-buffer
        (magpi-status--insert-intention intention (list resting))
        (let ((text (substring-no-properties (buffer-string))))
          (should (string-match-p (regexp-quote (magpi-status--auspice-motion 'quiescent))
                                  text))
          (should (string-match-p (regexp-quote (magpi-status--auspice-motion 'rest))
                                  text))
          (should-not (string-match-p "starting" text)))))))

(ert-deftest magpi-status-auspice-rows-are-legible-without-motion ()
  "Promise: every action auspice reconstructs as a still motion seat."
  (let* ((launch (magpi-launch-build "/tmp/" 'medium 'writer '(:kind none)))
         (rows
          (list
           (cons 'cold (make-magpi-action :id "c" :prompt "Cold" :launch launch))
           (cons 'lift (make-magpi-action
                        :id "l" :prompt "Lift" :launch launch
                        :observation (magpi-observation-initial)))
           (cons 'aloft (make-magpi-action
                         :id "a" :prompt "Aloft" :launch launch
                         :observation (make-magpi-observation
                                       :activity-state 'running
                                       :connection-state 'connected)))
           (cons 'rest (make-magpi-action
                        :id "r" :prompt "Rest" :launch launch
                        :observation (make-magpi-observation
                                      :activity-state 'idle
                                      :connection-state 'connected)))
           (cons 'blood (make-magpi-action
                         :id "b" :prompt "Blood" :launch launch
                         :observation (make-magpi-observation
                                       :activity-state 'running
                                       :connection-state 'disconnected))))))
    (dolist (row rows)
      (let* ((hour (car row))
             (action (cdr row))
             (motion (magpi-status--auspice-motion hour)))
        (should (eq (magpi-observation-auspice (magpi-action-observation action))
                    hour))
        (should (equal (car (car (magpi-status--heading-parts action))) motion))
        (with-temp-buffer
          (magpi-status--insert-action action)
          (let ((heading (car (split-string (substring-no-properties (buffer-string))
                                            "\n"))))
            (should (string-match-p (regexp-quote motion) heading))
            (should-not (string-match-p "starting" heading))))))))

(ert-deftest magpi-status-pending-ask-overlays-attention-without-recoding-auspice ()
  (let* ((launch (magpi-launch-build "/tmp/" 'medium 'writer '(:kind none)))
         (action (make-magpi-action
                  :id "a1" :prompt "Review" :launch launch
                  :observation
                  (make-magpi-observation
                   :activity-state 'running
                   :connection-state 'connected
                   :asks (list (make-magpi-ask
                                :id "q1" :question "Apply?" :state 'pending))))))
    (should (eq (magpi-observation-auspice (magpi-action-observation action))
                'aloft))
    (should (equal (car (car (magpi-status--heading-parts action)))
                   (magpi-status--auspice-motion 'aloft)))
    (with-temp-buffer
      (magpi-status--insert-action action)
      (let ((heading (car (split-string (substring-no-properties (buffer-string))
                                        "\n"))))
        (should (string-match-p (regexp-quote (magpi-status--auspice-motion 'aloft))
                                heading))
        (should (string-prefix-p "? " heading))
        (should (string-match-p
                 (regexp-quote (magpi-status--retention-mark 'hears)) heading))))))

(ert-deftest magpi-status-heading-uses-historical-session-label ()
  "Promise: observed → historical callback → fallback; cold stays cold."
  (let* ((cold (make-magpi-action :id "hist-1" :chat-ref "hist-1"))
         (live (make-magpi-action
                :id "live-1" :prompt "Prompt"
                :observation (make-magpi-observation
                              :display-title "Live title"
                              :activity-state 'running
                              :connection-state 'connected)))
         (magpi-status-action-title-function (lambda (_) "Historical why")))
    (should (equal (substring-no-properties (magpi-status--heading cold))
                   "Historical why"))
    (should (equal (magpi-status--heading-suffix cold)
                   (magpi-status--auspice-motion 'cold)))
    (should-not (magpi-action-observation cold))
    (should (equal (substring-no-properties (magpi-status--heading live))
                   "Live title"))
    (let ((action (make-magpi-action :id "a1" :title "Standalone title"))
          (magpi-status-action-title-function (lambda (_) nil)))
      (should (equal (substring-no-properties (magpi-status--heading action))
                     "Standalone title")))
    (let ((action (make-magpi-action :id "missing"))
          (magpi-status-action-title-function (lambda (_) nil)))
      (should (equal (substring-no-properties (magpi-status--heading action))
                     "New task"))
      (should (equal (magpi-status--heading-suffix action)
                     (magpi-status--auspice-motion 'cold))))))

(ert-deftest magpi-status-insert-action-pairs-user-with-indented-last ()
  "Promise: heading is the user task; last assistant is indented, not the title."
  (let* ((cold (make-magpi-action :id "hist-1" :chat-ref "hist-1"))
         (live (make-magpi-action
                :id "live-1" :prompt "First task"
                :observation (make-magpi-observation
                              :display-title "Named chat"
                              :last-prompt "Keep the race only"
                              :last-response "Migration removed"
                              :activity-state 'idle
                              :connection-state 'connected)))
         (magpi-status-action-title-function
          (lambda (_) (cons "Real task" "Last activity"))))
    (with-temp-buffer
      (magpi-status--insert-action cold)
      (let ((text (substring-no-properties (buffer-string)))
            (heading (car (split-string (substring-no-properties (buffer-string))
                                        "\n"))))
        (should (string-prefix-p "Real task" (string-trim-left heading)))
        (should-not (string-match-p "Last activity" heading))
        (should (string-match-p "^    Last activity$" text))
        (should-not (string-match-p "last       " text))))
    (with-temp-buffer
      (magpi-status--insert-action live)
      (let ((text (substring-no-properties (buffer-string)))
            (heading (car (split-string (substring-no-properties (buffer-string))
                                        "\n"))))
        (should (string-prefix-p "Named chat" (string-trim-left heading)))
        (should (string-match-p "^    Keep the race only$" text))
        (should (string-match-p "^      Migration removed$" text))
        (should-not (string-match-p "last       " text))))))

(ert-deftest magpi-status-prepare-refreshes-title-snapshot-without-event-paint ()
  "Promise: `g' refreshes the title snapshot; event paint reuses it."
  (let* ((label "Before")
         (order nil)
         (action (make-magpi-action :id "a1")))
    (cl-letf (((symbol-function 'magit-refresh-buffer)
               (lambda ()
                 (push 'paint order)
                 (insert (substring-no-properties (magpi-status--heading action))))))
      (with-temp-buffer
        (setq-local magpi-status-prepare-function
                    (lambda ()
                      (push 'prepare order)
                      (setq label "After g")))
        (setq-local magpi-status-action-title-function
                    (lambda (_) label))
        (should (equal (substring-no-properties (magpi-status--heading action))
                       "Before"))
        (magpi-status-refresh)
        (should (equal (nreverse order) '(prepare paint)))
        (should (equal (substring-no-properties (buffer-string)) "After g"))
        (setq order nil)
        (erase-buffer)
        (magit-refresh-buffer)
        (should (equal order '(paint)))
        (should (equal (substring-no-properties (buffer-string)) "After g"))))))
(ert-deftest magpi-status-heading-meta-shows-age ()
  (cl-letf (((symbol-function 'magpi-status--now) (lambda () 280)))
    (let ((action (make-magpi-action
                   :id "a1" :prompt "Go" :created-at 100
                   :observation (make-magpi-observation
                                 :activity-state 'running
                                 :connection-state 'connected))))
      (should (equal (car (last (mapcar #'car (magpi-status--heading-parts action))))
                     "3m")))))

(ert-deftest magpi-status-history-loading-is-pending-meta-not-an-hour ()
  "Promise: filling preexisting chat is a quiet spark, not lift or aloft."
  (let* ((cold (make-magpi-action :id "hist-1" :chat-ref "hist-1"))
         (magpi-status-action-loading-function (lambda (_) "loading 12"))
         (spark (progn
                  (cl-letf (((symbol-function 'magpi-status--fetch-index) (lambda () 0)))
                    (magpi-status--fetch-motion)))))
    (should (eq (magpi-observation-auspice (magpi-action-observation cold)) 'cold))
    (cl-letf (((symbol-function 'magpi-status--fetch-index) (lambda () 0)))
      (should (equal (mapcar #'car (magpi-status--heading-parts cold))
                     (list (magpi-status--auspice-motion 'cold) spark)))
      (should (eq (cdr (nth 1 (magpi-status--heading-parts cold)))
                  'magpi-status-quiet))
      (should (= (string-width spark) magpi-status-motion-width))
      (with-temp-buffer
        (magpi-status--insert-action cold)
        (let ((heading (car (split-string (substring-no-properties (buffer-string))
                                          "\n"))))
          (should (string-match-p (regexp-quote spark) heading))
          (should (string-match-p (regexp-quote (magpi-status--auspice-motion 'cold))
                                  heading))
          (should-not (string-match-p "loading" heading))
          (should-not (string-match-p "starting" heading))
          (should-not (string-match-p
                       (regexp-quote (magpi-status--auspice-motion 'lift)) heading))
          (should-not (string-match-p
                       (regexp-quote (magpi-status--auspice-motion 'aloft)) heading)))))
    (let ((magpi-status-reduced-motion t))
      (should (equal (magpi-status--fetch-motion) "load."))
      (should (= (string-width (magpi-status--fetch-motion))
                 magpi-status-motion-width)))))
(ert-deftest magpi-status-perch-then-cold-latest-live-first ()
  "Promise: Perch then Cold; latest live at the top; nested stay nested."
  (cl-letf (((symbol-function 'magpi-status--now) (lambda () 1000))
            ((symbol-function 'magpi-intention-git-facts)
             (lambda (_intention) '(:checkout dirty :exists t))))
    (let* ((live (lambda (id title at)
                   (make-magpi-action
                    :id id :title title :created-at at
                    :observation (make-magpi-observation
                                  :activity-state 'running
                                  :connection-state 'connected))))
           (older-live (funcall live "l-old" "Older live" 100))
           (newer-live (funcall live "l-new" "Newer live" 400))
           (cold-chat (make-magpi-action
                       :id "c-old" :title "Cold chat" :created-at 900))
           (intention (make-magpi-intention
                       :id "intent-1" :objective "Ship auth"
                       :state 'active :created-at 50))
           (nested-old (make-magpi-action
                        :id "n-old" :intention-id "intent-1"
                        :title "Nested old" :created-at 80
                        :observation (make-magpi-observation
                                      :activity-state 'idle
                                      :connection-state 'connected)))
           (nested-new (make-magpi-action
                        :id "n-new" :intention-id "intent-1"
                        :title "Nested new" :created-at 300
                        :observation (make-magpi-observation
                                      :activity-state 'running
                                      :connection-state 'connected)))
           (nested-cold (make-magpi-action
                         :id "n-cold" :intention-id "intent-1"
                         :title "Nested cold" :created-at 20)))
      (with-temp-buffer
        (magpi-status--insert-records
         (list intention)
         (list cold-chat nested-cold older-live nested-old newer-live nested-new))
        (let* ((text (substring-no-properties (buffer-string)))
               (case-fold-search nil)
               (perch (substring-no-properties (magpi-status--group-heading 'perch)))
               (cold (substring-no-properties (magpi-status--group-heading 'cold)))
               (cold-line (concat "^" (regexp-quote cold) "$")))
          (should (string-match-p (regexp-quote perch) text))
          (should (string-match-p cold-line text))
          (should (< (string-match (regexp-quote perch) text)
                     (string-match "Newer live" text)))
          (should (< (string-match "Newer live" text)
                     (string-match "Ship auth" text)))
          (should (< (string-match "Nested new" text)
                     (string-match "Nested old" text)))
          (should (< (string-match "Nested cold" text)
                     (string-match "Older live" text)))
          (should (< (string-match "Older live" text)
                     (string-match cold-line text)))
          (should (< (string-match cold-line text)
                     (string-match "Cold chat" text)))
          (should (< (string-match "Nested cold" text)
                     (string-match "Cold chat" text))))))))

(provide 'magpi-status-tests)
;;; magpi-status-tests.el ends here
