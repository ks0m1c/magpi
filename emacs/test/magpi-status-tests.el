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

(ert-deftest magpi-status-target-is-derived-from-section-identity ()
  (magpi-test-with-section-values
   '((magpi-observed-file . ("attempt-1" . "lib/auth.ex")))
   (should (equal (magpi-status-target-at-point)
                  '(:kind observed-file :attempt-id "attempt-1" :path "lib/auth.ex")))))

(ert-deftest magpi-status-attempt-target-is-not-parsed-from-heading-text ()
  (magpi-test-with-section-values
   '((magpi-attempt . "attempt-1"))
   (should (equal (magpi-status-target-at-point)
                  '(:kind attempt :attempt-id "attempt-1")))))

(ert-deftest magpi-status-heading-is-authored-intent-or-placeholder ()
  (let* ((absent (make-magpi-attempt
                  :id "attempt-1"
                  :observation (make-magpi-observation :display-title "Observed title")))
         (authored (make-magpi-attempt
                    :id "attempt-2" :intent "Authored"
                    :observation (make-magpi-observation :display-title "Observed title"))))
    (should (equal (magpi-status--heading authored) "Authored"))
    (let ((placeholder (magpi-status--heading absent)))
      (should (equal (substring-no-properties placeholder) "◯"))
      (should (eq (get-text-property 0 'face placeholder)
                  'magpi-status-placeholder)))
    (setf (magpi-attempt-observation authored) (magpi-observation-initial))
    (should (equal (magpi-status--heading authored) "Authored"))))

(ert-deftest magpi-status-insert-attempt-keeps-title-off-the-heading ()
  (let* ((launch (magpi-launch-build "/tmp/" "Standard" 'writer '(:kind none)))
         (attempt (make-magpi-attempt
                   :id "attempt-1"
                   :intent "Authored objective"
                   :launch launch
                   :observation (make-magpi-observation
                                 :activity-state 'idle
                                 :connection-state 'connected
                                 :display-title "Model summary"))))
    (with-temp-buffer
      (magpi-status--insert-attempt attempt)
      (let* ((text (substring-no-properties (buffer-string)))
             (heading (car (split-string text "\n"))))
        (should (string-prefix-p "Authored objective" heading))
        (should-not (string-match-p "Model summary" heading))
        (should (string-match-p "intent     Authored objective" text))
        (should (string-match-p "title      Model summary" text))))))

(ert-deftest magpi-status-renders-observation-activity-state ()
  (should (equal (magpi-status--activity-state-label 'starting) "starting"))
  (should (equal (magpi-status--activity-state-label 'running) "running"))
  (should (equal (magpi-status--activity-state-label 'idle) "idle"))
  (should (equal (magpi-status--connection-state-label 'disconnected)
                 "disconnected"))
  (should (equal (magpi-status--activity-detail "edit") "edit"))
  (should (eq (magpi-status--activity-state-face 'running)
              'magpi-status-activity-running))
  (should (eq (magpi-status--activity-state-face 'idle)
              'magpi-status-activity-idle)))

(ert-deftest magpi-status-inherits-basic-navigation-not-magit-commands ()
  (should (eq (keymap-parent magpi-status-mode-map) special-mode-map))
  (should-not (lookup-key magpi-status-mode-map (kbd "b"))))

(ert-deftest magpi-status-renders-requested-and-running-models-independently ()
  (should-not (magpi-status--model-label nil nil))
  (should (equal (magpi-status--model-label "openai/gpt-4.1" nil)
                 "openai/gpt-4.1"))
  (should (equal (magpi-status--model-label nil "anthropic/claude-sonnet")
                 "anthropic/claude-sonnet"))
  (should (equal (magpi-status--model-label "openai/gpt-4.1"
                                           "anthropic/claude-sonnet")
                 "openai/gpt-4.1 → anthropic/claude-sonnet"))
  (should (equal (magpi-status--model-label "openai/gpt-4.1" "openai/gpt-4.1")
                 "openai/gpt-4.1")))

(ert-deftest magpi-status-formats-usage-as-scannable-secondary-meta ()
  (should-not (magpi-status--usage-label nil))
  (should (equal (magpi-status--format-count 42) "42"))
  (should (equal (magpi-status--format-count 1500) "1.5k"))
  (should (equal (magpi-status--format-cost 0.45) "$0.45"))
  (should (equal (magpi-status--format-cost 1.2345) "$1.2345"))
  (should (equal (magpi-status--usage-label
                  '(:input 50000 :output 10000 :cache-read 40000
                    :cache-write 5000 :cost 0.45
                    :context-tokens 60000 :context-window 200000))
                 "↑50.0k ↓10.0k R40.0k W5.0k · ctx 60.0k/200.0k · $0.45"))
  (should (equal (magpi-status--usage-label
                  '(:context-tokens nil :context-window 200000))
                 "ctx ?/200.0k")))

(ert-deftest magpi-status-heading-suffix-includes-running-model-and-effort ()
  (let* ((launch (magpi-launch-build "/tmp/" "Standard" 'writer '(:kind none)))
         (attempt (make-magpi-attempt
                   :id "attempt-1"
                   :intent "Refactor tokens"
                   :launch launch
                   :observation (make-magpi-observation
                                 :activity-state 'running
                                 :connection-state 'connected
                                 :running-model "anthropic/claude-sonnet"))))
    (should (equal (magpi-status--heading-suffix attempt)
                   "running · Standard · medium · anthropic/claude-sonnet"))
    (setf (magpi-observation-running-model (magpi-attempt-observation attempt))
          nil)
    (should (equal (magpi-status--heading-suffix attempt)
                   "running · Standard · medium"))))

(ert-deftest magpi-status-face-sets-font-lock-face-for-magit ()
  (let ((text (magpi-status--face "running" 'magpi-status-activity-running)))
    (should (eq (get-text-property 0 'face text)
                'magpi-status-activity-running))
    (should (eq (get-text-property 0 'font-lock-face text)
                'magpi-status-activity-running))))

(ert-deftest magpi-status-insert-attempt-applies-information-hierarchy ()
  (let* ((launch (magpi-launch-build "/tmp/" "Deep" 'writer '(:kind none)
                                     "openai/gpt-4.1" 'high))
         (with-model (make-magpi-attempt
                      :id "attempt-1"
                      :intent "Show model"
                      :launch launch
                      :observation (make-magpi-observation
                                    :activity-state 'idle
                                    :connection-state 'connected
                                    :running-model "openai/gpt-4.1"
                                    :usage '(:input 1500 :output 250
                                             :cost 0.0123
                                             :context-tokens 8000
                                             :context-window 200000)
                                    :observed-files '("lib/auth.ex")
                                    :problem "needs review")))
         (without-model (make-magpi-attempt
                         :id "attempt-2"
                         :intent "Pending model"
                         :launch (magpi-launch-build "/tmp/" "Deep" 'read-only
                                                     '(:kind none))
                         :observation (magpi-observation-initial))))
    (with-temp-buffer
      (magpi-status--insert-attempt with-model)
      (let ((text (substring-no-properties (buffer-string))))
        (should (string-match-p "idle · Deep · high · openai/gpt-4.1" text))
        (should (string-match-p "effort     high" text))
        (should (string-match-p "model      openai/gpt-4.1" text))
        (should (string-match-p
                 "usage      ↑1.5k ↓250 · ctx 8.0k/200.0k · $0.0123" text))
        (should (string-match-p "authority  Writer" text))
        (should (string-match-p "problem    needs review" text))
        (should (string-match-p "lib/auth.ex" text)))
      (goto-char (point-min))
      (should (text-property-any (point-min) (point-max)
                                 'face 'magpi-status-attempt-heading))
      (should (text-property-any (point-min) (point-max)
                                 'font-lock-face 'magpi-status-attempt-heading))
      (should (text-property-any (point-min) (point-max)
                                 'face 'magpi-status-activity-idle))
      (should (text-property-any (point-min) (point-max)
                                 'font-lock-face 'magpi-status-activity-idle))
      (should (text-property-any (point-min) (point-max)
                                 'face 'magpi-status-authority-writer))
      (should (text-property-any (point-min) (point-max)
                                 'face 'magpi-status-problem))
      (should (text-property-any (point-min) (point-max)
                                 'face 'magpi-status-file)))
    (with-temp-buffer
      (magpi-status--insert-attempt without-model)
      (let ((text (substring-no-properties (buffer-string))))
        (should (string-match-p "model      ◯" text))
        (should (string-match-p "effort     high" text))
        (should-not (string-match-p "openai/gpt-4.1" text))))))

(ert-deftest magpi-status-event-paint-does-not-run-prepare-hook ()
  (let (order)
    (with-temp-buffer
      (setq magpi-status-root "/tmp/"
            magpi-status-prepare-function (lambda () (push 'prepare order))
            magpi-status-attempts-function
            (lambda ()
              (push 'read order)
              nil))
      (magpi-status-refresh-buffer)
      (should (equal order '(read)))
      (should (text-property-any (point-min) (point-max)
                                 'face 'magpi-status-header))
      (should (text-property-any (point-min) (point-max)
                                 'face 'magpi-status-empty)))))

(ert-deftest magpi-status-manual-refresh-snapshots-then-paints ()
  (let (order)
    (cl-letf (((symbol-function 'magit-refresh-buffer)
               (lambda () (push 'paint order))))
      (setq magpi-status-prepare-function (lambda () (push 'prepare order)))
      (magpi-status-refresh)
      (should (equal (nreverse order) '(prepare paint))))))

(provide 'magpi-status-tests)
;;; magpi-status-tests.el ends here
