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
(require 'magpi-status-hours)

(defvar magpi--actions)
(defvar magpi--intentions)
;;; Promise map — which seam guards which promise (STRUCTURE.org).
;;
;; view projection   magpi-status-heading-* : observed > authored > frozen title,
;;                   named seats, attention precedence (fault > ask), cold/rest/
;;                   quiescent hours, historical labels, Muninn retention.
;; geometry          magpi-status-heading-squeeze-*, heading-*-lean-right-*,
;;                   layout-width, wrap-safe-width, model-truncates, seats-do-not-wander,
;;                   quiet-grammar (single seat order).
;; casts and faces   magpi-status-auspice-*, *-cast-fallbacks, group-marks,
;;                   faces-inherit-without-background, face-sets-font-lock-face.
;; paint and body    magpi-status-insert-action-*, renders-nested-asks,
;;                   *-shows-chat-tokens, *-pairs-user-with-indented-last.
;; sections          magpi-facet-*, magpi-status-toggle-section, tab-on-root.
;; mode and keys     magpi-status-inherits-basic-navigation, binds-ordinary,
;;                   n-p-cycle-last-seen, jump-toggles, wraps-long-lines.
;; lifecycle         event-paint-does-not-run-prepare, manual-refresh-*,
;;                   prepare-refreshes-title-snapshot, g-snapshots-git-facts,
;;                   perch-then-cold, glance-keeps-only-this-branch,
;;                   branch-reculls-without-checkout.

;; Hours (fixtures and seat readers) live in magpi-status-hours.el so a row
;; can be projected, planned, and read back at any width without a buffer.

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
    (should (equal (magpi-status-view-slot (magpi-status-action-view action) 'title)
                   "Observed title"))
    (should (eq (magpi-status-view-slot (magpi-status-action-view action) 'title-kind)
                'observed))
    (setf (magpi-action-observation action) (magpi-observation-initial))
    (should (equal (magpi-status-view-slot (magpi-status-action-view action) 'title)
                   "First message"))
    (should (eq (magpi-status-view-slot (magpi-status-action-view action) 'title-kind)
                'doing))))

(ert-deftest magpi-status-heading-falls-back-to-frozen-context ()
  (let* ((launch (magpi-launch-build "/tmp/" 'medium 'writer
                                    '(:kind point :file "lib/auth.ex" :line 12)))
         (action (make-magpi-action :id "action-1" :launch launch
                                      :observation (magpi-observation-initial))))
    (should (equal (magpi-status-view-slot (magpi-status-action-view action) 'title)
                   "lib/auth.ex:12"))
    (should (eq (magpi-status-view-slot (magpi-status-action-view action) 'title-kind)
                'doing))))

(ert-deftest magpi-status-insert-action-uses-title-in-heading-not-id ()
  (let* ((launch (magpi-launch-build "/tmp/" 'medium 'writer '(:kind none)))
         (action (make-magpi-action
                   :id "action-1"
                   :prompt "Authored task"
                   :launch launch
                   :observation (make-magpi-observation
                                 :activity-state 'idle
                                 :connection-state 'connected
                                 :display-title "Model summary")))
         (view (magpi-status-action-view action))
         (heading (magpi-status-render-action action 80)))
    (should (equal (magpi-status-view-slot view 'title) "Model summary"))
    (should (eq (magpi-status-view-slot view 'title-kind) 'observed))
    (should (equal (magpi-status-heading-seat heading view 'title 80)
                   "Model summary"))
    (should-not (equal (magpi-status-view-slot view 'title) "Authored task"))
    (should-not (string-match-p "action-1" (magpi-status-view-slot view 'title)))
    (with-temp-buffer
      (magpi-status--insert-action action)
      (should-not (string-match-p "title      "
                                  (substring-no-properties (buffer-string)))))))
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
  (let* ((width 60)
         (observation (make-magpi-observation
                       :activity-state 'running :connection-state 'connected
                       :observed-files '("lib/a.ex")))
         (short (magpi-status-action-view
                 (make-magpi-action :id "s" :prompt "A"
                                    :observation observation)))
         (long (magpi-status-action-view
                (make-magpi-action
                 :id "l"
                 :prompt "A deliberately long title that must yield to fixed seats"
                 :observation observation)))
         (short-plan (magpi-status--heading-plan short width))
         (long-plan (magpi-status--heading-plan long width)))
    (should (equal (magpi-status-plan-start short-plan 'motion)
                   (magpi-status-plan-start long-plan 'motion)))
    (should (equal (magpi-status-plan-start short-plan 'retention)
                   (magpi-status-plan-start long-plan 'retention)))
    (should (equal (nth 2 (magpi-status--plan-slot short-plan 'title))
                   (nth 2 (magpi-status--plan-slot long-plan 'title))))))

(ert-deftest magpi-status-fault-attention-outranks-pending-ask ()
  (let ((action (make-magpi-action
                 :id "a"
                 :observation (make-magpi-observation
                               :activity-state 'running
                               :connection-state 'disconnected
                               :asks (list (make-magpi-ask
                                            :id "q" :state 'pending))))))
    (should (eq (magpi-status--action-attention action) 'fault))
    (should (eq (magpi-status-view-slot (magpi-status-action-view action) 'attention)
                'fault))))

(defun magpi-status-test--faces (val)
  (cond ((null val) nil)
        ((listp val) val)
        (t (list val))))

(defun magpi-status-test--buffer-has-face (face)
  (let ((pos (point-min))
        found)
    (while (and (not found) (< pos (point-max)))
      (when (or (memq face (magpi-status-test--faces (get-text-property pos 'face)))
                (memq face (magpi-status-test--faces
                            (get-text-property pos 'font-lock-face))))
        (setq found t))
      (setq pos (min (or (next-single-property-change pos 'face nil (point-max))
                         (point-max))
                     (or (next-single-property-change pos 'font-lock-face
                                                      nil (point-max))
                         (point-max)))))
    found))

(ert-deftest magpi-status-faces-are-temperatures-without-background ()
  "Perch follows Magit section headings; the repo follows Head."
  (should (eq (face-attribute 'magpi-status-identity :inherit nil) 'default))
  (should (eq (face-attribute 'magpi-status-header :inherit nil)
              'magit-section-heading))
  (should (eq (face-attribute 'magpi-status-repo :inherit nil)
              'magit-branch-local))
  (should (eq (face-attribute 'magpi-status-quiet :inherit nil) 'default))
  (should (eq (face-attribute 'magpi-status-title :inherit nil)
              'font-lock-string-face))
  (should (eq (face-attribute 'magpi-status-live :inherit nil) 'success))
  (should (eq (face-attribute 'magpi-status-pending :inherit nil) 'warning))
  (should (eq (face-attribute 'magpi-status-alert :inherit nil) 'error))
  (should (eq (face-attribute 'magpi-status-evidence :inherit nil) 'link))
  (dolist (face '(magpi-status-identity magpi-status-title magpi-status-pending
                   magpi-status-live magpi-status-quiet magpi-status-alert
                   magpi-status-header magpi-status-evidence magpi-status-repo))
    (should-not (seq-some (lambda (x) (and (stringp x) (string-prefix-p "#" x)))
                          (flatten-tree (get face 'face-defface-spec))))
    (should (eq (face-attribute face :background nil) 'unspecified))))

(ert-deftest magpi-status-mix-is-a-shade-of-its-ends ()
  (should (equal (magpi-status--mix "#ffffff" "#000000" 0.0) "#ffffff"))
  (should (equal (magpi-status--mix "#ffffff" "#000000" 1.0) "#000000"))
  (should (equal (magpi-status--mix "#ffffff" "#000000" 0.5) "#7f7f7f")))

(ert-deftest magpi-status-heading-cells-are-fixed-pitch ()
  (let* ((cell (magpi-status--heading-cell "Why" 'magpi-status-identity))
         (faces (magpi-status-test--faces (get-text-property 0 'face cell))))
    (should (memq 'magpi-status-identity faces))
    (should (memq 'fixed-pitch faces))
    (should (equal faces (magpi-status-test--faces
                          (get-text-property 0 'font-lock-face cell))))))


(ert-deftest magpi-status-why-is-identity-doing-is-title ()
  "Promise: Intention why is bold identity; Action title is the string face."
  (should (eq (cdr (magpi-status--slot-cell
                    (magpi-status-intention-view
                     (magpi-status-hour-intention) nil)
                    'title))
              'magpi-status-identity))
  (should (eq (cdr (magpi-status--slot-cell
                    (magpi-status-action-view (magpi-status-hour-lift))
                    'title))
              'magpi-status-title))
  (should (eq (face-attribute 'magpi-status-identity :weight nil) 'bold))
  (should (eq (face-attribute 'magpi-status-title :weight nil) 'normal)))

(ert-deftest magpi-status-nested-actions-are-indented ()
  "Promise: nested Actions sit under the why; standalone stay flush."
  (let* ((intention (magpi-status-hour-intention))
         (nested (make-magpi-action
                  :id "n1" :intention-id "intent-1" :title "Nested doing"
                  :observation (magpi-observation-initial)))
         (flush (substring-no-properties
                 (magpi-status--render-heading
                  (magpi-status-action-view nested))))
         (inset (substring-no-properties
                 (magpi-status--render-heading
                  (magpi-status-action-view nested)
                  magpi-status-action-indent))))
    (should (equal (string-match "Nested doing" flush) magpi-status-attention-width))
    (should (equal (string-match "Nested doing" inset)
                   (+ magpi-status-action-indent magpi-status-attention-width)))
    (should (= (string-width inset) (string-width flush)))
    (with-temp-buffer
      (magpi-status--insert-intention intention (list nested))
      (let ((text (substring-no-properties (buffer-string))))
        (should (string-match-p "Ship auth" text))
        (should (string-match-p (regexp-quote inset) text))))
    (with-temp-buffer
      (magpi-status--insert-action nested)
      (should (string-match-p (regexp-quote flush)
                              (substring-no-properties (buffer-string)))))))

(ert-deftest magpi-status-storehouse-and-preview-are-not-telemetry ()
  "Perch and Cold use Magit's section-heading cue.  Last reply recedes."
  (should (memq 'magpi-status-header
                (magpi-status-test--faces
                 (get-text-property 0 'face (magpi-status--group-heading 'perch)))))
  (should (memq 'magpi-status-header
                (magpi-status-test--faces
                 (get-text-property 0 'face (magpi-status--group-heading 'cold)))))
  (with-temp-buffer
    (magpi-status--insert-pair "Keep the race only" "Migration removed" "Named chat")
    (let ((text (buffer-string)))
      (should (eq (get-text-property (string-match "Migration removed" text)
                                     'face text)
                  'magpi-status-quiet)))))

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
    (should (eq (magpi-status-view-slot (magpi-status-action-view aloft) 'motion) 'aloft))
    (should (eq (magpi-status-view-slot (magpi-status-action-view problem) 'motion) 'blood))
    (should (eq (magpi-status-view-slot (magpi-status-action-view disconnected) 'motion) 'blood))
    (should (eq (magpi-status--auspice-face 'aloft) 'magpi-status-live))
    (should (eq (magpi-status--auspice-face 'blood) 'magpi-status-alert))))

(ert-deftest magpi-status-inherits-basic-navigation-not-magit-commands ()
  (should (eq (keymap-parent magpi-status-mode-map) special-mode-map))
  (should (eq (lookup-key magpi-status-mode-map (kbd "b"))
              #'magpi-status-branch))
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
    (should word-wrap)
    (should (buffer-local-value 'doom-real-buffer-p (current-buffer)))))
(ert-deftest magpi-status-heading-seats-are-named-not-a-bag ()
  (let* ((action (make-magpi-action
                  :id "action-1"
                  :prompt "Refactor tokens"
                  :launch (magpi-status-test-launch)
                  :observation (make-magpi-observation
                                :activity-state 'running
                                :connection-state 'connected
                                :running-model "anthropic/claude-sonnet")))
         (view (magpi-status-action-view action)))
    (should (eq (magpi-status-view-slot view 'motion) 'aloft))
    (should (equal (magpi-status-view-slot view 'model) "anthropic/claude-sonnet"))
    (should (equal (magpi-status-view-slot view 'effort) "medium"))
    (should-not (magpi-status-view-slot view 'lease))
    (setf (magpi-observation-running-model (magpi-action-observation action)) nil)
    (should-not (magpi-status-view-slot (magpi-status-action-view action) 'model))))

(ert-deftest magpi-status-heading-model-is-running-not-requested ()
  "Promise: heading model is observed running; requested stays in the body."
  (let* ((requested (magpi-launch-build "/tmp/" 'medium 'writer '(:kind none)
                                        "openai/gpt-4.1"))
         (idle (make-magpi-action
                :id "idle-1" :prompt "Idle" :launch requested
                :observation (make-magpi-observation
                              :activity-state 'idle
                              :connection-state 'connected)))
         (live (make-magpi-action
                :id "live-1" :prompt "Live" :launch requested
                :observation (make-magpi-observation
                              :activity-state 'running
                              :connection-state 'connected
                              :running-model "anthropic/claude-sonnet"
                              :usage '(:context 9000 :window 200000)))))
    (should-not (magpi-status-view-slot (magpi-status-action-view idle) 'model))
    (should (eq (magpi-status-view-slot (magpi-status-action-view idle) 'motion) 'rest))
    (should (equal (magpi-status-view-slot (magpi-status-action-view live) 'model)
                   "anthropic/claude-sonnet"))
    (let* ((view (magpi-status-action-view live 280))
           (heading (magpi-status-render-action live 80 280))
           (seat (magpi-status-heading-seat heading view 'model 80)))
      (should (memq 'model (magpi-status-plan-slots
                            (magpi-status--heading-plan view 80))))
      (should seat)
      (should (string-match-p "sonnet" seat))
      (should-not (string-match-p "gpt-4.1" heading)))
    (with-temp-buffer
      (magpi-status--insert-action idle)
      (let ((text (substring-no-properties (buffer-string))))
        (should (string-match-p "requested +openai/gpt-4.1" text))
        (should-not (string-match-p "^    model +" text))))))
(ert-deftest magpi-status-quiet-grammar-is-the-canon-order ()
  "The single quiet-seat grammar is STRUCTURE.org's, left to right.
  Age is its right edge; a row never holds seats out of this order."
  (should (equal magpi-status-quiet-order
                 '(lease history git model effort context age))))

(ert-deftest magpi-status-layout-width-cannot-overstate-a-visible-window ()
  (cl-letf (((symbol-function 'get-buffer-window-list) (lambda (&rest _) '(fake)))
            ((symbol-function 'window-body-width) (lambda (_) 40)))
    (let ((magpi-status-layout-width 80))
      (should (= (magpi-status--render-width) 40))))
  (let ((magpi-status-layout-width 64))
    (should (= (magpi-status--render-width) 64))))

(ert-deftest magpi-status-visible-width-is-wrap-safe-fixed-pitch ()
  "Promise: live windows sample wrap-safe fixed-pitch columns, not body width."
  (let (seen)
    (cl-letf (((symbol-function 'get-buffer-window-list)
               (lambda (&rest _) '(win)))
              ((symbol-function 'window-live-p)
               (lambda (window) (eq window 'win)))
              ((symbol-function 'window-max-chars-per-line)
               (lambda (window face)
                 (push (list window face) seen)
                 37))
              ((symbol-function 'window-body-width)
               (lambda (_) 40)))
      (should (= (magpi-status--render-width) 36))
      (should (equal seen '((win fixed-pitch)))))))

(ert-deftest magpi-status-wrap-safe-width-ignores-zero-and-never-overstates ()
  "Promise: a bad wrap sample cannot beat body width."
  (cl-letf (((symbol-function 'get-buffer-window-list)
             (lambda (&rest _) '(win)))
            ((symbol-function 'window-live-p)
             (lambda (_) t))
            ((symbol-function 'window-body-width)
             (lambda (_) 40)))
    (cl-letf (((symbol-function 'window-max-chars-per-line)
               (lambda (&rest _) 0)))
      (should (= (magpi-status--render-width) 39)))
    (cl-letf (((symbol-function 'window-max-chars-per-line)
               (lambda (&rest _) 90)))
      (should (= (magpi-status--render-width) 39)))))
(ert-deftest magpi-status-two-windows-use-the-narrowest ()
  (cl-letf (((symbol-function 'get-buffer-window-list)
             (lambda (&rest _) '(wide narrow)))
            ((symbol-function 'window-body-width)
             (lambda (window) (if (eq window 'wide) 80 40))))
    (should (= (magpi-status--render-width) 40))))

(ert-deftest magpi-status-resize-repaints-without-snapshot ()
  "Promise: resize is magit-refresh-buffer only; `g' still snapshots."
  (let (order
        (magpi-status--resize-timer nil))
    (cl-letf (((symbol-function 'run-at-time)
               (lambda (_delay _repeat fn &rest args)
                 (apply fn args)
                 'immediate))
              ((symbol-function 'window-list)
               (lambda (&rest _) '(win)))
              ((symbol-function 'window-buffer)
               (lambda (_) (current-buffer)))
              ((symbol-function 'magit-refresh-buffer)
               (lambda () (push 'paint order))))
      (with-temp-buffer
        (magpi-status-mode)
        (setq-local magpi-status-prepare-function
                    (lambda () (push 'prepare order)))
        (magpi-status--on-window-size-change (selected-frame))
        (should (equal order '(paint)))
        (should-not (memq 'prepare order))
        (setq order nil)
        (magpi-status-refresh)
        (should (equal (nreverse order) '(prepare paint)))))))

(ert-deftest magpi-status-resize-repaints-every-visible-buffer ()
  "Promise: one debounce still paints every visible Magpi status buffer."
  (let (order
        (magpi-status--resize-timer nil)
        (a (get-buffer-create " *magpi-resize-a*"))
        (b (get-buffer-create " *magpi-resize-b*")))
    (unwind-protect
        (cl-letf (((symbol-function 'run-at-time)
                   (lambda (_delay _repeat fn &rest args)
                     (apply fn args)
                     'immediate))
                  ((symbol-function 'frame-list)
                   (lambda () '(frame-a frame-b)))
                  ((symbol-function 'window-list)
                   (lambda (frame &rest _)
                     (list (if (eq frame 'frame-a) 'win-a 'win-b))))
                  ((symbol-function 'window-buffer)
                   (lambda (window)
                     (if (eq window 'win-a) a b)))
                  ((symbol-function 'magit-refresh-buffer)
                   (lambda ()
                     (push (current-buffer) order))))
          (with-current-buffer a (magpi-status-mode))
          (with-current-buffer b (magpi-status-mode))
          (magpi-status--on-window-size-change 'frame-a)
          (should (eq (length order) 2))
          (should (memq a order))
          (should (memq b order)))
      (when (buffer-live-p a) (kill-buffer a))
      (when (buffer-live-p b) (kill-buffer b)))))
(ert-deftest magpi-status-heading-squeeze-is-total-and-atomic ()
  (pcase-let* ((action (magpi-status-hour-crowded))
               (view (magpi-status-action-view action 280))
               (`(,title-min . ,title-max) (magpi-status--title-bounds))
               (leased (magpi-status--intention-heading-view
                        (magpi-status-hour-intention t) nil
                        (magpi-status-hour-git-dirty) 280))
               (wide (magpi-status--heading-plan view 120))
               (compact (magpi-status--heading-plan view 64))
               (narrow (magpi-status--heading-plan view 28))
               (lease-narrow (magpi-status--heading-plan leased 30))
               (emergency (magpi-status--heading-plan view 20))
               (ultra (magpi-status--heading-plan view 10)))
    (should (eq (magpi-status-view-slot view 'motion) 'aloft))
    (should-not (magpi-status-view-slot view 'history))
    (should (equal (magpi-status-plan-slots wide)
                   '(attention title motion retention model effort context age)))
    (let ((compact-alloc (magpi-status--allocate view 64 title-min title-max)))
      (should (memq 'model (nth 2 compact-alloc)))
      (should-not (memq 'effort (nth 2 compact-alloc)))
      (should-not (memq 'context (nth 2 compact-alloc)))
      (should (>= (nth 1 compact-alloc) title-min))
      (should (memq 'age (nth 2 compact-alloc))))
    (should (equal (magpi-status-plan-slots narrow)
                   '(attention title motion retention)))
    (should (member 'lease (magpi-status-plan-slots lease-narrow)))
    (should (member 'git (magpi-status-plan-slots lease-narrow)))
    (should-not (member 'motion (magpi-status-plan-slots lease-narrow)))
    (should (equal (magpi-status-plan-slots emergency)
                   '(attention title motion retention)))
    (should (= (nth 2 (magpi-status--plan-slot emergency 'title)) 10))
    (should (equal (magpi-status-plan-slots ultra) '(title)))
    (should-not (magpi-status--plan-slot ultra 'motion))
    (let ((seen '(effort context age model))
          (prev-slots (magpi-status-plan-slots wide)))
      (dolist (width (number-sequence 119 28 -1))
        (pcase-let* ((`(,mode ,title-w ,admitted _)
                      (magpi-status--allocate view width title-min title-max))
                     (slots (append '(attention title motion retention) admitted)))
          (should (eq mode 'normal))
          (should (>= title-w title-min))
          (let ((gone (seq-difference prev-slots slots)))
            (should (equal gone (seq-take seen (length gone))))
            (setq seen (nthcdr (length gone) seen)))
          (setq prev-slots slots))))
    (let ((heading (magpi-status-render-action action 120 280))
          (compact-heading (magpi-status-render-action action 64 280)))
      (should (equal (magpi-status-heading-seat heading view 'effort 120) "medium"))
      (should (equal (magpi-status-heading-seat heading view 'context 120) "9k/200k"))
      (should (equal (magpi-status-heading-seat heading view 'age 120) "3m"))
      (should (memq 'model (magpi-status-plan-slots compact)))
      (should (magpi-status-heading-seat compact-heading view 'model 64))
      (should-not (memq 'context (magpi-status-plan-slots compact)))
      (should-not (magpi-status-heading-seat compact-heading view 'context 64)))))

(ert-deftest magpi-status-ownership-outranks-cast-and-overflows-ornament ()
  "Promise: lease and Git stay on the heading; the cast continues underneath."
  (let* ((leased (magpi-status--intention-heading-view
                  (magpi-status-hour-intention t) nil
                  (magpi-status-hour-git-dirty) 280))
         (wide (magpi-status--heading-plan leased 80))
         (narrow (magpi-status--heading-plan leased 30))
         (magpi-status-layout-width 30))
    (should (member 'motion (magpi-status-plan-slots wide)))
    (should (member 'lease (magpi-status-plan-slots wide)))
    (should (member 'git (magpi-status-plan-slots wide)))
    (should (member 'lease (magpi-status-plan-slots narrow)))
    (should (member 'git (magpi-status-plan-slots narrow)))
    (should-not (member 'motion (magpi-status-plan-slots narrow)))
    (should-not (member 'retention (magpi-status-plan-slots narrow)))
    (should-not (magpi-status--overflow-slots leased narrow))
    (let ((text (with-temp-buffer
                  (magpi-status--insert-intention
                   (magpi-status-hour-intention t) nil
                   (magpi-status-hour-git-dirty) 280)
                  (substring-no-properties (buffer-string)))))
      (should (string-match-p "dirty" text))
      (should (string-match-p "\\bw\\b" text))
      (should-not (string-match-p "\n    " text)))
    (let* ((action (magpi-status-hour-crowded))
           (view (magpi-status-action-view action 280))
           (compact (magpi-status--heading-plan view 64))
           (magpi-status-layout-width 64)
           (text (with-temp-buffer
                   (magpi-status--insert-action action 280)
                   (substring-no-properties (buffer-string)))))
      (should (member 'model (magpi-status-plan-slots compact)))
      (should-not (member 'effort (magpi-status-plan-slots compact)))
      (should (equal (magpi-status--overflow-slots view compact)
                     '(effort context)))
      (should (string-match-p "\n    medium  9k/200k" text)))))
(ert-deftest magpi-status-heading-facts-are-atomic ()
  "Promise: facts fit completely or vanish; only title and model ellipsize."
  (let* ((usage '(:context 123500 :window 10500000))
         (label (magpi-status--context-label usage))
         (action (make-magpi-action
                  :id "a1" :prompt "Go" :created-at 100
                  :launch (magpi-launch-build "/tmp/" 'medium 'writer '(:kind none))
                  :observation (make-magpi-observation
                                :activity-state 'running
                                :connection-state 'connected
                                :usage usage)))
         (view (magpi-status-action-view action 280))
         (heading (magpi-status-render-action action 120 280))
         (seat (magpi-status-heading-seat heading view 'context 120)))
    (should (equal label "123.5k/10.5M"))
    (should (> (string-width label) magpi-status-context-width))
    (should (equal (magpi-status-view-slot view 'context) label))
    (should (equal seat label))
    (should-not (string-match-p "…" (or seat ""))))
  (should (equal (substring-no-properties
                  (magpi-status--fit-atomic "123.5k/10.5M" 10))
                 (make-string 10 ?\s)))
  (should (equal (substring-no-properties
                  (magpi-status--fit-atomic "123.5k/10.5M" 12))
                 "123.5k/10.5M")))

(ert-deftest magpi-status-seat-text-strips-display-properties ()
  (let ((text (propertize "Ship auth" 'display "WIDER-THAN-WHY")))
    (should (equal (magpi-status--seat-text text) "Ship auth"))
    (should-not (text-properties-at 0 (magpi-status--seat-text text)))))

(ert-deftest magpi-status-root-heading-obeys-width ()
  (with-temp-buffer
    (setq magpi-status-root "/tmp/a-very-long-repository-name-overflows/"
          magpi-status-layout-width 20)
    (magpi-status-refresh-buffer)
    (let ((heading (car (split-string (substring-no-properties (buffer-string)) "\n"))))
      (should (= (string-width heading) 20))
      (should (string-match-p "…" heading)))))

(ert-deftest magpi-status-root-heading-shows-checkout-branch ()
  "Promise: MAGPI line names the status checkout's attached branch."
  (cl-letf (((symbol-function 'magpi-git--maybe)
             (lambda (dir &rest args)
               (and (equal dir "/tmp/bridge/")
                    (equal args '("symbolic-ref" "--quiet" "--short" "HEAD"))
                    "magpi/lease"))))
    (with-temp-buffer
      (setq magpi-status-root "/tmp/bridge/"
            magpi-status-layout-width 80
            magpi-status-actions-function (lambda () nil))
      (magpi-status-refresh-buffer)
      (let ((heading (car (split-string (substring-no-properties (buffer-string)) "\n"))))
        (should (string-prefix-p "MAGPI · bridge" heading))
        (should (string-match-p "magpi/lease" heading))))))

(ert-deftest magpi-status-root-heading-shows-why-before-change-branch ()
  "Promise: magpi/<id> keeps the why in front on the MAGPI line."
  (let ((intention (make-magpi-intention
                    :id "lease" :objective "Hold the perch" :state 'active)))
    (cl-letf (((symbol-function 'magpi-git--maybe)
               (lambda (dir &rest args)
                 (and (equal dir "/tmp/bridge/")
                      (equal args '("symbolic-ref" "--quiet" "--short" "HEAD"))
                      "magpi/lease"))))
      (with-temp-buffer
        (setq magpi-status-root "/tmp/bridge/"
              magpi-status-layout-width 80
              magpi-status-actions-function (lambda () nil)
              magpi-status-intentions (list intention))
        (magpi-status-refresh-buffer)
        (let ((heading (car (split-string (substring-no-properties (buffer-string)) "\n"))))
          (should (string-prefix-p "MAGPI · bridge · Hold the perch · magpi/lease" heading)))))))

(ert-deftest magpi-status-root-heading-garden-yields-id-to-why ()
  "Promise: a garden folder that is only the id yields to the why."
  (let ((intention (make-magpi-intention
                    :id "lease" :objective "Hold the perch" :state 'active)))
    (cl-letf (((symbol-function 'magpi-git--maybe)
               (lambda (dir &rest args)
                 (and (equal dir "/tmp/garden/lease/")
                      (equal args '("symbolic-ref" "--quiet" "--short" "HEAD"))
                      "magpi/lease"))))
      (with-temp-buffer
        (setq magpi-status-root "/tmp/garden/lease/"
              magpi-status-layout-width 80
              magpi-status-actions-function (lambda () nil)
              magpi-status-intentions (list intention))
        (magpi-status-refresh-buffer)
        (let ((heading (car (split-string (substring-no-properties (buffer-string)) "\n"))))
          (should (string-prefix-p "MAGPI · Hold the perch · magpi/lease" heading))
          (should-not (string-match-p "MAGPI · lease" heading)))))))
(ert-deftest magpi-status-model-truncates-from-the-left ()
  (should (equal (magpi-status--fit-from-left "provider/specific-id" 12)
                 "…specific-id"))
  (should (equal (magpi-status--fit-from-left "short" 8) "short"))
  (should (equal (magpi-status--take-right "abcdef" 3) "def"))
  (let* ((magpi-status-title-max-width 18)
         (action (make-magpi-action
                  :id "a1" :prompt "Go"
                  :launch (magpi-launch-build "/tmp/" 'medium 'writer '(:kind none))
                  :observation (make-magpi-observation
                                :activity-state 'running
                                :connection-state 'connected
                                :running-model "provider/very-specific-model-id")))
         (view (magpi-status-action-view action))
         (heading (let ((magpi-status-title-max-width 18))
                    (magpi-status-render-action action 64)))
         (seat (let ((magpi-status-title-max-width 18))
                 (magpi-status-heading-seat heading view 'model 64))))
    (should seat)
    (should (string-prefix-p "…" seat))
    (should (string-suffix-p "specific-model-id" seat))
    (should-not (string-match-p "provider/very" seat))))
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
                         :observation (magpi-observation-initial)))
         (with-view (magpi-status-action-view with-model))
         (without-view (magpi-status-action-view without-model)))
    (should (eq (magpi-status-view-slot with-view 'attention) 'fault))
    (should (equal (magpi-status-view-slot with-view 'title) "Show run"))
    (should (eq (magpi-status-view-slot with-view 'motion) 'blood))
    (should (eq (magpi-status-view-slot with-view 'retention) 'hears))
    (should (equal (magpi-status-view-slot with-view 'model) "openai/gpt-4.1"))
    (should-not (magpi-status-view-slot with-view 'lease))
    (should (eq (magpi-status-view-slot without-view 'motion) 'lift))
    (should (eq (magpi-status-view-slot without-view 'retention) 'empty))
    (should (equal (magpi-status-view-slot without-view 'effort) "high"))
    (should-not (magpi-status-view-slot without-view 'model))
    (with-temp-buffer
      (magpi-status--insert-action with-model)
      (let ((text (substring-no-properties (buffer-string))))
        (should (string-match-p "role +writer" text))
        (should (string-match-p "requested +openai/gpt-4.1" text))
        (should (string-match-p "activity   edit" text))
        (should (string-match-p "problem    needs review" text))
        (should (string-match-p "lib/auth.ex" text))
        (should-not (string-match-p "effort" text))
        (should-not (string-match-p "usage" text))
        (should-not (string-match-p "intent     " text))
        (should-not (string-match-p "model      " text))
        (should-not (string-match-p "starting" text))
        (should-not (string-match-p "role +\\bw\\b" text)))
      (goto-char (point-min))
      (should (magpi-status-test--buffer-has-face 'magpi-status-title))
      (should (magpi-status-test--buffer-has-face 'magpi-status-alert))
      (should (magpi-status-test--buffer-has-face 'magpi-status-evidence)))
    (with-temp-buffer
      (magpi-status--insert-action without-model)
      (let ((text (substring-no-properties (buffer-string))))
        (should (string-match-p "role +reader" text))
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
        (should-not (string-match-p "a  react" text))
        (should-not (magpi-status--ask-start-hidden-p
                     (seq-find (lambda (ask) (eq (magpi-ask-state ask) 'pending))
                               (magpi-observation-asks (magpi-action-observation action)))))
        (should (magpi-status--ask-start-hidden-p
                 (seq-find (lambda (ask) (eq (magpi-ask-state ask) 'approved))
                           (magpi-observation-asks (magpi-action-observation action)))))
      (should (text-property-any (point-min) (point-max)
                                 'face 'magpi-status-pending))
      (should (text-property-any (point-min) (point-max)
                                 'face 'magpi-status-live))))))

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
      (should-not order)
      (should (magpi-status-test--buffer-has-face 'magpi-status-repo))
      (should (magpi-status-test--buffer-has-face 'magpi-status-quiet)))))

(ert-deftest magpi-status-event-paint-publishes-live-observations ()
  "Promise: events replace registry objects; paint consumes the published sample."
  (let* ((before (make-magpi-action
                  :id "a1" :prompt "Before"
                  :observation (make-magpi-observation
                                :display-title "Before"
                                :activity-state 'running
                                :connection-state 'connected)))
         (after (make-magpi-action
                 :id "a1" :prompt "After"
                 :observation (make-magpi-observation
                               :display-title "After"
                               :activity-state 'running
                               :connection-state 'connected)))
         (magpi--actions (make-hash-table :test #'equal))
         (reads 0))
    (puthash "a1" after magpi--actions)
    (with-temp-buffer
      (setq magpi-status-root "/tmp/"
            magpi-status-actions (list before)
            magpi-status-actions-function
            (lambda ()
              (setq reads (1+ reads))
              (list before)))
      (magpi-status-refresh-buffer)
      (should (zerop reads))
      (should (eq (car magpi-status-actions) after))
      (let ((text (substring-no-properties (buffer-string))))
        (should (string-match-p "After" text))
        (should-not (string-match-p "Before" text))))))

(ert-deftest magpi-status-event-paint-admits-new-ram-members ()
  "Promise: this Emacs's new Action appears on paint; disk list stays on `g'."
  (let* ((born (make-magpi-action
                :id "born-1" :prompt "New flight"
                :source-root "/tmp/"
                :observation (make-magpi-observation
                              :display-title "New flight"
                              :activity-state 'running
                              :connection-state 'connected)))
         (magpi--actions (make-hash-table :test #'equal))
         (reads 0))
    (puthash "born-1" born magpi--actions)
    (with-temp-buffer
      (setq magpi-status-root "/tmp/"
            magpi-status-actions nil
            magpi-status-actions-function
            (lambda ()
              (setq reads (1+ reads))
              nil))
      (magpi-status-refresh-buffer)
      (should (zerop reads))
      (should (eq (car magpi-status-actions) born))
      (let ((text (substring-no-properties (buffer-string))))
        (should (string-match-p "New flight" text))))))

(ert-deftest magpi-status-event-paint-drops-ended-intentions ()
  "Promise: this Emacs's discarded Intention leaves glance without `g'."
  (let* ((active (make-magpi-intention
                  :id "intent-1" :objective "Ship auth" :state 'active
                  :source-root "/tmp/"))
         (ended (copy-magpi-intention active))
         (magpi--intentions (make-hash-table :test #'equal))
         (reads 0))
    (setf (magpi-intention-state ended) 'discarded)
    (puthash "intent-1" ended magpi--intentions)
    (with-temp-buffer
      (setq magpi-status-root "/tmp/"
            magpi-status-intentions (list active)
            magpi-status-intentions-function
            (lambda ()
              (setq reads (1+ reads))
              (list active)))
      (magpi-status-refresh-buffer)
      (should (zerop reads))
      (should-not magpi-status-intentions)
      (let ((text (substring-no-properties (buffer-string))))
        (should-not (string-match-p "Ship auth" text))
        (should (string-match-p "i  create intention" text))))))

(ert-deftest magpi-status-empty-surface-is-ordinary-loop ()
  (should (eq (magpi-status--subject-glance nil nil) 'empty))
  (with-temp-buffer
    (setq magpi-status-root "/tmp/magpi-project/"
          magpi-status-actions-function (lambda () nil))
    (magpi-status-refresh-buffer)
    (let ((text (substring-no-properties (buffer-string))))
      (should (string-match-p "MAGPI · magpi-project" text))
      (should (string-match-p "i  create intention" text))
      (should (string-match-p "s  spawn action" text))
      (should-not (string-match-p "@  bind context" text))
      (should-not (string-match-p "a  react" text))
      (should-not (string-match-p "Planes" text))
      (should-not (string-match-p "intention surface" text))
      (should-not (string-match-p "Ready" text))
      (should-not (string-match-p "starting" text))
      (should-not (string-match-p "quiescent" text))
      (should-not (string-match-p "\\<rest\\>" text)))))

(ert-deftest magpi-status-root-roost-sits-at-sampled-width ()
  (with-temp-buffer
    (setq-local magpi-status-root "/tmp/bridge/"
                magpi-status-layout-width 60
                magpi-status-actions-function (lambda () nil))
    (magpi-status-refresh-buffer)
    (let ((heading (car (split-string (substring-no-properties (buffer-string)) "\n"))))
      (should (string-prefix-p "MAGPI · bridge" heading))
      (should (= (string-width heading) 60))
      (should (string-match-p
               (regexp-quote (magpi-status--retention-mark 'empty))
               heading)))))
(ert-deftest magpi-status-manual-refresh-snapshots-then-paints ()
  (let (order)
    (cl-letf (((symbol-function 'magit-refresh-buffer)
               (lambda () (push 'paint order))))
      (setq magpi-status-prepare-function (lambda () (push 'prepare order)))
      (magpi-status-refresh)
      (should (equal (nreverse order) '(prepare paint))))))

(ert-deftest magpi-status-intention-header-is-concise-git-dashboard ()
  (let* ((intention (make-magpi-intention
                     :id "intent-1" :objective "Ship auth repair"
                     :target-ref "main" :state 'active
                     :writer-lease '(:action-id "t1")))
         (facts (magpi-status-hour-git-dirty))
         (view (magpi-status-intention-view intention nil facts))
         (heading (magpi-status-render-intention intention nil facts 100)))
    (should (equal (magpi-status-view-slot view 'title) "Ship auth repair"))
    (should (eq (magpi-status-view-slot view 'title-kind) 'authored))
    (should (eq (magpi-status-view-slot view 'motion) 'quiescent))
    (should (eq (magpi-status-view-slot view 'retention) 'hears))
    (should (eq (magpi-status-view-slot view 'lease) t))
    (should (equal (magpi-status-view-slot view 'git) "dirty · +3 -1"))
    (should (equal (magpi-status-heading-seat heading view 'lease 100) "w"))
    (should (equal (magpi-status-heading-seat heading view 'git 100) "dirty · +3 -1"))
    (should-not (string-match-p "magpi/" (or (magpi-status-view-slot view 'git) "")))
    (should-not (magpi-status-view-slot view 'attention))))

(ert-deftest magpi-status-unstarted-intention-says-not-started ()
  (let* ((intention (make-magpi-intention
                     :id "intent-1" :objective "Ship auth" :state 'active))
         (facts (magpi-status-hour-git-unstarted))
         (view (magpi-status-intention-view intention nil facts)))
    (should (equal (magpi-status--git-label facts) "not started"))
    (should-not (eq (magpi-status--subject-glance intention nil facts) 'quiescent))
    (should (equal (magpi-status-view-slot view 'title) "Ship auth"))
    (should (equal (magpi-status-view-slot view 'git) "not started"))
    (should-not (eq (magpi-status-view-slot view 'motion) 'quiescent))))

(ert-deftest magpi-status-intention-why-does-not-yield-to-observed-title ()
  (let* ((intention (make-magpi-intention
                     :id "intent-1" :objective "Ship auth" :state 'active))
         (action (make-magpi-action
                  :id "a1" :intention-id "intent-1" :prompt "task"
                  :observation (make-magpi-observation
                                :display-title "Observed flight"
                                :activity-state 'running
                                :connection-state 'connected)))
         (facts '(:checkout dirty :exists t))
         (view (magpi-status-intention-view intention (list action) facts)))
    (should (equal (magpi-status-view-slot view 'title) "Ship auth"))
    (should-not (equal (magpi-status-view-slot view 'title) "Observed flight"))
    (should (equal (magpi-status-view-slot (magpi-status-action-view action) 'title)
                   "Observed flight"))
    (with-temp-buffer
      (magpi-status--insert-intention intention (list action) facts)
      (should (string-match-p "Observed flight"
                              (substring-no-properties (buffer-string)))))))

(ert-deftest magpi-status-heading-view-does-not-query-git ()
  "Promise: projectors take facts; they never call Git."
  (let ((calls 0)
        (intention (magpi-status-hour-intention))
        (facts (magpi-status-hour-git-dirty)))
    (cl-letf (((symbol-function 'magpi-intention-git-facts)
               (lambda (_) (setq calls (1+ calls)) facts)))
      (let ((view (magpi-status--intention-heading-view intention nil facts 1000)))
        (should (zerop calls))
        (should (equal (magpi-status-view-slot view 'git) "dirty · +3 -1"))))))

(ert-deftest magpi-status-omitted-facts-stay-missing ()
  "Promise: insert-intention never rev-parses; missing stays missing."
  (let ((calls 0)
        (intention (magpi-status-hour-intention)))
    (cl-letf (((symbol-function 'magpi-intention-git-facts)
               (lambda (_)
                 (setq calls (1+ calls))
                 (magpi-status-hour-git-dirty))))
      (with-temp-buffer
        (magpi-status--insert-intention intention nil)
        (should (zerop calls))
        (let ((text (substring-no-properties (buffer-string)))
              (view (magpi-status--intention-heading-view intention nil nil 1000)))
          (should-not (magpi-status-view-slot view 'git))
          (should-not (string-match-p "dirty" text))
          (should-not (string-match-p "not started" text))))
      (with-temp-buffer
        (magpi-status--insert-records (list intention) nil)
        (should (zerop calls))))))

(ert-deftest magpi-status-g-snapshots-git-facts-event-paint-reuses ()
  "Promise: `g' samples Git once per intention; event paint does not."
  (let* ((calls 0)
         (reads 0)
         (intention (magpi-status-hour-intention t))
         (facts (magpi-status-hour-git-dirty)))
    (cl-letf (((symbol-function 'magpi-intention-git-facts)
               (lambda (_) (setq calls (1+ calls)) facts))
              ((symbol-function 'magit-refresh-buffer)
               (lambda (&rest _) (magpi-status-refresh-buffer))))
      (with-temp-buffer
        (setq magpi-status-root "/tmp/"
              magpi-status-intentions-function
              (lambda ()
                (setq reads (1+ reads))
                (list intention))
              magpi-status-actions-function (lambda () nil))
        (magpi-status-refresh)
        (should (= calls 1))
        (should (= reads 1))
        (should (equal (gethash "intent-1" magpi-status-git-facts) facts))
        (magpi-status-refresh-buffer)
        (should (= calls 1))
        (should (= reads 1))
        (magit-refresh-buffer)
        (should (= calls 1))
        (should (= reads 1))
        (let ((text (substring-no-properties (buffer-string))))
          (should (string-match-p "dirty" text)))))))

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
  (should (eq (lookup-key magpi-status-mode-map (kbd "v"))
              #'magpi-visit-worktree))
  (should (eq (lookup-key magpi-status-mode-map (kbd "o"))
              #'magpi-open))
  (should (eq (lookup-key magpi-status-mode-map (kbd "b"))
              #'magpi-status-branch))
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
  (let* ((action (magpi-status-hour-cold "disk-1"))
         (view (magpi-status-action-view action))
         (heading (magpi-status-render-action action 80)))
    (should (eq (magpi-status-view-slot view 'motion) 'cold))
    (should-not (eq (magpi-status-view-slot view 'motion) 'lift))
    (should (equal (magpi-status-heading-seat heading view 'motion 80)
                   (magpi-status--auspice-motion 'cold)))
    (should-not (equal (magpi-status-heading-seat heading view 'motion 80)
                       (magpi-status--auspice-motion 'lift)))))

(ert-deftest magpi-status-quiescent-is-not-rest ()
  (let* ((resting (magpi-status-hour-rest))
         (lifting (magpi-status-hour-lift))
         (intention (magpi-status-hour-quiescent))
         (facts (magpi-status-hour-git-dirty))
         (view (magpi-status-intention-view intention (list resting) facts)))
    (should (eq (magpi-status-view-slot (magpi-status-action-view resting) 'motion) 'rest))
    (should (equal (magpi-status-view-slot (magpi-status-action-view resting) 'effort) "medium"))
    (should-not (eq (magpi-status-view-slot (magpi-status-action-view resting) 'motion) 'quiescent))
    (should (eq (magpi-status--subject-glance intention (list resting) facts)
                'quiescent))
    (should-not (eq (magpi-status--subject-glance intention (list lifting) facts)
                    'quiescent))
    (should (eq (magpi-status-view-slot view 'motion) 'quiescent))
    (should (eq (magpi-status-view-slot (magpi-status-action-view resting) 'motion) 'rest))))

(ert-deftest magpi-status-auspice-rows-are-legible-without-motion ()
  "Promise: every action auspice reconstructs as a still motion seat."
  (dolist (row (list (cons 'cold (magpi-status-hour-cold))
                     (cons 'lift (magpi-status-hour-lift))
                     (cons 'aloft (magpi-status-hour-aloft))
                     (cons 'rest (magpi-status-hour-rest))
                     (cons 'blood (magpi-status-hour-blood))))
    (let* ((hour (car row))
           (action (cdr row))
           (view (magpi-status-action-view action))
           (heading (magpi-status-render-action action 80)))
      (should (eq (magpi-observation-auspice (magpi-action-observation action))
                  hour))
      (should (eq (magpi-status-view-slot view 'motion) hour))
      (should (equal (magpi-status-heading-seat heading view 'motion 80)
                     (magpi-status--auspice-motion hour))))))

(ert-deftest magpi-status-pending-ask-overlays-attention-without-recoding-auspice ()
  (let* ((action (magpi-status-hour-ask))
         (view (magpi-status-action-view action))
         (heading (magpi-status-render-action action 80)))
    (should (eq (magpi-observation-auspice (magpi-action-observation action))
                'aloft))
    (should (eq (magpi-status-view-slot view 'motion) 'aloft))
    (should (eq (magpi-status-view-slot view 'attention) 'ask))
    (should (equal (magpi-status-heading-seat heading view 'attention 80) "?"))
    (should (equal (magpi-status-heading-seat heading view 'motion 80)
                   (magpi-status--auspice-motion 'aloft)))
    (should (equal (magpi-status-heading-seat heading view 'retention 80)
                   (magpi-status--retention-mark 'hears)))))

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
    (should (equal (magpi-status-view-slot (magpi-status-action-view cold) 'title)
                   "Historical why"))
    (should (eq (magpi-status-view-slot (magpi-status-action-view cold) 'motion) 'cold))
    (should-not (magpi-action-observation cold))
    (should (equal (magpi-status-view-slot (magpi-status-action-view live) 'title)
                   "Live title"))
    (should (eq (magpi-status-view-slot (magpi-status-action-view live) 'title-kind)
                'observed))
    (let ((action (make-magpi-action :id "a1" :title "Standalone title"))
          (magpi-status-action-title-function (lambda (_) nil)))
      (should (equal (magpi-status-view-slot (magpi-status-action-view action) 'title)
                     "Standalone title")))
    (let ((action (make-magpi-action :id "missing"))
          (magpi-status-action-title-function (lambda (_) nil)))
      (should (equal (magpi-status-view-slot (magpi-status-action-view action) 'title)
                     "New task"))
      (should (eq (magpi-status-view-slot (magpi-status-action-view action) 'motion) 'cold)))))

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
    (should (equal (magpi-status-view-slot (magpi-status-action-view cold) 'title)
                   "Real task"))
    (should-not (equal (magpi-status-view-slot (magpi-status-action-view cold) 'title)
                       "Last activity"))
    (should (equal (magpi-status-view-slot (magpi-status-action-view live) 'title)
                   "Named chat"))
    (with-temp-buffer
      (magpi-status--insert-action cold)
      (let ((text (substring-no-properties (buffer-string))))
        (should (string-match-p "^    Last activity$" text))
        (should-not (string-match-p "last       " text))))
    (with-temp-buffer
      (magpi-status--insert-action live)
      (let ((text (substring-no-properties (buffer-string))))
        (should (string-match-p "^    Keep the race only$" text))
        (should (string-match-p "^      Migration removed$" text))
        (should-not (string-match-p "last       " text))))))


(ert-deftest magpi-status-last-response-stays-on-one-wrap-safe-line ()
  "Promise: last assistant is one glance line; it never wraps past width."
  (let ((magpi-status-layout-width 40)
        (last (concat "Committed on animation-and-feedback as 628b69f — "
                      "wrap-safe heading width only. Life-play and the proposal rest.")))
    (with-temp-buffer
      (magpi-status--insert-pair "Named chat" last "Named chat")
      (let ((lines (split-string (substring-no-properties (buffer-string)) "\n" t)))
        (should (equal (length lines) 1))
        (should (<= (string-width (car lines)) 40))
        (should (string-suffix-p "…" (car lines)))
        (should (string-prefix-p "    Committed on" (car lines)))))))
(ert-deftest magpi-status-prepare-refreshes-title-snapshot-without-event-paint ()
  "Promise: `g' refreshes the title snapshot; event paint reuses it."
  (let* ((label "Before")
         (order nil)
         (action (make-magpi-action :id "a1")))
    (cl-letf (((symbol-function 'magit-refresh-buffer)
               (lambda ()
                 (push 'paint order)
                 (insert (magpi-status-view-slot
                          (magpi-status-action-view action) 'title)))))
      (with-temp-buffer
        (setq-local magpi-status-prepare-function
                    (lambda ()
                      (push 'prepare order)
                      (setq label "After g")))
        (setq-local magpi-status-action-title-function
                    (lambda (_) label))
        (should (equal (magpi-status-view-slot (magpi-status-action-view action) 'title)
                       "Before"))
        (magpi-status-refresh)
        (should (equal (nreverse order) '(prepare paint)))
        (should (equal (substring-no-properties (buffer-string)) "After g"))
        (setq order nil)
        (erase-buffer)
        (magit-refresh-buffer)
        (should (equal order '(paint)))
        (should (equal (substring-no-properties (buffer-string)) "After g"))))))
(ert-deftest magpi-status-heading-age-seat ()
  (let ((action (make-magpi-action
                 :id "a1" :prompt "Go" :created-at 100
                 :observation (make-magpi-observation
                               :activity-state 'running
                               :connection-state 'connected))))
    (should (equal (magpi-status-view-slot (magpi-status-action-view action 280) 'age)
                   "3m"))))

(ert-deftest magpi-status-heading-drops-writer-for-effort-and-context ()
  (cl-letf (((symbol-function 'magpi-status--now) (lambda () 280)))
    (let* ((launch (magpi-launch-build "/tmp/" 'medium 'writer '(:kind none)))
           (action (make-magpi-action
                    :id "a1" :prompt "Go" :launch launch :created-at 100
                    :observation (make-magpi-observation
                                  :activity-state 'running
                                  :connection-state 'connected
                                  :usage '(:context 9000 :window 200000)))))
      (should (equal (magpi-status--context-label
                      (magpi-observation-usage (magpi-action-observation action)))
                     "9k/200k"))
      (should (equal (magpi-status-view-slot (magpi-status-action-view action 280) 'effort)
                     "medium"))
      (should (equal (magpi-status-view-slot (magpi-status-action-view action 280) 'context)
                     "9k/200k"))
      (should-not (magpi-status-view-slot (magpi-status-action-view action 280) 'lease))
      (let ((view (magpi-status-action-view action 280)))
        (should-not (string-match-p "writer"
                                    (magpi-status-render-action action 80 280)))
        (should (equal (magpi-status-heading-seat
                        (magpi-status-render-action action 80 280) view 'context 80)
                       "9k/200k"))
        (should-not (magpi-status--plan-slot (magpi-status--heading-plan view 80) 'effort))
        (should (equal (magpi-status-heading-seat
                        (magpi-status-render-action action 120 280) view 'effort 120)
                       "medium"))))))

(ert-deftest magpi-status-heading-ages-lean-right-and-align ()
  (let* ((width 80)
         (launch (magpi-launch-build "/tmp/" 'high 'writer '(:kind none)))
         (short-action (make-magpi-action
                        :id "s" :prompt "A" :launch launch :created-at 3990
                        :observation (make-magpi-observation
                                      :activity-state 'running
                                      :connection-state 'connected)))
         (long-action (make-magpi-action
                       :id "l" :prompt "A deliberately long title" :launch launch
                       :created-at 100
                       :observation (make-magpi-observation
                                     :activity-state 'idle
                                     :connection-state 'connected
                                     :usage '(:context 12400 :window 200000))))
         (short (magpi-status-action-view short-action 4000))
         (long (magpi-status-action-view long-action 4000))
         (short-heading (magpi-status-render-action short-action width 4000))
         (long-heading (magpi-status-render-action long-action width 4000)))
    (should (= (string-width short-heading) width))
    (should (= (string-width long-heading) width))
    (should (equal (magpi-status-heading-seat short-heading short 'age width) "now"))
    (should (equal (magpi-status-heading-seat long-heading long 'age width) "1h"))
    (should (equal (magpi-status-heading-seat short-heading short 'effort width) "high"))
    (should (equal (magpi-status-heading-seat long-heading long 'context width) "12.4k/200k"))
    (let ((short-plan (magpi-status--heading-plan short width))
          (long-plan (magpi-status--heading-plan long width)))
      (should (= (+ (magpi-status-plan-start short-plan 'age)
                    (nth 2 (magpi-status--plan-slot short-plan 'age)))
                 width))
      (should (= (+ (magpi-status-plan-start long-plan 'age)
                    (nth 2 (magpi-status--plan-slot long-plan 'age)))
                 width)))))

(ert-deftest magpi-status-own-columns-keeps-string-width ()
  "Pixel pad must not move canonical seats.  Rest is the narrow Huginn cast."
  (let ((rest (magpi-status--auspice-motion 'rest))
        (aloft (magpi-status--auspice-motion 'aloft)))
    (should (= (string-width rest) magpi-status-motion-width))
    (should (= (string-width aloft) magpi-status-motion-width))
    (should (= (string-width rest)
               (string-width (magpi-status--own-columns rest magpi-status-motion-width))))
    (should (= (string-width aloft)
               (string-width (magpi-status--own-columns aloft magpi-status-motion-width))))))

(ert-deftest magpi-status-own-columns-uses-fixed-pitch ()
  "Huginn's pixel pad uses the same pitch the heading samples."
  (let (faces)
    (cl-letf (((symbol-function 'window-font-width)
               (lambda (_window face)
                 (push face faces)
                 10))
              ((symbol-function 'string-pixel-width)
               (lambda (_) 50)))
      (let ((padded (magpi-status--own-columns "x" 8)))
        (should (equal faces '(fixed-pitch)))
        (should (equal (get-text-property (1- (length padded)) 'display padded)
                       '(space :width 3.0)))))))
(ert-deftest magpi-status-history-loading-is-quiet-seat-not-an-hour ()
  "Promise: filling preexisting chat is quiet history, not lift or aloft."
  (let* ((cold (magpi-status-hour-history))
         (magpi-status-action-loading-function (lambda (_) "loading 12"))
         (view (magpi-status-action-view cold))
         (heading (magpi-status-render-action cold 80)))
    (should (eq (magpi-observation-auspice (magpi-action-observation cold)) 'cold))
    (should (eq (magpi-status-view-slot view 'motion) 'cold))
    (should (eq (magpi-status-view-slot view 'history) t))
    (should (equal (magpi-status-heading-seat heading view 'history 80) "load."))
    (should (equal (magpi-status-heading-seat heading view 'motion 80)
                   (magpi-status--auspice-motion 'cold)))
    (should-not (equal (magpi-status-heading-seat heading view 'motion 80)
                       (magpi-status--auspice-motion 'lift)))
    (should-not (equal (magpi-status-heading-seat heading view 'motion 80)
                       (magpi-status--auspice-motion 'aloft)))
    (should-not (string-match-p "loading" heading))
    (should-not (string-match-p "starting" heading))))

(ert-deftest magpi-status-hours-cover-structure-list ()
  (should-not (magpi-status-hour-empty))
  (should (eq (magpi-status-view-slot
               (magpi-status-action-view (magpi-status-hour-cold)) 'motion)
              'cold))
  (should (eq (magpi-status-view-slot
               (magpi-status-action-view (magpi-status-hour-bound)) 'retention)
              'bound))
  (should (eq (magpi-status-view-slot
               (magpi-status-action-view (magpi-status-hour-lift)) 'motion)
              'lift))
  (should (eq (magpi-status-view-slot
               (magpi-status-action-view (magpi-status-hour-aloft)) 'motion)
              'aloft))
  (should (eq (magpi-status-view-slot
               (magpi-status-action-view (magpi-status-hour-rest)) 'motion)
              'rest))
  (should (eq (magpi-status-view-slot
               (magpi-status-action-view (magpi-status-hour-blood)) 'motion)
              'blood))
  (should (eq (magpi-status-view-slot
               (magpi-status-action-view (magpi-status-hour-ask)) 'attention)
              'ask))
  (let ((magpi-status-action-loading-function (lambda (_) "loading 12")))
    (should (eq (magpi-status-view-slot
                 (magpi-status-action-view (magpi-status-hour-history)) 'history)
                t)))
  (let* ((intention (magpi-status-hour-quiescent t))
         (view (magpi-status-intention-view
                intention nil (magpi-status-hour-git-dirty))))
    (should (eq (magpi-status-view-slot view 'motion) 'quiescent))
    (should (eq (magpi-status-view-slot view 'lease) t))
    (should (magpi-status-view-slot view 'git)))
  (let ((view (magpi-status-action-view (magpi-status-hour-crowded) 280)))
    (should (magpi-status-view-slot view 'model))
    (should (magpi-status-view-slot view 'effort))
    (should (magpi-status-view-slot view 'context))
    (should (magpi-status-view-slot view 'age))))
(ert-deftest magpi-status-perch-then-cold-latest-live-first ()
  "Promise: Perch then Cold; latest live at the top; nested stay nested."
  (cl-letf (((symbol-function 'magpi-status--now) (lambda () 1000)))
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
(ert-deftest magpi-status-help-names-marks ()
  (let ((doc (documentation 'magpi-status-mode t)))
    (should (string-match-p "pending Ask" doc))
    (should (string-match-p "fault" doc))
    (should (string-match-p "writer lease" doc))
    (should (string-match-p "load\\." doc))
    (should (string-match-p "aloft" doc))
    (should (string-match-p "hears" doc))))

(ert-deftest magpi-status-heading-never-exposes-fact-fragments ()
  "Promise: a seat is complete or silent; never a plausible fragment."
  (let* ((action (magpi-status-hour-crowded))
         (view (magpi-status-action-view action 280))
         (context (magpi-status-view-slot view 'context))
         (effort (magpi-status-view-slot view 'effort)))
    (should (equal context "9k/200k"))
    (should (equal effort "medium"))
    (dolist (width (number-sequence 1 120))
      (let* ((heading (substring-no-properties
                       (magpi-status-render-action action width 280)))
             (slots (magpi-status-plan-slots
                     (magpi-status--heading-plan view width))))
        (should (<= (string-width heading) width))
        (when (memq 'motion slots)
          (should (= (string-width
                      (magpi-status-heading-seat heading view 'motion width))
                     magpi-status-motion-width)))
        (when (memq 'context slots)
          (should (equal (magpi-status-heading-seat heading view 'context width)
                         context)))
        (when (memq 'effort slots)
          (should (equal (magpi-status-heading-seat heading view 'effort width)
                         effort)))
        (unless (memq 'context slots)
          (should-not (string-match-p (regexp-quote context) heading))
          (should-not (string-match-p "[0-9]k/" heading)))
        (unless (memq 'effort slots)
          (should-not (string-match-p (regexp-quote effort) heading)))))))

(ert-deftest magpi-status-films-end-on-the-still-cast ()
  (should (equal (car (last magpi-status-lift-aloft-film))
                 (magpi-status--auspice-motion 'aloft)))
  (should (equal (car (last magpi-status-aloft-rest-film))
                 (magpi-status--auspice-motion 'rest)))
  (should-not (equal (car magpi-status-lift-aloft-film)
                     (magpi-status--auspice-motion 'lift)))
  (dolist (frame (append magpi-status-lift-aloft-film
                         magpi-status-aloft-rest-film))
    (should (= (string-width frame) magpi-status-motion-width))))

(ert-deftest magpi-status-explicit-frame-is-still-when-nil ()
  (let* ((action (magpi-status-hour-aloft))
         (view (magpi-status-action-view action))
         (still (magpi-status--auspice-motion 'aloft))
         (frame (car magpi-status-lift-aloft-film)))
    (should (equal (magpi-status-heading-seat
                    (magpi-status--render-heading view) view 'motion 80)
                   still))
    (should (equal (magpi-status-heading-seat
                    (magpi-status--render-heading view nil frame)
                    view 'motion 80)
                   frame))
    (should (equal (magpi-status-heading-seat
                    (magpi-status--render-heading view nil still)
                    view 'motion 80)
                   still))))

(defun magpi-status-test--with-film (action fn)
  (let ((magpi--actions (make-hash-table :test #'equal))
        (started nil)
        (repaints 0))
    (cl-letf (((symbol-function 'run-at-time)
               (lambda (delay repeat fn &rest args)
                 (setq started (list delay repeat fn args))
                 'motion-timer))
              ((symbol-function 'timerp)
               (lambda (timer) (eq timer 'motion-timer)))
              ((symbol-function 'cancel-timer)
               (lambda (_) (setq started nil)))
              ((symbol-function 'magpi-status--motion-repaint)
               (lambda () (setq repaints (1+ repaints)))))
      (puthash (magpi-action-id action) action magpi--actions)
      (unwind-protect
          (funcall fn (lambda () started)
                   (lambda () repaints)
                   (lambda ()
                     (when started
                       (apply (nth 2 started) (nth 3 started)))))
        (magpi-status--motion-cancel-all)
        (clrhash magpi-status--hours)))))

(ert-deftest magpi-status-lift-still-does-not-start-a-film ()
  (let ((action (magpi-status-hour-lift)))
    (magpi-status-test--with-film
     action
     (lambda (started-fn &rest _)
       (should-not (magpi-status--motion-sync (magpi-action-id action) 'lift))
       (should-not (funcall started-fn))
       (should-not (gethash (magpi-action-id action) magpi-status--films))))))

(ert-deftest magpi-status-lift-to-aloft-plays-then-lands-still ()
  (let ((action (magpi-status-hour-lift))
        (id nil))
    (magpi-status-test--with-film
     action
     (lambda (started-fn _repaint-fn tick-fn)
       (setq id (magpi-action-id action))
       (magpi-status--motion-sync id 'lift)
       (should (equal (magpi-status--motion-sync id 'aloft)
                      (car magpi-status-lift-aloft-film)))
       (should (funcall started-fn))
       (should (equal (magpi-status--motion-sync id 'aloft)
                      (car magpi-status-lift-aloft-film)))
       (dotimes (_ (1- (length magpi-status-lift-aloft-film)))
         (funcall tick-fn))
       (should (equal (magpi-status--motion-frame id)
                      (car (last magpi-status-lift-aloft-film))))
       (funcall tick-fn)
       (should-not (gethash id magpi-status--films))
       (should-not (funcall started-fn))))))


(ert-deftest magpi-status-aloft-to-rest-is-one-way ()
  (let ((action (magpi-status-hour-aloft)))
    (magpi-status-test--with-film
     action
     (lambda (started-fn &rest _)
       (let ((id (magpi-action-id action)))
         (should-not (magpi-status--motion-sync id 'aloft))
         (should-not (funcall started-fn))
         (should (equal (magpi-status--motion-sync id 'rest)
                        (car magpi-status-aloft-rest-film)))
         (should (equal (car (funcall started-fn))
                        magpi-status--settle-dwell))
         (should-not (member (magpi-status--auspice-motion 'lift)
                             magpi-status-aloft-rest-film)))))))

(ert-deftest magpi-status-blood-cancels-to-the-still-mark ()
  (let ((action (magpi-status-hour-lift))
        (id nil))
    (magpi-status-test--with-film
     action
     (lambda (started-fn &rest _)
       (setq id (magpi-action-id action))
       (magpi-status--motion-sync id 'lift)
       (magpi-status--motion-sync id 'aloft)
       (should (funcall started-fn))
       (should-not (magpi-status--motion-sync id 'blood))
       (should-not (gethash id magpi-status--films))
       (should-not (funcall started-fn))))))

(ert-deftest magpi-status-reduced-motion-is-still-only ()
  (let ((action (magpi-status-hour-lift))
        (magpi-status-reduced-motion t))
    (magpi-status-test--with-film
     action
     (lambda (started-fn &rest _)
       (let ((id (magpi-action-id action)))
         (magpi-status--motion-sync id 'lift)
         (should-not (magpi-status--motion-sync id 'aloft))
         (should-not (funcall started-fn))
         (should-not (gethash id magpi-status--films)))))))

(ert-deftest magpi-status-removing-the-animator-loses-no-state ()
  (let* ((action (magpi-status-hour-aloft))
         (view (magpi-status-action-view action))
         (still (magpi-status--auspice-motion 'aloft)))
    (magpi-status-test--with-film
     (magpi-status-hour-lift)
     (lambda (_started-fn &rest _)
       (magpi-status--motion-sync "lift-1" 'lift)
       (magpi-status--motion-sync "lift-1" 'aloft)
       (magpi-status--motion-cancel-all)
       (should (equal (magpi-status-heading-seat
                       (magpi-status--render-heading view) view 'motion 80)
                      still))
       (should (eq (magpi-status-view-slot view 'motion) 'aloft))))))

(ert-deftest magpi-status-dead-action-cancels-its-timer ()
  (let ((action (magpi-status-hour-lift)))
    (magpi-status-test--with-film
     action
     (lambda (started-fn &rest _)
       (let ((id (magpi-action-id action)))
         (magpi-status--motion-sync id 'lift)
         (magpi-status--motion-sync id 'aloft)
         (should (funcall started-fn))
         (remhash id magpi--actions)
         (magpi-status--motion-sweep)
         (should-not (gethash id magpi-status--films))
         (should-not (funcall started-fn)))))))

(ert-deftest magpi-status-history-stays-still-during-a-frame ()
  (let* ((cold (magpi-status-hour-history))
         (magpi-status-action-loading-function (lambda (_) "loading 12"))
         (view (magpi-status-action-view cold))
         (frame (car magpi-status-lift-aloft-film))
         (heading (magpi-status--render-heading view nil frame)))
    (should (eq (magpi-status-view-slot view 'motion) 'cold))
    (should (equal (magpi-status-heading-seat heading view 'history 80) "load."))
    (should (equal (magpi-status-heading-seat heading view 'motion 80)
                   frame))))

(ert-deftest magpi-status-sample-keeps-only-this-branch ()
  "Promise: glance membership is the invoking checkout's attached branch."
  (let* ((here (make-magpi-intention
                :id "here" :objective "This branch" :state 'active
                :target-ref "refs/heads/master"))
         (elsewhere (make-magpi-intention
                     :id "else" :objective "Other target" :state 'active
                     :target-ref "refs/heads/main"))
         (change (make-magpi-intention
                  :id "why" :objective "On the change" :state 'active
                  :target-ref "refs/heads/master"))
         (unborn (make-magpi-intention
                  :id "new" :objective "Not started" :state 'active))
         (nested (make-magpi-action :id "a-here" :intention-id "here"
                                    :title "Nested here"))
         (foreign (make-magpi-action :id "a-else" :intention-id "else"
                                     :title "Nested else"))
         (alone (make-magpi-action :id "solo" :title "Solo"
                                   :source-root "/tmp/repo/"
                                   :spawn-oid "aaa"))
         (magpi-status-head "master"))
    (cl-letf (((symbol-function 'magpi-store-oid-ancestor-p)
               (lambda (_directory oid ref)
                 (and (equal oid "aaa") (equal ref "master")))))
      (pcase-let ((`(,actions ,intentions)
                   (magpi-status--related
                    (list nested foreign alone)
                    (list here elsewhere change unborn)
                    "master" "/tmp/repo/")))
        (should (equal (mapcar #'magpi-intention-id intentions)
                       '("here" "why" "new")))
        (should (equal (mapcar #'magpi-action-id actions)
                       '("a-here" "solo"))))
      (pcase-let ((`(,actions ,intentions)
                   (magpi-status--related
                    (list nested foreign alone)
                    (list here elsewhere change unborn)
                    "magpi/why" "/tmp/repo/")))
        (should (equal (mapcar #'magpi-intention-id intentions) '("why")))
        (should-not actions))
      (pcase-let ((`(,actions ,intentions)
                   (magpi-status--related
                    (list nested foreign alone)
                    (list here elsewhere change unborn)
                    "magpi/why" "/tmp/work/why/")))
        (should (equal (mapcar #'magpi-intention-id intentions) '("why")))
        (should-not actions)))))

(ert-deftest magpi-status-event-paint-does-not-admit-other-branch ()
  "Promise: paint will not grow glance with another branch's RAM members."
  (let* ((foreign (make-magpi-intention
                   :id "else" :objective "Other" :state 'active
                   :source-root "/tmp/" :target-ref "refs/heads/main"))
         (magpi--intentions (make-hash-table :test #'equal)))
    (puthash "else" foreign magpi--intentions)
    (with-temp-buffer
      (setq magpi-status-root "/tmp/"
            magpi-status-origin "/tmp/"
            magpi-status-ref "master"
            magpi-status-intentions nil
            magpi-status-actions nil)
      (magpi-status-refresh-buffer)
      (should-not magpi-status-intentions))))

(ert-deftest magpi-status-branch-reculls-without-checkout ()
  "Promise: b glances another Magpi branch and does not check it out."
  (let* ((here (make-magpi-intention
                :id "here" :objective "This branch" :state 'active
                :target-ref "refs/heads/master"))
         (change (make-magpi-intention
                  :id "why" :objective "On the change" :state 'active
                  :target-ref "refs/heads/master"))
         (nested (make-magpi-action :id "a-why" :intention-id "why"
                                    :title "Nested why"))
         git
         (magpi--intentions (make-hash-table :test #'equal)))
    (cl-letf (((symbol-function 'magpi-status--read-branch)
               (lambda () "magpi/why"))
              ((symbol-function 'magpi-status--attached-ref)
               (lambda (_directory) "master"))
              ((symbol-function 'magpi-intention-git-facts)
               (lambda (_) nil))
              ((symbol-function 'magpi-store-git)
               (lambda (_directory &rest args)
                 (when (member (car args) '("checkout" "switch" "worktree"))
                   (setq git args)
                   (error "must not checkout"))
                 ""))
              ((symbol-function 'magit-refresh-buffer)
               (lambda (&rest _) (magpi-status-refresh-buffer))))
      (with-temp-buffer
        (setq magpi-status-root "/tmp/repo/"
              magpi-status-origin "/tmp/repo/"
              magpi-status-head "master"
              magpi-status-ref "master"
              magpi-status-intentions-function (lambda () (list here change))
              magpi-status-actions-function (lambda () (list nested)))
        (magpi-status-branch)
        (should-not git)
        (should (equal magpi-status-ref "magpi/why"))
        (should (equal magpi-status-head "master"))
        (should (equal (mapcar #'magpi-intention-id magpi-status-intentions)
                       '("why")))
        (should (equal (mapcar #'magpi-action-id magpi-status-actions)
                       '("a-why")))
        (magpi-status--take-sample)
        (should-not git)
        (should (equal magpi-status-ref "master"))
        (should (equal (mapcar #'magpi-intention-id magpi-status-intentions)
                       '("here" "why")))))))
(provide 'magpi-status-tests)
;;; magpi-status-tests.el ends here
