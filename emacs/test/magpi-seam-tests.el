;;; magpi-seam-tests.el --- Contract tests against real Magit/Transient -*- lexical-binding: t; -*-

;; Test Magpi's promises at the porcelain seams.
;; Domain and host libraries stay real.  Environment quirks are neutralized.
;; Do not assert internal call routes (require, callback symbols, hook lists).

(require 'ert)
(require 'cl-lib)
(require 'seq)

(require 'magpi-seams)
(magpi-seams-require)

(require 'magpi-launch)
(require 'magpi-transient)
(require 'magpi-status)
(require 'magpi-test-repo)

;;;; Launch menu — declarative model contract

(ert-deftest magpi-seam-launch-exposes-frozen-spec-axes ()
  "Promise: launch freezes model, thinking, role, and context.
Role is always present as w|r (never unset, never the symbol quote)."
  (let* ((args (magpi-seams-launch-args))
         (suffix (magpi-seams-role-suffix))
         (rendered (and suffix (transient-format-value suffix))))
    (should (stringp (transient-arg-value "--model=" args)))
    (should (stringp (transient-arg-value "--thinking=" args)))
    (should (stringp (transient-arg-value "--context=" args)))
    (should (member (transient-arg-value "--role=" args) '("w" "r")))
    (should-not (member "--lease" args))
    (should (member (format "--role=%s"
                            (transient-arg-value "--role=" args))
                    args))
    (should suffix)
    (should (equal (oref suffix choices) '("w" "r")))
    (should (seq-every-p #'stringp (oref suffix choices)))
    (should (stringp rendered))
    (should-not (string-match-p "quote" rendered))
    (should (string-match-p "w" rendered))
    (should (string-match-p "r" rendered))))

(ert-deftest magpi-seam-launch-role-stays-binary-under-init ()
  "Promise: Role init from a prefix value always lands on w or r."
  (let* ((suffix (magpi-seams-role-suffix))
         (transient--prefix (get 'magpi-launch 'transient--prefix)))
    (should suffix)
    (dolist (arg '("--role=w" "--role=r"))
      (oset transient--prefix value (list arg))
      (transient-init-value suffix)
      (should (member (oref suffix value) '("--role=w" "--role=r")))
      (should (equal (oref suffix value) arg)))))

(ert-deftest magpi-seam-launch-dispatch-hands-semantic-options ()
  "Promise: dispatch turns live menu values into semantic spawn options."
  (let* ((args (magpi-seams-launch-args))
         (captured nil)
         (magpi-launch-execute-function
          (lambda (options) (setq captured options))))
    (cl-letf (((symbol-function 'transient-args) (lambda (_command) args)))
      (magpi-launch-dispatch))
    (should (stringp (plist-get captured :model)))
    (should (memq (plist-get captured :role) '(writer reader)))
    (should (memq (plist-get captured :bind) '(none point region)))
    (should (plist-member captured :thinking))
    ;; Labels never leak into the semantic contract.
    (should-not (stringp (plist-get captured :role)))
    (should-not (stringp (plist-get captured :thinking)))))

(ert-deftest magpi-seam-launch-dispatch-errors-without-orchestration ()
  "Promise: spawn is refused with a user error, never void-function."
  (let ((magpi-launch-execute-function nil)
        (args (magpi-seams-launch-args)))
    (cl-letf (((symbol-function 'transient-args) (lambda (_command) args))
              ;; Neutralize package load: the observable is the user error.
              ((symbol-function 'require) (lambda (&rest _) nil)))
      (should-error (magpi-launch-dispatch) :type 'user-error))))

;;;; Status — Magit-backed dashboard contract

(ert-deftest magpi-seam-status-opens-on-real-repository ()
  "Promise: status opens a Magpi buffer over a real Git root without void hooks."
  (magpi-test-with-repo (root)
    (let (buffer)
      (unwind-protect
          (progn
            (setq buffer
                  (save-window-excursion
                    (magpi-status-open
                     root
                     (lambda () nil)
                     (lambda () nil))
                    (current-buffer)))
            (with-current-buffer buffer
              (should (derived-mode-p 'magpi-status-mode))
              (should (equal magpi-status-root
                             (file-name-as-directory
                              (expand-file-name root))))
              (should (string-match-p "MAGPI"
                                      (buffer-substring-no-properties
                                       (point-min) (min (point-max) 200))))))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))

(ert-deftest magpi-seam-status-create-collects-intention-text ()
  "Promise: status `i' is intention create, not a status callback."
  (magpi-test-with-repo (root)
    (let* ((collected nil)
           (buffer nil))
      (unwind-protect
          (progn
            (setq buffer
                  (save-window-excursion
                    (magpi-status-open
                     root
                     (lambda () nil)
                     (lambda () nil))
                    (current-buffer)))
            (with-current-buffer buffer
              (cl-letf (((symbol-function 'read-string)
                         (lambda (&rest _) "Repair auth"))
                        ((symbol-function 'magpi-intention-create)
                         (lambda (intent)
                           (interactive (list (read-string "Intention: ")))
                           (setq collected intent))))
                (call-interactively #'magpi-intention-create))
              (should (equal collected "Repair auth"))))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))

(defun magpi-seam--section (type value)
  "Find the first Magit section of TYPE and VALUE anywhere under the root.

Perch/Cold/intention wrappers mean a two-step ident is not the path."
  (cl-labels ((walk (section)
                (when section
                  (if (and (eq (oref section type) type)
                           (equal (oref section value) value))
                      section
                    (cl-some #'walk (oref section children))))))
    (walk magit-root-section)))

(defun magpi-seam--goto (type value)
  (let ((section (magpi-seam--section type value)))
    (should section)
    (goto-char (oref section start))
    section))
(defun magpi-seam--auspice-actions (root)
  (let ((launch (magpi-launch-build root 'medium 'writer '(:kind none))))
    (list
     (make-magpi-action :id "cold-1" :prompt "Cold kernel" :launch launch)
     (make-magpi-action :id "lift-1" :prompt "Lifting" :launch launch
                        :observation (magpi-observation-initial))
     (make-magpi-action
      :id "aloft-1" :prompt "In flight" :launch launch
      :observation (make-magpi-observation
                    :activity-state 'running
                    :connection-state 'connected
                    :observed-files '("lib/auth.ex")))
     (make-magpi-action
      :id "rest-1" :prompt "Settled" :launch launch
      :observation (make-magpi-observation
                    :activity-state 'idle
                    :connection-state 'connected))
     (make-magpi-action
      :id "blood-1" :prompt "Fault" :launch launch
      :observation (make-magpi-observation
                    :activity-state 'running
                    :connection-state 'disconnected)))))

(ert-deftest magpi-seam-status-refresh-preserves-point-and-folds ()
  "Promise: event paint skips `g'; point and Magit folds survive both."
  (magpi-test-with-repo (root)
    (let* ((actions (magpi-seam--auspice-actions root))
           (order nil)
           (buffer nil))
      (unwind-protect
          (progn
            (setq buffer
                  (save-window-excursion
                    (magpi-status-open
                     root
                     (lambda () actions)
                     (lambda () nil)
                     (lambda () (push 'prepare order)))
                    (current-buffer)))
            (with-current-buffer buffer
              (let ((text (buffer-substring-no-properties (point-min) (point-max)))
                    (aloft (magpi-seam--section 'magpi-action "aloft-1"))
                    (rest (magpi-seam--section 'magpi-action "rest-1")))
                (should (string-match-p (regexp-quote (magpi-status--auspice-motion 'cold)) text))
                (should (string-match-p (regexp-quote (magpi-status--auspice-motion 'lift)) text))
                (should (string-match-p (regexp-quote (magpi-status--auspice-motion 'aloft)) text))
                (should (string-match-p (regexp-quote (magpi-status--auspice-motion 'rest)) text))
                (should (string-match-p (regexp-quote (magpi-status--auspice-motion 'blood)) text))
                (should-not (string-match-p "starting" text))
                (should aloft)
                (should rest)
                (magit-section-hide aloft)
                (goto-char (oref rest start))
                (should (oref aloft hidden))
                (should (equal (oref (magit-current-section) value) "rest-1"))
                (setq order nil)
                (magit-refresh-buffer)
                (should-not (memq 'prepare order))
                (setq aloft (magpi-seam--section 'magpi-action "aloft-1"))
                (should (oref aloft hidden))
                (should (equal (oref (magit-current-section) value) "rest-1"))
                (magpi-status-refresh)
                (should (equal order '(prepare)))
                (setq aloft (magpi-seam--section 'magpi-action "aloft-1"))
                (should (oref aloft hidden))
                (should (equal (oref (magit-current-section) value) "rest-1"))
                (let ((after (buffer-substring-no-properties (point-min) (point-max))))
                  (should (string-match-p (regexp-quote (magpi-status--auspice-motion 'aloft)) after))
                  (should (string-match-p (regexp-quote (magpi-status--auspice-motion 'rest)) after))))))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))

(ert-deftest magpi-seam-status-reconstructs-title-without-spawn-or-write ()
  "Promise: cold Action heading uses the title callback; no spawn, no disk write."
  (magpi-test-with-repo (root)
    (let* ((action (make-magpi-action
                    :id "hist-1" :chat-ref "hist-1"
                    :source-root root :created-at 1))
           (buffer nil)
           (spawned nil))
      (unwind-protect
          (progn
            (cl-letf (((symbol-function 'pimacs-chat)
                       (lambda (&rest _) (setq spawned t))))
              (setq buffer
                    (save-window-excursion
                      (magpi-status-open
                       root
                       (lambda () (list action))
                       (lambda () nil)
                       nil
                       (lambda (_action) "Historical why"))
                      (current-buffer)))
              (with-current-buffer buffer
                (let ((text (buffer-substring-no-properties (point-min) (point-max))))
                  (should (string-match-p "Historical why" text))
                  (should (string-match-p (regexp-quote (magpi-status--auspice-motion 'cold)) text))
                  (should-not (string-match-p "New task" text))
                  (should-not (string-match-p "starting" text))))
              (should-not spawned)
              (should-not (file-exists-p
                           (expand-file-name ".git/magpi/actions/hist-1.el" root)))))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))


(defun magpi-seam--contextual-fixtures (root)
  (let* ((launch (magpi-launch-build root 'medium 'writer '(:kind none)))
         (ask (make-magpi-ask :id "q1" :question "Apply this?" :state 'pending
                              :affected-paths '("lib/auth.ex")))
         (nested (make-magpi-action
                  :id "nested-1" :intention-id "intent-1" :title "Nested flight"
                  :launch launch
                  :observation (make-magpi-observation
                                :activity-state 'running
                                :connection-state 'connected
                                :asks (list ask)
                                :observed-files '("lib/auth.ex"))))
         (live (make-magpi-action
                :id "live-1" :title "Perch live" :launch launch
                :observation (make-magpi-observation
                              :activity-state 'running
                              :connection-state 'connected)))
         (cold (make-magpi-action :id "cold-1" :title "Cold chat" :launch launch))
         (intention (make-magpi-intention
                     :id "intent-1" :objective "Ship auth" :state 'active
                     :worktree-path root :branch "magpi/auth"
                     :writer-lease '(:action-id "nested-1")
                     :bindings '((:kind file :reference "notes.org" :label "notes")))))
    (list intention (list nested live cold))))

(ert-deftest magpi-seam-status-contextual-actions-follow-point ()
  "Promise: Magit lineage at point is spawn/visit/react/bind/changes subject."
  (magpi-test-with-repo (root)
    (cl-letf (((symbol-function 'magpi-intention-git-facts)
               (lambda (_intention) '(:checkout dirty :exists t))))
      (pcase-let* ((`(,intention ,actions) (magpi-seam--contextual-fixtures root))
                   (buffer nil))
        (unwind-protect
            (progn
              (setq buffer
                    (save-window-excursion
                      (magpi-status-open
                       root
                       (lambda () actions)
                       (lambda () (list intention)))
                      (current-buffer)))
              (with-current-buffer buffer
                (let ((nested-line
                       (progn
                         (magpi-seam--goto 'magpi-action "nested-1")
                         (buffer-substring-no-properties (line-beginning-position)
                                                         (line-end-position))))
                      (intention-line
                       (progn
                         (magpi-seam--goto 'magpi-intention "intent-1")
                         (buffer-substring-no-properties (line-beginning-position)
                                                         (line-end-position)))))
                  (should (string-match-p "\\? Nested flight" nested-line))
                  (should (string-match-p (regexp-quote (magpi-status--auspice-motion 'aloft))
                                          nested-line))
                  (should (string-match-p "\\? Ship auth" intention-line))
                  (should (string-match-p "\\bw\\b" intention-line)))
                (cl-labels
                    ((assert-facets (type value intention action ask path)
                       (magpi-seam--goto type value)
                       (should (equal (magpi-facet) (cons type value)))
                       (should (eq (magpi-facet-type) type))
                       (should (equal (magpi-section-intention-id) intention))
                       (should (equal (magpi-section-action-id) action))
                       (should (equal (magpi-section-ask-id) ask))
                       (should (equal (magpi-section-path) path))))
                  (assert-facets 'magpi-intention "intent-1"
                                 "intent-1" nil nil nil)
                  (assert-facets 'magpi-action "nested-1"
                                 "intent-1" "nested-1" nil nil)
                  (assert-facets 'magpi-ask '("nested-1" . "q1")
                                 "intent-1" "nested-1" "q1" nil)
                  (assert-facets 'magpi-ask-path '("nested-1" "q1" "lib/auth.ex")
                                 "intent-1" "nested-1" "q1" "lib/auth.ex")
                  (assert-facets 'magpi-observed-file '("nested-1" . "lib/auth.ex")
                                 "intent-1" "nested-1" nil "lib/auth.ex")
                  (assert-facets 'magpi-action "live-1"
                                 nil "live-1" nil nil)
                  (assert-facets 'magpi-action "cold-1"
                                 nil "cold-1" nil nil)
                  (magpi-seam--goto 'magpi-bindings "intent-1")
                  (should (equal (magpi-facet) '(magpi-bindings . "intent-1")))
                  (should (equal (magpi-section-intention-id) "intent-1"))
                  (should-not (magpi-section-action-id))
                  (should (eq (magpi-section-bind-surface) 'intention))
                  (magpi-seam--goto 'magpi-perch nil)
                  (should (eq (magpi-facet-type) 'magpi-perch))
                  (should-not (magpi-section-intention-id))
                  (should-not (magpi-section-action-id))
                  (should (eq (magpi-section-bind-surface) 'root))
                  (magpi-seam--goto 'magpi-cold nil)
                  (should (eq (magpi-facet-type) 'magpi-cold))
                  (should (eq (magpi-section-bind-surface) 'root))
                  (magpi-seam--goto 'magpi-observed-files "nested-1")
                  (should (equal (magpi-facet) '(magpi-observed-files . "nested-1")))
                  (should (equal (magpi-section-action-id) "nested-1"))
                  (should (equal (magpi-section-intention-id) "intent-1"))
                  (should (eq (magpi-section-bind-surface) 'action))
                  (magpi-seam--goto 'magpi-ask-paths '("nested-1" . "q1"))
                  (should (equal (magpi-facet) '(magpi-ask-paths . ("nested-1" . "q1"))))
                  (should (equal (magpi-section-ask-id) "q1"))
                  (should (equal (magpi-section-action-id) "nested-1"))
                  (should (eq (magpi-section-bind-surface) 'action)))))
          (when (buffer-live-p buffer)
            (kill-buffer buffer)))))))

(ert-deftest magpi-seam-jump-perch-then-cold ()
  "Promise: j toggles Perch and Cold."
  (magpi-test-with-repo (root)
    (cl-letf (((symbol-function 'magpi-intention-git-facts)
               (lambda (_intention) '(:checkout dirty :exists t))))
      (pcase-let* ((`(,intention ,actions) (magpi-seam--contextual-fixtures root))
                   (buffer nil))
        (unwind-protect
            (progn
              (setq buffer
                    (save-window-excursion
                      (magpi-status-open
                       root
                       (lambda () actions)
                       (lambda () (list intention)))
                      (current-buffer)))
              (with-current-buffer buffer
                (goto-char (point-min))
                (should (eq (magpi-facet-type) 'magpi-status))
                (magpi-status-jump)
                (should (eq (magpi-facet-type) 'magpi-cold))
                (should (string-match-p
                         (regexp-quote
                          (substring-no-properties
                           (magpi-status--group-heading 'cold)))
                         (buffer-substring-no-properties
                          (line-beginning-position) (line-end-position))))
                (magpi-status-jump)
                (should (eq (magpi-facet-type) 'magpi-perch))
                (magpi-status-jump)
                (should (eq (magpi-facet-type) 'magpi-cold))))
          (when (buffer-live-p buffer)
            (kill-buffer buffer)))))))

(provide 'magpi-seam-tests)
;;; magpi-seam-tests.el ends here
