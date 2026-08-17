;;; magpi-launch-tests.el --- Tests for frozen launch specifications -*- lexical-binding: t; -*-

(require 'ert)
(require 'magpi-launch)

(ert-deftest magpi-launch-resolves-profile-without-transport-artifacts ()
  (let ((spec (magpi-launch-build
               "/tmp/project/" "Quick" 'read-only '(:kind none))))
    (should (equal (magpi-launch-spec-root spec) "/tmp/project/"))
    (should (equal (magpi-launch-spec-profile spec) "Quick"))
    (should-not (magpi-launch-spec-requested-model spec))
    (should (eq (magpi-launch-spec-thinking spec) 'low))
    (should (eq (magpi-launch-spec-authority spec) 'read-only))))

(ert-deftest magpi-launch-spec-excludes-transport-artifacts ()
  (let ((slots (mapcar #'car (cl-struct-slot-info 'magpi-launch-spec))))
    (dolist (artifact '(flags first-message name))
      (should-not (memq artifact slots)))))

(ert-deftest magpi-launch-preserves-semantic-context ()
  (let ((context '(:kind point :file "lib/auth.ex" :line 12 :text "refresh()")))
    (should (eq (magpi-launch-spec-context
                 (magpi-launch-build "/tmp/" "Default" 'writer context))
                context))))

(ert-deftest magpi-launch-rejects-unknown-semantic-values ()
  (should-error (magpi-launch-build "/tmp/" "Default" 'unknown '(:kind none)))
  (should-error (magpi-launch-build "/tmp/" "Default" 'writer '(:kind unknown)))
  (should-error (magpi-launch-authority-from-label "write everything"))
  (should-error (magpi-launch-context-kind-from-label "nearby"))
  (should-error (magpi-launch-effort-from-label "ludicrous"))
  (should-error (magpi-launch-build "/tmp/" "Default" 'writer '(:kind none)
                                    "not-a-canonical-model")))

(ert-deftest magpi-launch-normalizes-blank-authored-input-once ()
  (should-not (magpi-normalize-intent "   "))
  (should-not (magpi-normalize-intent nil))
  (should (equal (magpi-normalize-intent "  Repair token refresh.  ")
                 "Repair token refresh."))
  (should-not (magpi-normalize-model "Inherit"))
  (should-not (magpi-normalize-model "  "))
  (should (equal (magpi-normalize-model "  openai/gpt-4.1  ")
                 "openai/gpt-4.1"))
  (let ((spec (magpi-launch-build "/tmp/" "Default" 'writer '(:kind none))))
    (should-not (magpi-launch-spec-requested-model spec))))

(ert-deftest magpi-launch-freezes-requested-model-and-effort-override ()
  (let ((spec (magpi-launch-build
               "/tmp/" "Quick" 'writer '(:kind none)
               "anthropic/claude-sonnet" 'high)))
    (should (equal (magpi-launch-spec-requested-model spec)
                   "anthropic/claude-sonnet"))
    ;; Explicit effort wins over the Quick profile's low thinking.
    (should (eq (magpi-launch-spec-thinking spec) 'high)))
  (let ((spec (magpi-launch-build
               "/tmp/" "Deep" 'read-only '(:kind none)
               "Inherit" 'none)))
    (should-not (magpi-launch-spec-requested-model spec))
    (should-not (magpi-launch-spec-thinking spec)))
  (let ((spec (magpi-launch-build
               "/tmp/" "Deep" 'read-only '(:kind none)
               nil 'profile)))
    (should (eq (magpi-launch-spec-thinking spec) 'high))))

(ert-deftest magpi-launch-effort-labels-round-trip ()
  (should (eq (magpi-launch-effort-from-label "Profile") 'profile))
  (should (eq (magpi-launch-effort-from-label "Default") 'none))
  (should (eq (magpi-launch-effort-from-label "High") 'high))
  (should (equal (magpi-launch-thinking-label 'high) "high"))
  (should (equal (magpi-launch-thinking-label nil) "default")))

(ert-deftest magpi-launch-default-context-kind-skips-porcelain ()
  (should (equal (magpi-launch-context-kind-label 'none) "None"))
  (with-temp-buffer
    (should (eq (magpi-launch-default-context-kind) 'none)))
  (with-temp-buffer
    (setq buffer-file-name "/tmp/lib/auth.ex")
    (should (eq (magpi-launch-default-context-kind) 'point))))

(provide 'magpi-launch-tests)
;;; magpi-launch-tests.el ends here
