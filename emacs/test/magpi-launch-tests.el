;;; magpi-launch-tests.el --- Tests for frozen launch specifications -*- lexical-binding: t; -*-

(require 'ert)
(require 'magpi-launch)

(ert-deftest magpi-launch-resolves-profile-before-backend-use ()
  (let ((spec (magpi-launch-build
               "attempt-1" "/tmp/project/" "  Repair token refresh.  "
               "Quick" "Read-only" '(:kind none))))
    (should (equal (magpi-launch-spec-intent spec) "Repair token refresh."))
    (should (equal (magpi-launch-spec-name spec) "Repair token refresh. · atte"))
    (should (eq (magpi-launch-spec-thinking spec) 'low))
    (should (equal (magpi-launch-spec-flags spec)
                   '("--thinking" "low" "--tools" "read,grep,find,ls")))
    (should (equal (magpi-launch-spec-first-message spec)
                   "Repair token refresh.\n\nAuthority: Read-only."))))

(ert-deftest magpi-launch-freezes-context-into-first-message ()
  (let ((spec (magpi-launch-build
               "attempt-1" "/tmp/project/" "Inspect this" "Inherit" "Writer"
               '(:kind point :file "lib/auth.ex" :line 12 :text "refresh()"))))
    (should (equal (magpi-launch-spec-first-message spec)
                   (concat "Inspect this\n\nContext captured at dispatch:\n"
                           "- lib/auth.ex:12\n\nrefresh()\n\nAuthority: Writer.")))))

(ert-deftest magpi-launch-keeps-an-absent-intention-explicit ()
  (let ((spec (magpi-launch-build "attempt-1" "/tmp/project/" "   "
                                  "Inherit" "Writer" '(:kind none))))
    (should (equal (magpi-launch-spec-intent spec) ""))
    (should (equal (magpi-launch-spec-name spec) "◯ · atte"))
    (should-not (magpi-launch-spec-first-message spec))))

(provide 'magpi-launch-tests)
;;; magpi-launch-tests.el ends here
