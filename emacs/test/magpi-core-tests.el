;;; magpi-core-tests.el --- Tests for lean Magpi core -*- lexical-binding: t; -*-

(require 'ert)
(require 'magpi-core)

(defun magpi-test-attempt ()
  (make-magpi-attempt
   :id "a1" :intent "Make intent visible" :root "/tmp/project/"
   :profile "Standard" :authority "Writer" :status 'starting))

(ert-deftest magpi-attempt-reduces-lifecycle-events ()
  (let ((attempt (magpi-test-attempt)))
    (magpi-attempt-apply-event attempt '(:type "agent_start"))
    (should (eq (magpi-attempt-status attempt) 'running))
    (should (equal (magpi-attempt-activity attempt) "thinking"))
    (magpi-attempt-apply-event attempt '(:type "agent_settled"))
    (should (eq (magpi-attempt-status attempt) 'idle))
    (should-not (magpi-attempt-activity attempt))))

(ert-deftest magpi-attempt-records-only-structured-write-paths ()
  (let ((attempt (magpi-test-attempt)))
    (magpi-attempt-apply-event
     attempt '(:type "tool_execution_start" :toolName "read"
                     :args (:path "lib/read.ex")))
    (should-not (magpi-attempt-observed-files attempt))
    (magpi-attempt-apply-event
     attempt '(:type "tool_execution_start" :toolName "edit"
                     :args (:path "lib/auth.ex")))
    (magpi-attempt-apply-event
     attempt '(:type "tool_execution_start" :toolName "write"
                     :args (:path "lib/auth.ex")))
    (should (equal (magpi-attempt-observed-files attempt)
                   '("lib/auth.ex")))))

(ert-deftest magpi-attempt-keeps-frozen-intention-across-events ()
  (let ((attempt (magpi-test-attempt)))
    (magpi-attempt-apply-event
     attempt '(:type "tool_execution_start" :toolName "edit"
                     :args (:path "lib/a.ex")))
    (magpi-attempt-apply-event
     attempt '(:type "message_end"
                     :message (:role "assistant"
                               :content [(:type "text"
                                          :text "Changed implementation.\nTesting now.")])))
    (should (equal (magpi-attempt-intent attempt) "Make intent visible"))
    (should (equal (magpi-attempt-last-response attempt)
                   "Changed implementation. Testing now."))))

(ert-deftest magpi-attempt-disconnect-is-explicit ()
  (let ((attempt (magpi-test-attempt)))
    (magpi-attempt-mark-disconnected attempt)
    (should (eq (magpi-attempt-status attempt) 'disconnected))
    (should-not (magpi-attempt-activity attempt))))

(ert-deftest magpi-attempt-reduces-backend-disconnection-as-a-fact ()
  (let ((attempt (magpi-test-attempt)))
    (magpi-attempt-apply-event attempt '(:type "backend_disconnected"))
    (should (eq (magpi-attempt-status attempt) 'disconnected))
    (should-not (magpi-attempt-activity attempt))))

(ert-deftest magpi-attempt-adopts-a-backend-title-only-when-intention-is-absent ()
  (let ((empty (magpi-test-attempt))
        (authored (magpi-test-attempt)))
    (setf (magpi-attempt-intent empty) "")
    (magpi-attempt-apply-event empty
                               '(:type "extension_ui_request"
                                 :method "setTitle" :title "Derived task"))
    (magpi-attempt-apply-event authored
                               '(:type "backend_title" :title "Ignored title"))
    (should (equal (magpi-attempt-intent empty) "Derived task"))
    (should (equal (magpi-attempt-intent authored) "Make intent visible"))))

(ert-deftest magpi-content-text-ignores-non-text-blocks ()
  (should
   (equal (magpi--content-text
           [(:type "thinking" :thinking "hidden")
            (:type "text" :text "visible")])
          "visible")))

(provide 'magpi-core-tests)
;;; magpi-core-tests.el ends here
