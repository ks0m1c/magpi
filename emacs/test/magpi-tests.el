;;; magpi-tests.el --- Tests for Magpi orchestration -*- lexical-binding: t; -*-

(require 'ert)
(require 'magpi)

(cl-defstruct magpi-test-backend listener initial-sent attempt fail-initial fail-spawn)

(cl-defmethod magpi-backend-spawn ((backend magpi-test-backend) attempt listener)
  (setf (magpi-test-backend-attempt backend) attempt
        (magpi-test-backend-listener backend) listener)
  ;; A synchronous observation must reduce the registry's stored value, not a
  ;; listener-captured attempt object.
  (funcall listener '(:type activity-started :activity "thinking"))
  (if (magpi-test-backend-fail-spawn backend)
      (progn
        ;; A process can emit facts before its launch RPC reports failure.
        (funcall listener '(:type response-observed :text "Process started"))
        (error "spawn outcome uncertain"))
    'test-handle))

(cl-defmethod magpi-backend-send-initial ((backend magpi-test-backend) _handle _attempt)
  (if (magpi-test-backend-fail-initial backend)
      (error "initial delivery failed")
    (setf (magpi-test-backend-initial-sent backend)
          (not (null (magpi-test-backend-listener backend))))))

(ert-deftest magpi-event-listener-captures-only-id-and-replaces-registry-value ()
  (let* ((backend (make-magpi-test-backend))
         (magpi-backend backend)
         (magpi--attempts (make-hash-table :test #'equal))
         (magpi--handles (make-hash-table :test #'equal))
         (magpi--refresh-timer nil)
         (launch (magpi-launch-build default-directory "Default" 'writer '(:kind none)))
         (attempt (magpi-spawn-spec "attempt-atomic" "Inspect this" launch)))
    (unwind-protect
        (let ((stored (gethash "attempt-atomic" magpi--attempts)))
          (should (eq attempt stored))
          (should-not (eq (magpi-test-backend-attempt backend) stored))
          (should (eq (magpi-observation-activity-state
                         (magpi-attempt-observation stored))
                      'running))
          (should (equal (magpi-observation-activity
                          (magpi-attempt-observation stored))
                         "thinking"))
          (should (magpi-test-backend-initial-sent backend)))
      (when (timerp magpi--refresh-timer)
        (cancel-timer magpi--refresh-timer)))))

(ert-deftest magpi-spawn-failure-preserves-attributable-attempt-and-handle ()
  (let* ((backend (make-magpi-test-backend :fail-initial t))
         (magpi-backend backend)
         (magpi--attempts (make-hash-table :test #'equal))
         (magpi--handles (make-hash-table :test #'equal))
         (magpi--refresh-timer nil)
         (attempt (magpi-spawn-spec
                   "attempt-failed" "Inspect this"
                   (magpi-launch-build default-directory "Default" 'writer
                                       '(:kind none)))))
    (unwind-protect
        (let ((observation (magpi-attempt-observation attempt)))
          (should (eq (gethash "attempt-failed" magpi--handles) 'test-handle))
          (should (equal (magpi-observation-problem observation)
                         "initial delivery failed"))
          (should (eq (magpi-observation-connection-state observation)
                      'disconnected)))
      (when (timerp magpi--refresh-timer)
        (cancel-timer magpi--refresh-timer)))))

(ert-deftest magpi-partial-spawn-failure-preserves-attributable-attempt ()
  (let* ((backend (make-magpi-test-backend :fail-spawn t))
         (magpi-backend backend)
         (magpi--attempts (make-hash-table :test #'equal))
         (magpi--handles (make-hash-table :test #'equal))
         (magpi--refresh-timer nil)
         (attempt (magpi-spawn-spec
                   "attempt-uncertain" "Inspect this"
                   (magpi-launch-build default-directory "Default" 'writer
                                       '(:kind none)))))
    (unwind-protect
        (let ((observation (magpi-attempt-observation attempt)))
          (should (eq attempt (gethash "attempt-uncertain" magpi--attempts)))
          (should-not (gethash "attempt-uncertain" magpi--handles))
          (should (equal (magpi-attempt-intent attempt) "Inspect this"))
          (should (equal (magpi-observation-problem observation)
                         "spawn outcome uncertain"))
          (should (equal (magpi-observation-last-response observation)
                         "Process started"))
          (should (eq (magpi-observation-connection-state observation)
                      'disconnected)))
      (when (timerp magpi--refresh-timer)
        (cancel-timer magpi--refresh-timer)))))

(ert-deftest magpi-spawn-from-options-freezes-model-and-effort ()
  (let* ((backend (make-magpi-test-backend))
         (magpi-backend backend)
         (magpi--attempts (make-hash-table :test #'equal))
         (magpi--handles (make-hash-table :test #'equal))
         (magpi--refresh-timer nil)
         attempt)
    (unwind-protect
        (cl-letf (((symbol-function 'magpi--root)
                   (lambda () "/tmp/project/"))
                  ((symbol-function 'magpi--capture-context)
                   (lambda (_kind) '(:kind none)))
                  ((symbol-function 'magpi--new-id)
                   (lambda () "attempt-options")))
          (setq attempt
                (magpi-spawn-from-options
                 '(:intent "Use model"
                   :profile "Quick"
                   :effort high
                   :model "openai/gpt-4.1"
                   :authority writer
                   :context-kind none)))
          (let ((launch (magpi-attempt-launch attempt)))
            (should (equal (magpi-launch-spec-requested-model launch)
                           "openai/gpt-4.1"))
            (should (eq (magpi-launch-spec-thinking launch) 'high))
            (should (equal (magpi-launch-spec-profile launch) "Quick"))))
      (when (timerp magpi--refresh-timer)
        (cancel-timer magpi--refresh-timer)))))

(ert-deftest magpi-restated-events-do-not-reschedule-refresh ()
  (let* ((backend (make-magpi-test-backend))
         (magpi-backend backend)
         (magpi--attempts (make-hash-table :test #'equal))
         (magpi--handles (make-hash-table :test #'equal))
         (magpi--refresh-timer nil)
         (attempt (magpi-spawn-spec
                   "attempt-idempotent" "Inspect this"
                   (magpi-launch-build default-directory "Default" 'writer
                                       '(:kind none)))))
    (unwind-protect
        (progn
          (when (timerp magpi--refresh-timer)
            (cancel-timer magpi--refresh-timer)
            (setq magpi--refresh-timer nil))
          (let ((stored (gethash "attempt-idempotent" magpi--attempts)))
            (magpi--handle-event "attempt-idempotent"
                                 '(:type activity-started :activity "thinking"))
            (should (eq stored (gethash "attempt-idempotent" magpi--attempts)))
            (should-not magpi--refresh-timer)))
      (when (timerp magpi--refresh-timer)
        (cancel-timer magpi--refresh-timer)))))

(ert-deftest magpi-capture-context-rejects-workbench-porcelain ()
  (with-temp-buffer
    (let ((buffer-file-name nil))
      (should-error (magpi--capture-context 'point))))
  (with-temp-buffer
    (setq buffer-file-name "/tmp/lib/auth.ex")
    (insert "refresh()")
    (goto-char (point-min))
    (should (equal (plist-get (magpi--capture-context 'point) :text)
                   "refresh()"))))

(provide 'magpi-tests)
;;; magpi-tests.el ends here
