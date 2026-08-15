;;; magpi-pimacs-backend-tests.el --- Tests for the Lean Cut backend -*- lexical-binding: t; -*-

(require 'ert)

;; Keep the adapter testable without loading the full Pimacs UI or a real Pi
;; process.  The production adapter is exercised unchanged against this small
;; compatibility surface.
(unless (require 'pimacs nil t)
  (defvar pimacs-flags nil)
  (defvar pimacs--agents (make-hash-table :test #'equal))
  (defvar magpi-pimacs-test--chat nil)
  (defvar magpi-pimacs-test--flags nil)
  (defvar magpi-pimacs-test--listener nil)
  (defvar magpi-pimacs-test--sent nil)
  (defvar magpi-pimacs-test--terminated nil)
  (defvar magpi-pimacs-test--title-request nil)
  (defvar-local pimacs--project-key nil)
  (defun pimacs-chat (_name _root)
    (setq magpi-pimacs-test--flags pimacs-flags)
    (setq magpi-pimacs-test--chat (get-buffer-create " *magpi-pimacs-test*"))
    (switch-to-buffer magpi-pimacs-test--chat)
    (setq-local pimacs--project-key "test-project"))
  (defun pimacs--current-agent () nil)
  (defun pimacs--set-event-listener (_name _id listener)
    (setq magpi-pimacs-test--listener listener))
  (defun pimacs--agent-add-cleanup (_agent _cleanup) nil)
  (defun pimacs-send-prompt (message mode)
    (setq magpi-pimacs-test--sent (list message mode (current-buffer))))
  (defun pimacs--kill-agent (&optional cleanup)
    (setq magpi-pimacs-test--terminated cleanup))
  (defun pimacs--send-command (_type _args &optional callback)
    (setq magpi-pimacs-test--title-request callback))
  (defun pimacs--response-success-p (response)
    (plist-get response :success))
  (provide 'pimacs))

(require 'magpi-pimacs-backend)

(defun magpi-pimacs-test-spec (&optional profile)
  (magpi-launch-build "attempt-1" "/tmp/" "Agent"
                      (or profile "Inherit") "Writer" '(:kind none)))

(defmacro magpi-pimacs-test-with-backend (&rest body)
  `(let ((pimacs-flags '("--global"))
         (magpi-pimacs-test--chat nil)
         (magpi-pimacs-test--flags nil)
         (magpi-pimacs-test--listener nil)
         (magpi-pimacs-test--sent nil)
         (magpi-pimacs-test--terminated nil)
         (magpi-pimacs-test--title-request nil))
     ,@body))

(ert-deftest magpi-pimacs-backend-spawn-keeps-launch-details-at-boundary ()
  (magpi-pimacs-test-with-backend
   (let ((handle (magpi-backend-spawn
                  (make-magpi-pimacs-backend)
                  (magpi-pimacs-test-spec "Quick"))))
     (should (equal magpi-pimacs-test--flags
                    '("--global" "--thinking" "low")))
     (should (equal (magpi-pimacs-handle-id handle) "attempt-1"))
     (should (eq (magpi-pimacs-handle-chat-buffer handle)
                 magpi-pimacs-test--chat))
     (should (equal (magpi-pimacs-handle-project-key handle) "test-project")))))

(ert-deftest magpi-pimacs-backend-delivers-events-and-sends-through-handle ()
  (magpi-pimacs-test-with-backend
   (let* ((backend (make-magpi-pimacs-backend))
          (handle (magpi-backend-spawn backend (magpi-pimacs-test-spec)))
          received)
     (magpi-backend-subscribe backend handle (lambda (event) (setq received event)))
     (funcall magpi-pimacs-test--listener '(:type "agent_start"))
     (magpi-backend-send backend handle "Start here" 'followUp)
     (should (equal received '(:type "agent_start")))
     (should (equal magpi-pimacs-test--sent
                    (list "Start here" 'followUp magpi-pimacs-test--chat))))))

(ert-deftest magpi-pimacs-backend-reconciles-a-title-from-state ()
  (magpi-pimacs-test-with-backend
   (let* ((backend (make-magpi-pimacs-backend))
          (handle (magpi-backend-spawn backend (magpi-pimacs-test-spec)))
          received)
     (magpi-backend-subscribe backend handle (lambda (event) (setq received event)))
     (funcall magpi-pimacs-test--title-request
              '(:success t :data (:sessionName "Derived task title")))
     (should (equal received
                    '(:type "backend_title" :title "Derived task title"))))))

(provide 'magpi-pimacs-backend-tests)
;;; magpi-pimacs-backend-tests.el ends here
