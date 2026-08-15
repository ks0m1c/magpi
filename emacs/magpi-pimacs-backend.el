;;; magpi-pimacs-backend.el --- Pimacs adapter for Magpi -*- lexical-binding: t; -*-

;; This is the only Magpi module allowed to use Pimacs private APIs.  It is an
;; ephemeral Lean Cut adapter; its public surface is magpi-backend.el.

(require 'cl-lib)
(require 'pimacs)
(require 'magpi-backend)
(require 'magpi-launch)

(cl-defstruct magpi-pimacs-backend)

(cl-defstruct magpi-pimacs-handle
  id chat-buffer project-key agent cleanup initial-title)

(defvar magpi-backend (make-magpi-pimacs-backend)
  "The Lean Cut backend.

Later backends replace this value while retaining the `magpi-backend-*'
contract.")

(cl-defmethod magpi-backend-spawn ((_backend magpi-pimacs-backend) spec)
  (let ((pimacs-flags (append pimacs-flags (magpi-launch-spec-flags spec))))
    (pimacs-chat (magpi-launch-spec-name spec) (magpi-launch-spec-root spec)))
  (let ((chat (current-buffer)))
    (make-magpi-pimacs-handle
     :id (magpi-launch-spec-id spec)
     :chat-buffer chat
     :project-key (buffer-local-value 'pimacs--project-key chat)
     :agent (with-current-buffer chat (pimacs--current-agent))
     :initial-title (magpi-launch-spec-name spec))))

(cl-defmethod magpi-backend-visit ((_backend magpi-pimacs-backend) handle)
  (let ((chat (magpi-pimacs-handle-chat-buffer handle)))
    (if (buffer-live-p chat)
        (pop-to-buffer chat)
      (user-error "Attempt %s has no live chat buffer"
                  (magpi-pimacs-handle-id handle)))))

(cl-defmethod magpi-backend-snapshot ((_backend magpi-pimacs-backend))
  (let (snapshot)
    (maphash
     (lambda (key agent)
       (push (list :project-key key :live (process-live-p agent)) snapshot))
     pimacs--agents)
    (nreverse snapshot)))

(cl-defmethod magpi-backend-send ((_backend magpi-pimacs-backend) handle message
                                  &optional mode)
  (let ((chat (magpi-pimacs-handle-chat-buffer handle)))
    (unless (buffer-live-p chat)
      (user-error "Attempt %s has no live chat buffer"
                  (magpi-pimacs-handle-id handle)))
    (with-current-buffer chat
      (pimacs-send-prompt message mode))))

(defun magpi-pimacs--request-title (handle listener)
  "Reconcile a Pimacs session title into the generic event stream."
  (pimacs--send-command
   "get_state" '()
   (lambda (response)
     (when (pimacs--response-success-p response)
       (when-let ((title (plist-get (plist-get response :data) :sessionName)))
         (unless (equal title (magpi-pimacs-handle-initial-title handle))
           (funcall listener (list :type "backend_title" :title title))))))))

(cl-defmethod magpi-backend-subscribe ((_backend magpi-pimacs-backend) handle listener)
  (let ((chat (magpi-pimacs-handle-chat-buffer handle))
        (id (magpi-pimacs-handle-id handle)))
    (unless (buffer-live-p chat)
      (user-error "Attempt %s has no live chat buffer" id))
    (with-current-buffer chat
      (pimacs--set-event-listener t id listener)
      (magpi-pimacs--request-title handle listener)
      (when-let ((agent (pimacs--current-agent)))
        (let ((cleanup (lambda ()
                         (funcall listener '(:type "backend_disconnected")))))
          (setf (magpi-pimacs-handle-cleanup handle) cleanup)
          (pimacs--agent-add-cleanup agent cleanup))))))

(cl-defmethod magpi-backend-terminate ((_backend magpi-pimacs-backend) handle)
  (let ((chat (magpi-pimacs-handle-chat-buffer handle)))
    (when (buffer-live-p chat)
      (with-current-buffer chat
        (pimacs--kill-agent (magpi-pimacs-handle-cleanup handle))))))

(provide 'magpi-pimacs-backend)
;;; magpi-pimacs-backend.el ends here
