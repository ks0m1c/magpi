;;; magpi-backend.el --- Stable runtime boundary for Magpi -*- lexical-binding: t; -*-

;; The frontend consumes this protocol, not a particular Pi client.  The
;; Pimacs implementation is intentionally isolated in magpi-pimacs-backend.

(require 'cl-lib)

(cl-defgeneric magpi-backend-spawn (backend spec)
  "Launch SPEC and return an opaque attempt handle.")

(cl-defgeneric magpi-backend-visit (backend handle)
  "Visit HANDLE's session or transcript.")

(cl-defgeneric magpi-backend-snapshot (backend)
  "Return BACKEND's best read-only snapshot.")

(cl-defgeneric magpi-backend-send (backend handle message &optional mode)
  "Send MESSAGE to HANDLE, optionally using delivery MODE.")

(cl-defgeneric magpi-backend-subscribe (backend handle listener)
  "Deliver HANDLE events to LISTENER.

LISTENER receives one event at a time.  A backend must report loss of its
handle as an event; it must not mutate Magpi's attempt record directly.")

(cl-defgeneric magpi-backend-terminate (backend handle)
  "Terminate HANDLE when the backend supports that operation.")

(provide 'magpi-backend)
;;; magpi-backend.el ends here
