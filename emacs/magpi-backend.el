;;; magpi-backend.el --- Stable runtime boundary for Magpi -*- lexical-binding: t; -*-

;; The frontend consumes this protocol, not a particular Pi client.  The
;; Pimacs implementation is intentionally isolated in magpi-pimacs-backend.

(require 'cl-lib)

(defconst magpi-backend-event-types
  '(activity-started activity-ended file-observed response-observed
    title-observed model-observed usage-observed disconnected
    problem-observed)
  "Documented semantic event vocabulary for backend listeners.

Events are plists. `activity-ended' may include `:idle t' when the backend
observes current idleness; it does not mean that an attempt is complete.
`usage-observed' carries a backend-neutral usage plist (`:input', `:output',
`:cache-read', `:cache-write', `:total', `:cost', `:context-tokens',
`:context-window', `:context-percent').")

(cl-defgeneric magpi-backend-spawn (backend attempt listener)
  "Launch ATTEMPT, attach LISTENER, and return an opaque handle.

The adapter must install LISTENER before returning, but must not send initial
input.  It must not mutate ATTEMPT directly.")

(cl-defgeneric magpi-backend-send-initial (backend handle attempt)
  "Send ATTEMPT's initial input through HANDLE at most once.

This narrow operation runs only after `magpi-backend-spawn' has installed the
listener.  Adapters compile their transport-specific session name and flags at
launch, and compile the initial prompt here, from ATTEMPT's semantic launch
specification.  A second call must not re-deliver the same instruction: retry
is a new attempt, never a restated prompt.")

(cl-defgeneric magpi-backend-visit (backend handle)
  "Visit HANDLE's session or transcript.")

(cl-defgeneric magpi-backend-send (backend handle message &optional mode)
  "Send MESSAGE to HANDLE, optionally using delivery MODE.")

(cl-defgeneric magpi-backend-terminate (backend handle)
  "Terminate HANDLE when the backend supports that operation.")

(cl-defgeneric magpi-backend-reconcile (backend handle listener)
  "Re-emit current observations for HANDLE through LISTENER.

Used by an explicit status snapshot so facts that only exist as transport
state (for example the running model from `get_state') stay visible without
waiting for a later change event.  Event-driven paints must not call this:
restated snapshots are not new facts, and feeding them back into the mailbox
re-enters refresh.  Default is a no-op for backends without snapshot state.
Emitted events must be safe to reduce twice.")

(cl-defmethod magpi-backend-reconcile (_backend _handle _listener)
  nil)

(provide 'magpi-backend)
;;; magpi-backend.el ends here
