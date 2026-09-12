;;; magpi-backend.el --- Stable adapter contract for Magpi -*- lexical-binding: t; -*-

;; Copyright (C) 2026 ks0m1c_dharma
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is part of Magpi.

;; One job: the action adapter contract.  Catalog fill is launch UX and lives
;; on a registered filler, not this protocol.
;;
;; Transport nouns (Pi "approval", etc.) are translated here once into Magpi
;; vocabulary (ask).  Domain code never speaks Approval.

(require 'cl-lib)

(cl-defgeneric magpi-backend-spawn (backend action listener)
  "Launch ACTION, attach LISTENER, and return an opaque handle.

The adapter must install LISTENER before returning, but must not send initial
input.  It must not mutate ACTION directly.")

(cl-defgeneric magpi-backend-send-initial (backend handle action)
  "Send ACTION's initial input through HANDLE at most once.

This narrow operation runs only after `magpi-backend-spawn' has installed the
listener.  Adapters compile their transport-specific session name and flags at
launch, and compile the initial prompt here, from ACTION's prompt and
launch-local source.  Intention `@' bindings are not that message.  A second
call must not re-deliver the same instruction: retry is a new action, never a restated prompt.")

(cl-defgeneric magpi-backend-visit (backend handle)
  "Visit HANDLE's session or transcript.")

(cl-defgeneric magpi-backend-visit-root (backend root)
  "Visit the active chat for project ROOT, when the adapter can locate it.")

(cl-defmethod magpi-backend-visit-root ((_backend t) root)
  (user-error "Adapter cannot visit project root: %s" root))

(cl-defgeneric magpi-backend-chat-candidates (backend root)
  "Return adapter-visible past chat references relevant to ROOT.

Each candidate is a plist with stable `:reference', display `:label', and
optional `:last' assistant peek.  The reference is metadata only; binding it
never loads or copies a transcript.")

(cl-defmethod magpi-backend-chat-candidates ((_backend t) _root) nil)

(cl-defgeneric magpi-backend-session-ref (backend handle)
  "Return HANDLE's opaque session pointer, or nil if it is not yet known.

Magpi stores the string and does not interpret it.  Nil means uncertain birth,
not a deferred first open.")

(cl-defmethod magpi-backend-session-ref ((_backend t) _handle) nil)

(cl-defgeneric magpi-backend-live-p (backend handle)
  "Return non-nil when HANDLE's agent process is actually controllable.

A chat buffer is UI, not liveness.")

(cl-defmethod magpi-backend-live-p ((_backend t) _handle) nil)

(cl-defgeneric magpi-backend-history-pending (backend handle)
  "Return a glance label while HANDLE is filling session history, or nil.

`get_entries' and lazy transcript paint are porcelain, not a model turn.
Nil means nothing is filling; a string occupies the quiet history seat only.")

(cl-defmethod magpi-backend-history-pending ((_backend t) _handle) nil)

(cl-defgeneric magpi-backend-send (backend handle message &optional mode)
  "Send MESSAGE to HANDLE, optionally using delivery MODE.")

(cl-defgeneric magpi-backend-ask-supported-p (backend)
  "Return non-nil when BACKEND can submit structured Pi-ask answers.")

(cl-defmethod magpi-backend-ask-supported-p ((_backend t)) nil)

(cl-defgeneric magpi-backend-respond-ask (backend handle ask-id response)
  "Submit structured RESPONSE for ASK-ID through HANDLE.

RESPONSE is an adapter-supported decision such as `approved' or `rejected'.
Adapters must reject unsupported responses rather than translating chat prose.
Transport may still call this Pi approval; Magpi only speaks ask.")

(cl-defmethod magpi-backend-respond-ask ((_backend t) _handle ask-id _response)
  (user-error "Adapter cannot answer Pi-ask: %s" ask-id))

(cl-defgeneric magpi-backend-terminate (backend handle)
  "Terminate HANDLE when the adapter supports that operation.")

(cl-defmethod magpi-backend-terminate ((_backend t) _handle)
  nil)

(cl-defgeneric magpi-backend-reconcile (backend handle listener)
  "Re-emit current observations for HANDLE through LISTENER.

Used by an explicit status snapshot so facts that only exist as transport
state (for example the running model from `get_state', tokens from
`get_session_stats') stay visible without
waiting for a later change event.  Event-driven paints must not call this:
restated snapshots are not new facts, and feeding them back into the mailbox
re-enters refresh.  Default is a no-op for adapters without snapshot state.
Emitted events must be safe to reduce twice.")

(cl-defmethod magpi-backend-reconcile (_backend _handle _listener)
  nil)

(provide 'magpi-backend)
;;; magpi-backend.el ends here
