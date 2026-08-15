# Magpi

Magpi is an intention-first Emacs porcelain for Pi sessions. It is being
organized as a package now so the eventual Elixir/OTP backend can be added
without mixing runtime, frontend, tests, or design notes.

## Frontend composition

Magpi does not reimplement a status UI. `magpi-status.el` composes Magit's
transactional `magit-mode` refresh lifecycle with `magit-section`'s typed,
foldable sections. `magpi.el` owns attempt orchestration; the sole
Pimacs-private adapter is `magpi-pimacs-backend.el`. The status adapter receives
read-model, visit, and spawn callbacks.

Dispatch is a single frozen `magpi-launch-spec`: profile resolution, authority,
context, flags, and first message are settled before backend launch. Intention
may be explicitly absent; Magpi renders a quiet `◯` until the backend supplies a
session title. Model selection remains Pimacs's provider-aware `m` command.

## Layout

```text
magpi/
├── emacs/       # installable Emacs Lisp frontend, model, and tests
├── docs/        # canonical design and research notes
├── Makefile     # reproducible local checks
└── README.md
```

The planned OTP application will live beside `emacs/` (`lib/`, `test/`, and
`mix.exs`). The Emacs layer must talk to it through a compatibility boundary;
frontend code should not depend on future internal OTP modules.

## Development

Run the current Emacs checks from this directory:

```sh
make test
```

The Doom configuration loads the frontend from `magpi/emacs/` and declares the
folder as a local package in `packages.el`. This keeps the package boundary
explicit even before the backend exists.
