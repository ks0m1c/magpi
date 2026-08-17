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

Dispatch freezes a `magpi-launch-spec` containing root, profile, requested
model, thinking/effort, authority, and context. The launch Transient exposes
intention, model (Inherit or `provider/model`), effort, profile, context, and
authority. Effort may defer to the profile or override it; model remains optional
(`nil` means inherit). Status projection uses a small face hierarchy so identity,
activity, authority, and evidence scan at different weights. The Pimacs launch
compiles transport flags and an injective session identity while installing the
attempt listener; `magpi-backend-send-initial` then compiles and sends the
initial prompt at most once. Authored intent is the heading and is immutable
on the attempt; title, running model, and token usage are separate runtime
observations. Model selection is deliberately not a Magpi control surface in this
cut.

The domain uses three plain records: `Attempt` (`id`, immutable `intent`,
`launch`, `observation`, `started-at`), `Launch` (frozen configuration), and
`Observation` (runtime facts, including session token usage). Backend handles live in a separate registry. Event
listeners capture only an attempt ID; `magpi-attempt-reduce` returns the same
attempt when a fact is restated, otherwise a new value which is atomically
replaced in that registry. Event paints never reconcile; `g` is the snapshot
pull. Point/Region context is source-file evidence only.

## Boundaries and tests

Keep authored intent and launch configuration as immutable plain data. Backends
normalize transport events into facts; the pure reducer returns a new attempt;
`magpi.el` alone owns registry replacement and effects; status is read-only.
Tests follow those boundaries: launch normalization, reduction, adapter
normalization and compilation, orchestration, then projection. In particular, no
backend observation may alter intent or launch configuration.

## Layout

```text
magpi/
├── emacs/       # installable Emacs Lisp frontend, model, and tests
├── docs/        # MAGPI.org, why/run/locus freeze, research notes
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
