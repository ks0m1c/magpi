# Magpi

Magpi is a small intention-first Emacs porcelain for deliberate Pi work. It
keeps the human loop close: state the change, launch a bounded task, inspect
chat and Git evidence, then explicitly merge or discard.

## Frontend composition

Each module has one job:

- `magpi-action.el` — immutable Action / Observation (asks are observation facts) and the pure reducer
- `magpi-launch.el` — frozen launch specification and launch defaults
- `magpi-backend.el` — adapter contract verbs: spawn, send-initial, visit, send, terminate, reconcile
- `magpi-pimacs-backend.el` — the sole Pimacs adapter implementation; registers catalog fill
- `magpi-status.el` — Magit-mode/Section projection
- `magpi-transient.el` — launch choices
- `magpi.el` — registry, effects, intention commands, and the `C-c m` prefix
- `magpi-intention.el` — persisted intention, Git change, lease, and audit operations

Dispatch freezes a `magpi-launch-spec` containing root, requested model,
thinking, role, and context. The launch Transient exposes one thinking
axis (Default/Quick/Standard/Deep/Off), last-used or Inherit model, context,
and role. Model catalog fill is a registered launch helper, not an adapter
protocol operation. Grouped chat windows use the intention objective as their
title; standalone chats use a generated task title. The task is entered as the
first user message in chat; an intention objective is never sent to chat.
An intention is a small durable record for one authored objective and its chosen
references. In Magpi status, `i` creates and selects it, `@` binds context to the surface at point (file/chat/point/region), and `s` spawns an action.
Outside status, use `C-c m i` and `C-c m @`. References are never copied
transcript or file content. The first task launched in the intention creates
its managed Git worktree and branch.
The record then holds that Git identity, state (`active`, `merged`, or
`discarded`), action membership, one conservative writer lease, and a narrow audit.
Task exit never decides the intention, and transport disconnection never releases
uncertain writer ownership.

The objective is control-plane data and is never sent to chat. Magpi opens the
chat without a task prompt; the user authors the task as the first message.
Explicitly captured source context is appended to that message when selected.
Tasks share the intention workspace by reference, while each action keeps its
own chat reference, launch, observation, handle, and runtime facts.

Magpi status is the concise intention dashboard. Each intention header shows
its attachment count, branch (or `not started`), dirty state, ahead/behind
counts, state, active writer, and action count; attachments and tasks nest
below. `s` spawns because Magpi has no stageable rows;
after `RET` enters native Magit, `s` stages normally. On an intention or nested
task, `RET` opens
Magit status, `d` compares base to branch, `l` reviews the range, `c` opens
Magit commit. Glance marks pending reactions; `a` or RET on a Pi-ask opens React (approve/reject, release writer, merge/discard). `m` opens Magit for change depth.
After `s`, the launch menu defaults to `w` (writer); press `w` to toggle between
classic `w` (writer) and `r` (reader) before spawning. Delegated Magit buffers receive
intention, task, audit, and chat-history metadata. Git facts are read live;
missing facts render as missing rather than clean/zero. Pi-asks nest below their action as observation facts (not Magpi beings). Glance `?`/`!` marks pending asks or disconnection. React answers asks and settles intentions; Magit holds depth.
A separate multi-worktree record is intentionally deferred until an
intention truly needs multiple independently managed worktrees.

The domain keeps `Intention` as durable repository metadata, `Action` as one
independent execution, `Launch` as frozen configuration, and `Observation` as
runtime facts. Adapter handles remain in a separate in-memory registry.
Event listeners capture only an attempt ID; `magpi-action-reduce` returns
the same attempt when a fact is restated, otherwise a new value which is
atomically replaced in that registry. Event paints never reconcile; `g` is
the snapshot pull. Default context is None unless an active region is
source-file evidence.

## Boundaries and tests

Keep intention objective and launch configuration distinct and immutable at their
owning boundaries. The task is authored in chat, not collected as a launch field.
Backends normalize transport events into facts; the pure reducer returns a new
action; `magpi.el` alone owns registry replacement and effects; status is
reader. No backend observation may alter objective, launch, or Git identity.

### What to test

Test Magpi's promises, not its call graph.

| Promise | How |
|---|---|
| Labels become semantic launch options | Compare the options plist |
| Launch freezes model/thinking/role/context | Real Transient layout + live args |
| Access is always w\|r | Init/format on the live switch |
| Status opens on a Git root | Real Magit buffer, real repo |
| Create collects an intention string | Observable owner result |
| One intention, one change, one writer | Real Git worktree/branch/lease |
| Missing Git facts stay missing | Real facts API over absent paths |

Do not mock Git semantics. Neutralize the environment (identity, global config,
temp location). On failure, temporary repositories are retained and their path
is printed as evidence.

Transient's layout is itself a declarative contract: asserting the live suffix
model is appropriate. Asserting that `require` or an internal helper was called
is not.

## Layout

```text
magpi/
├── emacs/       # installable Emacs Lisp frontend, model, and tests
├── docs/        # MAGPI.org, why/run/locus freeze, research notes
├── Makefile     # reproducible local checks
└── README.md
```

A durable external runtime is deliberately deferred. It is earned only by an
observed need such as surviving Emacs exit, truthful reconnect, or serialized
multi-client control; the Emacs/adapter boundary keeps that option possible.

## Development

Run checks from this directory:

```sh
make test          # unit + seams
make test-unit     # pure domain/adapter tests (no Magit package tree)
make test-seams    # real Magit + Transient; fails if the build root is missing
```

`make test-seams` loads real Magit/Transient and asserts porcelain contracts
(launch axes, status open/create). Override the package tree with
`MAGPI_STRAIGHT_BUILD=/path/to/straight/build-…` when needed. Git fixtures
neutralize identity/config and retain the repo path on failure.

The Doom configuration loads the frontend from `magpi/emacs/` and declares the
folder as a local package in `packages.el`. This keeps the package boundary
explicit even before a durable external adapter exists.
