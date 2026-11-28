# One Git write queue per repository

Status: implemented. `xl-async` gained explicit queue keys and a per-job
owner; `xl-git-status` owns the key of a repository; `xl-git-commands`
queues `Gwrite` and `Gcommit` on it.

## Problem

`xl-git-status` serializes its own write operations (staging, unstaging,
discarding, push, fetch, pull) in one queue. The queue key was
`(CONS (current-buffer) 'queue)`, so it belonged to the status buffer.
`Gwrite` and `Gcommit` ran `git` directly with `xl-async-run`, outside any
queue and unowned.

Two consequences:

- `git pull --ff-only` writes the index while it checks out, and `git add`
  from `Gwrite` writes it too. Run at the same time, one of them fails with
  `Unable to create '.git/index.lock': File exists`.
- `Gwrite` followed by `Gcommit` had no order. The commit could start
  before the add finished and commit the previous index.

The lock is per repository (per worktree index), not per buffer, so a
queue owned by a buffer cannot express the rule. Merging
`xl-git-commands.el` and `xl-git-status.el` into one file would not change
it either: `(current-buffer)` is still the file buffer in one case and the
status buffer in the other, so two queues would remain.

## Design

### The queue key

One key per repository:

```elisp
(cons (file-truename (file-name-as-directory (expand-file-name root)))
      'git-writes)
```

`file-truename` resolves symlinks, so the same repository reached through
two paths (macOS `/var` and `/private/var`, or a symlinked home) gets one
key. Forcing a trailing slash makes a caller's `root` and a status
buffer's `default-directory` agree. A linked worktree has its own index,
so its own key: two worktrees of one repository still write concurrently,
which is correct.

`xl-git-status--queue-key` is the only place that builds the key. Every
queue operation must go through it; building a key by hand would silently
create a second queue.

### Explicit keys in `xl-async`

The queue primitives (`xl-async-queue-push`, `-length`, `-busy-p`,
`-clear`) always accepted any key. Only `xl-async-enqueue` and
`xl-async-clear-queue` mapped `(SLOT, BUFFER)` to `(BUFFER . SLOT)`. They
now treat a cons `SLOT` as a key of its own:

```elisp
(xl-async-enqueue (cons "/repo/" 'git-writes) job)
```

A keyed queue also stops passing `:slot` to `xl-async-run`. A slot is
"replace the older run", which the queue never needs: jobs of one queue
do not overlap. Passing the key as a slot would have made two jobs of the
same repository abort each other through the slot table.

### Ownership

A job now takes an optional `:owner`:

| `:owner` | Meaning |
|---|---|
| absent | the buffer that called `xl-async-enqueue`, as before |
| a buffer | handlers run there; killing it aborts the running attempt |
| `nil` | unowned: survives every buffer kill, and `ON-DONE` always runs |

The two Git callers need different values:

- The status buffer owns its jobs. Killing the status buffer aborts a
  running pull, as before.
- `Gwrite` and `Gcommit` are unowned. Closing the file buffer must not
  kill a commit halfway through its hooks. This is why they could not
  simply be enqueued as they were.

A buffer-keyed queue is dropped when its buffer dies. A repository-keyed
queue is not, so a job carries its owner and the buffer's kill hook removes
the buffer's waiting jobs from every queue **before** aborting its running
tasks. The order is the whole point: aborting a task advances its queue in
the same call stack, so the next job must not be one of the dying
buffer's. `xl-async-enqueue` also keeps a guard for a job that reaches the
front with a dead owner: it is skipped, calls no handler and runs no
command, and the queue moves on.

### Drain and refresh

`ON-DRAIN` is per queue and a new push replaces it, so it cannot capture
the buffer that enqueued. The Git drain therefore takes the root and finds
the status buffer that shows it:

- a visible status buffer reloads once, silently;
- a hidden one only ends its loader, and reloads when it is opened;
- no status buffer means nothing to do.

This removes `xl-git-commands--refresh-status`: `Gwrite` no longer needs
its own refresh, because the shared drain refreshes the same buffer once,
after the last job of the repository, whichever command queued it. The
drain also skips a status buffer that is being killed, so aborting the
buffer's own task cannot start a new refresh in it.

The mode-line loader stays a status-buffer feature. A queued `Gwrite` from
a file buffer shows no loader; it reports its result with `message` when
it is done, as before.

## Behaviour

| Situation | Before | After |
|---|---|---|
| `Gwrite` during a pull | may fail on `index.lock` | waits, then stages |
| pull or push during `Gwrite` | may fail | waits |
| `Gwrite` then `Gcommit` | no order | FIFO: add, then commit |
| file buffer killed after `Gwrite` | commit or stage continues | continues (unowned) |
| status buffer killed with queued jobs | queue dropped | jobs skipped, nothing runs |
| status visible when `Gwrite` ends | one extra refresh | one refresh, from the drain |
| two worktrees of one repository | separate indexes | separate queues |

## What does not join the queue

- Reads: `status`, `diff`, `log`, `blame`, `show`. They take no write
  lock (`xl-git-status--git-command` adds `--no-optional-locks`), and
  queueing them would hold up a slow `pull` or block a refresh after it.
- The `git config` lookups of the push menu (`xl-git-status--git-string`).
  They are small synchronous reads, not writes.
- Discarding an untracked file. It is a file deletion, not Git, followed
  by a synchronous status refresh; it is outside this queue.

An external `git` in a terminal is outside Emacs's control and can still
take `index.lock`.

## Tests

In `tests/xl-async-tests.el`:

- one explicit key serializes jobs enqueued from two different buffers;
- a job whose owner was killed is skipped, and a later unowned job of the
  same queue still runs;
- `xl-async-clear-queue` accepts a key: waiting jobs are dropped and the
  running process becomes `cancelled`.

In `tests/smoke-tests.el`:

- `Gwrite` and `Gcommit` pass `:root`, `:attempts` and `:action` to the
  queue instead of starting a process themselves;
- in a real repository, `Gwrite` then `Gcommit` produces the commit
  `Update <file>` and leaves that file out of `git status`;
- the queue key is equal for the same repository written with or without a
  trailing slash and different for another repository.

## Open items

- A job that never ends (a Git process waiting on credentials) keeps its
  repository queue busy forever. A timeout or a "clear this repository's
  queue" command would bound it; neither is implemented.
- The status buffer's name is `*git-status: <basename>*`, so two
  repositories with the same directory name collide. The drain finds the
  buffer by `default-directory`, so it does not refresh the wrong one, but
  the collision itself is older than this queue.
- A queued `Gwrite` does not update the mode line of a status buffer that
  is already showing another operation's loader, so its waiting-count can
  be one behind until the next job starts.
