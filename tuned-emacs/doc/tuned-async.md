# tuned-async: composable asynchronous primitives

Status: implemented (commit `aae1e6c` and later). The original `tuned-async-run` interface is kept as a compatibility layer built on the new primitives, so existing callers did not change. Remaining deviations from the plan are listed under "Status of the migration".

## Files

| File | Contents |
|---|---|
| `lisp/tuned-async.el` | Layers 0 and 1, and the compatibility layer (`tuned-async-run`, `tuned-async-enqueue`, `tuned-async-cancel`, ...) |
| `lisp/tuned-spinner.el` | The loader, separate from the process layer. Callers that draw a loader require both files. |
| `lisp/tuned-async-evaluate.el` | Layer 2: evaluate, worker, and the child Emacs command line |
| `lisp/tuned-native-compile.el` | Native-compile queue, monitor, and `tuned-async-native-compile` |
| `tests/tuned-async-tests.el` | Tests for layers 0 and 1 |
| `tests/tuned-async-compat-tests.el` | Tests for the compatibility layer |
| `tests/tuned-async-evaluate-tests.el` | Tests for layer 2 |

All public names use the `tuned-async-` prefix. The `async-` prefix belongs to the emacs-async package (used by helm and dired-async), which also defines `async-start`. Private functions of layer 2 use `tuned-async-evaluate--` and `tuned-async-worker--`, so they do not collide with `tuned-async--`.

## Background

Before the redesign, `tuned-async.el` had two layers: the process layer (`tuned-async-run`, `tuned-async-enqueue`) and the loader (`tuned-spinner-*`). The process layer put starting a process, reading its output, ownership, slot replacement, timeouts, streaming output and queueing into one function with one large property list:

```elisp
(tuned-async-run :command … :directory … :owner … :slot … :stdin …
              :name … :timeout … :on-output … :on-done …)
```

That worked, but the mechanisms were tied together. "Time out without an owner", "own without a slot", or "stream output and also parse it on exit" could only be written by combining arguments of this one function.

A separate `tuned-async-evaluate` (which evaluates one form in a clean `emacs -Q --batch`) also called `make-process` itself, which duplicated the process layer.

Keyword use across the callers of `tuned-async-run`: `:command` 17, `:on-done` 15, `:name` 14, `:slot` 13, `:directory` 10, `:timeout` 7, `:owner` 5, `:stdin` 4, `:on-output` 4.

## Principles

1. **An asynchronous task is one process object.** No separate job structure: status, output and handlers are stored as process properties (`process-put`).
2. **Each primitive does one thing and has the same shape: it takes a process and returns the process.** Composition uses `thread-first`, not one large property list.
3. **Only one place calls `make-process`.** Only that place installs the filter and sentinel. Other primitives register handlers on the process, so they cannot overwrite each other.
4. **Hard guarantees are written once.** "Output is complete when the exit handlers run", "callbacks run exactly once", "a cancelled task does not call back", and "a lost sentinel is picked up by the reaper" are implemented only in the core, so no composition can bypass them.
5. **Composition is order-independent.** A handler registered after the process has already exited still runs, so `(tuned-async-on-exit p f)` is correct wherever it appears.

## Non-goals

- No threads and no future or promise library. A callback is a callback.
- No remote processes (ssh or tramp). A command may be `ssh …`, but the layer does not manage the connection.
- Not a replacement for `compile` or `comint`. Interactive shells are out of scope.

## Structure

```
Layer 2  applications   tuned-async-evaluate   tuned-async-worker   native compile (tuned-native-compile)
                        compatibility: tuned-async-run  tuned-async-enqueue  tuned-async-cancel ...
Layer 1  behaviours     tuned-async-timeout  tuned-async-own  tuned-async-slot  tuned-async-queue-push
Layer 0  core           tuned-async-start  tuned-async-on-output  tuned-async-on-exit  tuned-async-finally
                        tuned-async-abort  tuned-async-send  tuned-async-close-input  query functions
```

Each layer uses only the layer below it. The four behaviours in layer 1 are independent of each other.

## Layer 0: the core

### Task properties

| Property | Meaning |
|---|---|
| `tuned-async-status` | One of `running`, `done`, `timeout`, `cancelled` |
| `tuned-async-output` | All output, copied from the output buffer when the task ends (a string) |
| `tuned-async-on-output` | Handlers, in registration order |
| `tuned-async-on-exit` | Handlers, in registration order |
| `tuned-async-finally` | Handlers, in registration order |
| `tuned-async-owner` | The owning buffer, or nil (set by layer 1's `own`) |
| `tuned-async-finished` | The end-of-task procedure has run (guarantees it runs once) |
| `tuned-async-keep-buffer` | Whether the output buffer survives the end of the task |

### `(tuned-async-start COMMAND &rest ARGS)` → process

Starts the process, sends its output to a buffer, and returns the process. Keywords:

| Keyword | Meaning | Default |
|---|---|---|
| `:directory` | Working directory | `default-directory` |
| `:stdin` | Text to write to standard input | none |
| `:keep-input` | When non-nil, standard input stays open (the worker needs this) | nil: standard input is closed after `:stdin` is written, so commands that read stdin, such as `git apply`, can finish |
| `:name` | Process name, also used in perfetto and error messages | `"tuned-async"` |
| `:buffer-name` | Output buffer name; may contain `%d`, replaced by the pid. When given, the buffer is **kept** after the task ends. | Hidden buffer `" *tuned-async*"`, deleted when the task ends |
| `:coding` | Process coding system | `utf-8-unix` |

It calls `make-process` (with `:connection-type 'pipe` and `:noquery t`), installs the one filter and sentinel, sets the status to `running`, and starts the reaper (checks every 0.25 s until the task ends).

### `(tuned-async-on-output PROC FN)` → PROC

Calls `(FN PROC CHUNK)` for each chunk of output. The output is appended to the buffer first, so the handler sees a buffer that already contains the chunk. When the task has an owner, the handler runs in the owner buffer.

### `(tuned-async-on-exit PROC FN)` → PROC

Calls `(FN PROC)` once when the task ends **normally** (`done` or `timeout`). A cancelled task (`cancelled`) does not call it. The handler reads the output and exit code with the query functions.

### `(tuned-async-finally PROC FN)` → PROC

Calls `(FN PROC)` exactly once **however** the task ends. Used for cleanup: cancelling timers, removing entries from the slot table, and advancing the queue.

### `(tuned-async-abort PROC)` → PROC

Cancels: sets the status to `cancelled` and kills the process. `on-exit` does not run; `finally` does. It does nothing to a task that has already ended.

The name is `abort` rather than `cancel`, because the compatibility function `tuned-async-cancel (&optional slot buffer)` already has a different signature.

### `(tuned-async-send PROC STRING)`, `(tuned-async-close-input PROC)` → PROC

Write to standard input and close it. These only matter for `:keep-input` tasks.

### Queries

| Function | Returns |
|---|---|
| `(tuned-async-status PROC)` | The status symbol |
| `(tuned-async-live-p PROC)` | Whether the status is `running` |
| `(tuned-async-output PROC)` | While running: the output so far from the buffer. After the end: the copied string, which remains available after the buffer is deleted. |
| `(tuned-async-exit-code PROC)` | The exit code. `timeout` gives 124, the exit code of coreutils `timeout`, so callers can treat it uniformly. While running: nil. |
| `(tuned-async-buffer PROC)` | The output buffer, or nil |

## The end-of-task procedure

Both the sentinel and the reaper call the same end function. `tuned-async-finished` ensures it runs once. The order is fixed:

1. **Read all output.** The process may have exited while data remains in the pipe, so `accept-process-output` loops until the pipe is empty. The reaper does this too. On Emacs 30.1, a process that exits while another sentinel is running may never get its own sentinel called. Calling back without draining would deliver truncated output.
2. **Set the final status.** Keep `cancelled` or `timeout` if already set; otherwise set `done`.
3. **Save the output** by copying the buffer into the `tuned-async-output` property.
4. **Call the exit handlers** (only for `done` and `timeout`), in registration order. With an owner they run in the owner buffer. If the owner has been killed, the status becomes `cancelled` and the exit handlers are skipped.
5. **Call the finally handlers**, in registration order.
6. **Dispose of the buffer.** A hidden buffer without `:buffer-name` is deleted; a named buffer is kept.

Each handler runs inside `condition-case`. A failing handler writes its error to `*Messages*`, with the task name, and the other handlers still run. When `tuned-async-debug-on-error` is non-nil, errors are re-signalled instead; the tests use this.

**Late registration.** A handler added with `tuned-async-on-exit` or `tuned-async-finally` after the task has ended runs on the next event loop turn (`run-at-time 0`), following the same rules (a cancelled task does not run exit handlers). Composition is therefore order-independent, even when the command exits before `tuned-async-start` returns.

## State machine

```
            exit                       ┌─→ done
running ─────────────────────────────┤
            timeout timer fires         ├─→ timeout   (exit code 124; exit handlers still run)
            abort / owner killed /      │
            slot replaced               └─→ cancelled (exit handlers do not run)
```

Terminal states do not change. `finally` runs once in every terminal state.

## Layer 1: behaviours

All four behaviours are implemented with layer 0 only, and they are independent of each other.

### `(tuned-async-timeout PROC SECONDS)` → PROC

Starts a timer. If the task is still running when it fires, the status becomes `timeout` and the process is killed. A `finally` handler cancels the timer.

### `(tuned-async-own PROC BUFFER)` → PROC

Ownership has three effects:

- It sets `tuned-async-owner`, so the on-output and on-exit handlers run in BUFFER. If BUFFER is killed, the exit handlers do not run.
- It adds PROC to BUFFER's buffer-local list `tuned-async--owned`. The variable is `permanent-local`, so changing the major mode does not lose it.
- It adds a **buffer-local** `kill-buffer-hook` to BUFFER which, when the buffer is killed, first removes that buffer's waiting jobs from every queue and then aborts every task the buffer owns. Removing the jobs first matters: aborting a task can advance its queue in the same call stack, and the next job must not be one of the dying buffer's.

A `finally` handler removes PROC from the list.

### `(tuned-async-slot PROC SLOT)` → PROC

For the same owner (or globally, with no owner) and the same SLOT, a new task aborts the old one. The table is keyed by `(OWNER . SLOT)`. A `finally` handler deletes the entry, but only while the entry still belongs to this task.

Query: `(tuned-async-slot-process SLOT &optional BUFFER)`.

### Queues: `(tuned-async-queue-push KEY JOB &optional ON-DRAIN OWNER)`

The queue's unit is a **job**, not a process: `(lambda (DONE) …)`. The job calls `(funcall DONE)` when it finishes. The queue does not care how many processes a job starts, or whether it runs git or evaluate.

- Jobs with the same KEY run strictly in order; the next starts only after the previous one calls DONE.
- When the queue is empty, ON-DRAIN is called once (a new one replaces the old one), and the queue is removed.
- KEY is usually `(BUFFER . SLOT)`. When BUFFER is killed, its queue is cleared.
- KEY may also be a key of its own, shared by several buffers, such as one per repository. Such a queue outlives its buffers.
- OWNER, a buffer, ties the job to that buffer: killing it removes the job from the queue before it starts. With no OWNER the job is never removed this way.

Two helpers:

| Function | Purpose |
|---|---|
| `(tuned-async-job THUNK)` | THUNK returns a process; the process's finally handler calls DONE |
| `(tuned-async-attempts THUNKS ON-DONE)` | Tries each thunk in turn: the next one runs only if the previous exit code was non-zero. When the last one finishes, it calls ON-DONE and DONE. This is the `:attempts` used by `tuned-async-enqueue`. |

Queries: `(tuned-async-queue-size KEY)`, `(tuned-async-queue-busy-p KEY)`. Clear: `(tuned-async-queue-clear KEY)`, which aborts the job that is running.

## Composition examples

```elisp
;; A Git view: owned, replaces the old request, times out, renders on exit
(thread-first
  (tuned-async-start '("git" "status" "--porcelain=v2") :name "git status")
  (tuned-async-own (current-buffer))
  (tuned-async-slot 'status)
  (tuned-async-timeout 10)
  (tuned-async-on-exit (lambda (p) (my-render (tuned-async-output p)))))

;; Timeout without an owner: check an external command in the background
(thread-first
  (tuned-async-start '("rg" "--version"))
  (tuned-async-timeout 5)
  (tuned-async-on-exit (lambda (p) (message "rg: %s" (tuned-async-exit-code p)))))

;; Stream output, then parse it on exit
(thread-first
  (tuned-async-start '("git" "push") :name "git push")
  (tuned-async-own log-buffer)
  (tuned-async-on-output (lambda (_p chunk) (my-append chunk)))
  (tuned-async-on-exit #'my-parse-push))

;; A queue: "s s s" stages in order
(tuned-async-queue-push (cons (current-buffer) 'stage)
                     (tuned-async-job (lambda () (tuned-async-start (list "git" "add" "--" file)))))
```

## Layer 2: evaluating in a child Emacs

### Child protocol

Evaluate and the worker use the **same** child-process loop. Evaluate sends one request and then closes input.

- Start command: `emacs -Q --batch --eval <short bootstrap>`. The form is **not** on the command line. It is sent on standard input. This avoids command-line quoting problems and the Windows limit of 32767 characters.
- Request: one line `(ID . FORM)`, printed with `prin1`, with `print-escape-newlines`, `print-escape-multibyte` and `print-escape-nonascii` on, and `print-length` and `print-level` nil. The line is pure ASCII, so the child's locale encoding does not matter (this matters most on Windows).
- Child: `read-from-minibuffer` reads one line (in batch mode, from stdin), `read` parses the request, `(eval FORM t)` runs inside `condition-case`, and the child prints a newline, a marker, the ID and the result: `XL-ASYNC-RESULT <ID> (:ok VALUE)` or `XL-ASYNC-RESULT <ID> (:error MESSAGE)`. A newline is printed first, so the marker starts a line even if the form printed half a line. At end of file the child exits.
- Parent: `on-output` splits the output into lines, accepts only lines that start with the marker, and passes each result to the callback for its ID. Other output stays in the buffer for a person to read. Results are read with `read-eval` nil. A single line over the limit (8 MB) is an error and is not read.

### `(tuned-async-evaluate FORM &optional CALLBACK &rest ARGS)` → process

Evaluates FORM in a clean child Emacs. CALLBACK receives `(:ok VALUE)` or `(:error MESSAGE)`. Output goes to `*async-buffer <pid>*`, which is kept after the task ends.

Composed from layer 0 and 1 primitives:

```elisp
(thread-first
  (tuned-async-start (tuned-async-evaluate--command) :keep-input t
                  :buffer-name "*async-buffer %d*" :name "tuned-async-evaluate")
  (tuned-async-evaluate--request 0 form)      ; send the single request
  (tuned-async-close-input)
  (tuned-async-on-exit (lambda (p) (funcall callback (tuned-async-evaluate--result p 0)))))
```

If the child exits without a result (it crashed, was killed, or the command was not Emacs), CALLBACK receives `(:error "no result; …")` with the reason.

### Worker: a long-lived child Emacs

```elisp
(let ((w (tuned-async-worker-start)))
  (tuned-async-worker-eval w '(require 'org) #'ignore)
  (tuned-async-worker-eval w '(org-version) (lambda (r) (message "%S" r)))
  (tuned-async-worker-stop w))
```

- `tuned-async-worker-start` starts a `:keep-input` child and returns the process itself, so layer 1 behaviours (own, timeout) apply to the worker too.
- `tuned-async-worker-eval` assigns an ID, registers the callback, sends the request, and returns the ID.
- Requests are processed in order (the child is single-threaded), and results are delivered by ID. For parallel work, start several workers.
- The child's state persists between requests. That is the difference from evaluate, which always starts from a clean environment. A worker pays the start-up cost once (about 0.13 to 0.18 s in tests) and suits many interactions.
- When a worker exits, every request still outstanding receives `(:error "worker exited: …")`.
- `tuned-async-worker-stop` closes input so the child exits by itself. `tuned-async-abort` kills it.

### `(tuned-async-native-compile FILE &optional CALLBACK &rest ARGS)` → process

A thin wrapper around `(tuned-async-evaluate '(native-compile FILE) …)`:

- When the platform has no native compilation (`native-comp-available-p` is nil, as on some Windows builds), it signals `user-error` at once, starts no child and calls no callback. Callers should check `native-comp-available-p` first.
- On success, VALUE is the path of the eln file.
- For a file that depends on other packages, the caller passes the directories explicitly with `:load-path` (see "Decisions").

## Compatibility layer

No existing caller changed. The old functions are now compositions:

| Old interface | Composition |
|---|---|
| `tuned-async-run` | `tuned-async-start` (`:directory`, `:stdin`, `:name`) → no `own` if the owner is nil, otherwise `tuned-async-own` (default: current buffer) → `tuned-async-slot` if `:slot` is given → `tuned-async-timeout` if `:timeout` is given → `tuned-async-on-output` if `:on-output` is given (wrapped to call `(ON-OUTPUT CHUNK)`) → `tuned-async-on-exit` if `:on-done` is given (wrapped to call `(ON-DONE OUTPUT EXIT-CODE)`) |
| `tuned-async-cancel (&optional SLOT BUFFER)` | Aborts the tasks in SLOT (or all) for BUFFER |
| `tuned-async-busy-p` | Looks up the slot table or the owner list |
| `tuned-async-enqueue` | `tuned-async-queue-push` with `tuned-async-attempts`; `:on-start` runs when the job starts; `:owner` selects the buffer that owns the attempts (default: current buffer, nil: unowned); a cons SLOT is an explicit queue key shared by other buffers |
| `tuned-async-queue-length`, `tuned-async-queue-busy-p`, `tuned-async-clear-queue` | The matching queue query or clear; all accept an explicit key |
| `tuned-async-timeout-code` | Kept; the value is still 124 |

Behaviour that callers rely on (covered by existing tests): ON-DONE runs in OWNER; ON-DONE does not run if OWNER is killed or the task is cancelled; an unowned task's ON-DONE always runs, and `:slot` cannot be used with it; blocks delivered through `:on-output` are all present in the OUTPUT passed to ON-DONE.

## Loader split

`tuned-spinner-*` does not depend on processes, so it lives in `lisp/tuned-spinner.el` (`provide 'tuned-spinner`). The process layer no longer requires it: `tuned-async.el` draws nothing, and every caller that shows a loader (`tuned-git-status`, `tuned-dired`, `tuned-org-babel-async`, `tuned-eln-opt`, ...) requires `tuned-spinner` itself. The dependency now points one way, process -> UI is gone.

## Observation (perfetto)

Perfetto records asynchronous work by advising the entry point. The design moves the advice from `tuned-async-run` to `tuned-async-start`, because every process now starts there: a `b` event at the start and an `e` event in the finally handler, named by `:name`. That would make evaluate, the worker and native compile visible on the timeline, while compatibility calls are still recorded.

**Current state:** `tuned-perf-trace.el` advises `tuned-async-start` (see `tuned-perf--async-around`), so jobs made directly, such as evaluate, the worker, package installs and native compiles, are recorded as well as those made through `tuned-async-run`.

## Platforms

| Platform | Notes |
|---|---|
| macOS | Scheduling wrappers such as `taskpolicy -b` belong to the command, not the core. |
| Linux | No special handling. |
| Windows | Forms go on standard input, not the command line, which avoids quoting and length limits. Requests and results are ASCII-only, so code pages do not matter. `:coding` defaults to `utf-8-unix`. Without native compilation, native-compile signals an error and starts no process. |

## Safety

- Evaluate and the worker run arbitrary forms in a child process. The trust boundary is the same as calling `eval` in the current Emacs: only forms built by the caller are run, never text read from files or the network.
- The parent reads results with `read-eval` nil. It reads only marked lines, and each line has a size limit.
- The child starts with `-Q`, so it does not load the user's configuration and cannot have side effects from it.

## Performance

- Each child Emacs starts in about 0.13 to 0.18 s (`-Q`, measured in tests). A worker amortises this over many small tasks.
- Dispatching output to handlers walks a list of handlers, usually 0 to 2 entries.
- The reaper checks each running task every 0.25 s and stops when the task ends, as before.
- At the end, output is copied into a string, so briefly there are two copies (the buffer and the string). This does not matter for Git output sizes.

## Test plan

Each contract has a test. Layer 0 and 1 tests are in `tests/tuned-async-tests.el`; layer 2 tests are in `tests/tuned-async-evaluate-tests.el`. The compatibility layer is still covered by `smoke-tests.el`.

| Contract | Test |
|---|---|
| on-exit runs exactly once | When both the sentinel and the reaper fire, the handler runs once |
| Cancel does not call back | After abort, on-exit does not run and finally runs once |
| Complete output | Large output (over 1 MB) has the correct length in on-exit |
| Late registration | on-exit and finally registered after the process ended still run |
| Handler isolation | If the first handler fails, the second still runs |
| Timeout | Status `timeout`, exit code 124, on-exit runs, timer cancelled |
| Ownership | Handlers run in the owner; killing the owner aborts the task and on-exit does not run |
| Slots | A new task on the same slot aborts the old one; the old task's finally does not delete the new entry |
| Queues | Jobs run in order; attempts try the next command after a failure; ON-DRAIN runs once when the queue empties |
| Evaluate | Values, errors, unreadable values, a child with no output, non-ASCII round trips |
| Worker | Multiple requests are delivered by ID; state persists between requests; outstanding requests get an error when the worker dies |
| Compatibility | The existing smoke tests and perfetto tests pass unchanged |

## Status of the migration

The plan had six steps, each one commit, with all test suites passing at every step:

1. Layer 0 in `tuned-async.el` next to the old code, with `tests/tuned-async-tests.el`. **Done.**
2. Layer 1 with tests. **Done.**
3. The compatibility layer: `tuned-async-run` and `tuned-async-enqueue` rewritten as compositions, the old implementation removed, existing tests unchanged. **Done.**
4. Perfetto advising `tuned-async-start` instead of `tuned-async-run`. **Done**: perfetto advises `tuned-async-start`.
5. Evaluate on layer 0 with the standard-input protocol, plus the worker and native compile. **Done** in `lisp/tuned-async-evaluate.el`.
6. Loader split into `tuned-spinner.el`. **Done.**

## Decisions

These were settled before the first version, for simplicity and stability.

1. **Layer 2 file name: `tuned-async-evaluate.el`.** Evaluate is the core of this layer; the worker and native compile are built on it.
2. **Cleanup of `*async-buffer <pid>*`: keep the 10 most recent.** Each new evaluate or worker deletes older buffers. Output stays inspectable, and the number of buffers is bounded without manual cleanup.
3. **Handler arguments: `(FN PROC)`.** The process is the only object. State, output and exit code are all queried from it, so the shape is uniform and later additions do not change the signature. The compatibility layer wraps `(FN PROC)` back into the old `(ON-DONE OUTPUT EXIT-CODE)`, with a few lines per wrapper.
4. **The child's `load-path` is passed explicitly with `:load-path`.** The child adds only those directories in front of its own `load-path`. It does not inherit the parent's whole `load-path`, so its behaviour is reproducible and does not change with the parent's configuration.
5. **Workers do not run concurrent requests.** Requests are processed in order; for parallelism, start several workers. This gives one execution order, which makes state and errors easier to reason about.

## Open items

- **Feature-cache notes from the design phase.** The `tuned-async-jit-rank` module and its feature cache no longer exist. Nothing here needs follow-up unless a cache is reintroduced.
