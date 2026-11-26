# xl-perfetto-trace: a Perfetto timeline of the Emacs Lisp layer

Status: implemented (2026-11-25). Implementation notes are in [`xl-perfetto-trace-prompt.md`](xl-perfetto-trace-prompt.md).

## In one sentence

Record what Emacs does at the Lisp level over a period of time: commands, timers, process callbacks, asynchronous tasks from xl-async, garbage collection, font-lock, and any functions you name. The result is a Chrome JSON trace file. Open it at [ui.perfetto.dev](https://ui.perfetto.dev) to see the timeline and where the time went.

**Motivation.** During a slow Chinese input-method session, guessing and batch measurements found that every commit ran an `ispell-completion-at-point` for about 230 ms. A timeline shows that directly.

## Goals and non-goals

**Goals**

- Emacs Lisp only. No changes to Emacs's C code, and no external programs.
- Zero cost when not recording: no hooks, no advice, no timers.
- Bounded cost while recording: a few microseconds per event, with a cap on the number of events.
- The same behaviour on macOS, Linux and Windows.
- Output that Perfetto UI opens directly, with no conversion.

**Non-goals (not in the first version)**

- Inside the C layer: redisplay phases, key reading, font rendering. Elisp cannot see these.
- Merging external samplers (Linux `perf`, macOS Instruments).
- Perfetto's protobuf format. JSON is enough.
- Tracing every function. Only the boundaries listed below, plus functions the user names.

## Principles

1. **No changes to C code.**
2. **No advice on C primitives.** Advising a C primitive (where `subr-primitive-p` is true) makes a native-compiled Emacs build a trampoline synchronously, which takes about 1 second and freezes the interface. This repository has been caught by that (see the trampoline section of [eln-opt.md](eln-opt.md)). So `make-process`, `set-process-filter`, `garbage-collect` and similar functions are never advised. `xl-perfetto-trace-function` also refuses C primitives (see "Functions the user names").
3. **Zero cost by default.** Every hook, advice, process-function wrapper and timer exists only between `xl-perfetto-trace-start` and `xl-perfetto-trace-stop`. Stopping removes all of them, including on error (`unwind-protect`).
4. **Bounded.** Events go into a fixed-size ring buffer that overwrites the oldest events when full. A recording has a maximum length and stops automatically.
5. **No I/O while recording.** The file is written once, at stop.
6. **No content.** Key sequences, buffer text, minibuffer input and process output are not recorded. Only names, durations and sizes are (see "Privacy").
7. **The tracer does not trace itself.** A dynamic variable guards against re-entry: if code inside the tracer triggers a traced boundary again (for example, allocating inside `post-gc-hook` and causing another GC), the nested call is skipped.

## User interface

File: `tuned-emacs/lisp/xl-perfetto-trace.el`. Feature `xl-perfetto-trace`. Public prefix `xl-perfetto-`; internal prefix `xl-perfetto--`.

### Commands

| Command | Effect |
|---|---|
| `xl-perfetto-trace-start` | Start recording. With a prefix argument, asks for the maximum length in seconds. If already recording, says so and does not restart. |
| `xl-perfetto-trace-stop` | Stop recording, remove all instrumentation, write the file, show its path in the echo area, and return the path. Says so if not recording. |
| `xl-perfetto-trace-function` (a Lisp function, not a command) | Trace one function during this recording (`:around` advice). It can be registered before recording starts; it takes effect when recording starts. It does not appear in the command palette. |
| `xl-perfetto-untrace-function` (a Lisp function) | Stop tracing a function. |
| `xl-perfetto-trace-mark` | Ask for some text and add an instant marker to the timeline, for example "start typing Chinese". |
| `xl-perfetto-trace-open` | Open ui.perfetto.dev with `browse-url`, and show the most recent trace file in the system file manager, so it can be dragged into the page. |

### Macro for code

```elisp
(xl-perfetto-with-span "eln-opt rank" (eln-opt-rank--compute dir))
```

When not recording, this costs one variable check and then runs BODY. When recording, it records a span, and a non-local exit also ends the span.

### Options (defcustom, group `xl-perfetto`)

| Variable | Default | Meaning |
|---|---|---|
| `xl-perfetto-trace-categories` | all | Categories to record: `commands`, `timers`, `processes`, `xl-async`, `gc`, `redisplay`, `functions`, `counters`. |
| `xl-perfetto-trace-ring-size` | 100000 | Number of events the ring buffer holds. |
| `xl-perfetto-trace-max-seconds` | 120 | Maximum recording length. Stops and writes the file when reached. |
| `xl-perfetto-trace-directory` | `traces/` under `user-emacs-directory` | Output directory, created if missing. |
| `xl-perfetto-trace-record-names` | t | Whether buffer and file names are recorded. When nil, only the major mode of a buffer is recorded. |
| `xl-perfetto-trace-process-scan-interval` | 0.5 | Seconds between scans for new processes during recording (see "Processes"). |

### Recording indicator

While recording, the mode line shows `● REC` and the seconds recorded (through `global-mode-string`; removed at stop). xl-spinner is not used, because it is per buffer and recording is global.

## Output format

Chrome JSON trace format (Trace Event Format), which Perfetto UI reads natively. The overall structure:

```json
{"displayTimeUnit": "ms",
 "traceEvents": [ ... ],
 "metadata": {"emacs-version": "31.1", "system-type": "darwin",
              "started": "2026-11-25T12:00:00", "dropped-events": 0}}
```

**Time.** `ts` and `dur` are in microseconds, taken from `(car (time-convert nil 1000000))`: integer microseconds, exact, with no floating point. The start of the recording is subtracted, so times begin at 0.

**Process and tracks.** `pid` is always `(emacs-pid)`. Each category has one "thread" track, named at the start by an `M` metadata event:

| tid | Track | Contents |
|---|---|---|
| 1 | commands | Commands |
| 2 | timers | Timers (normal and idle) |
| 3 | processes | Process filters and sentinels |
| 4 | gc | Garbage collection |
| 5 | redisplay | Redisplay-start markers and font-lock |
| 6 | functions | User-named functions and `xl-perfetto-with-span` |
| 7 | marks | User marks |

Asynchronous xl-async tasks use async events (`b`/`e`), which Perfetto shows on their own async tracks. Counters use `C` events, one counter track each.

**Event examples**

```json
{"name":"thread_name","ph":"M","pid":123,"tid":1,"args":{"name":"commands"}}
{"name":"self-insert-command","cat":"commands","ph":"X","ts":1200,"dur":85,"pid":123,"tid":1,
 "args":{"buffer":"notes.org","mode":"org-mode"}}
{"name":"git status","cat":"xl-async","ph":"b","id":"xl-async-7","ts":3000,"pid":123,"tid":3}
{"name":"git status","cat":"xl-async","ph":"e","id":"xl-async-7","ts":41000,"pid":123,"tid":3,
 "args":{"exit":0,"bytes":5120}}
{"name":"memory","ph":"C","ts":50000,"pid":123,"args":{"conses":123456,"strings":2345}}
{"name":"start typing Chinese","cat":"marks","ph":"i","s":"t","ts":60000,"pid":123,"tid":7}
```

## Event sources

For each source: the mechanism, the events it produces, the arguments recorded, and what to watch for.

### 1. Commands (`commands`)

- **Mechanism.** `pre-command-hook` records the start time and `this-command`. `post-command-hook` produces an `X` event. Both hooks go on the global value and are placed first and last (`add-hook` with DEPTH -100 and 100), so the measured time covers other hooks.
- **Name.** The symbol name of `this-command`, or `"(anonymous command)"`.
- **Arguments.** `buffer` (controlled by `xl-perfetto-trace-record-names`) and `mode`. `this-command-keys` is **not** recorded, because it may contain text being typed.
- **Watch for.** If a command signals an error, `post-command-hook` still runs. If two `pre` events occur with no `post` between them (for example, in a recursive edit), the earlier start time is discarded, so no false span is produced.

### 2. Timers (`timers`)

- **Mechanism.** `:around` advice on the Lisp function `timer-event-handler`.
- **Name.** The symbol name of `(timer--function timer)`, or `"(anonymous timer)"`.
- **Arguments.** `idle`, which is t when `(timer--idle-delay timer)` is non-nil.
- **Watch for.** Idle timers are the main source of stutter between keystrokes, so they must be kept apart from normal timers.

### 3. Process callbacks (`processes`)

- **Mechanism.** `add-function :around` wraps `(process-filter proc)` and `(process-sentinel proc)`. This is the proper nadvice use: `remove-function` restores exactly, and no C primitive is advised.
- **Which processes.**
  1. Processes that exist when recording starts: each is wrapped after walking `(process-list)`.
  2. Processes created during recording: `make-process` cannot be advised (principle 2), so a timer scans `process-list` every `xl-perfetto-trace-process-scan-interval` seconds and wraps any process not yet wrapped. The drawback: output in the first 0.5 seconds of a new process may be missed. Processes started by xl-async are recorded exactly (see item 4).
- **Name.** `filter <process name>` or `sentinel <process name>`.
- **Arguments.** The filter records the byte count of each output chunk (length only, never content). The sentinel records the event string, such as `"finished\n"`.
- **At stop.** `remove-function` is called on every wrapped process, including processes that have already exited. A list records which processes were wrapped.

### 4. xl-async tasks (`xl-async`)

- **Mechanism.** `:around` advice on the Lisp function `xl-async-run` (see `tuned-emacs/lisp/xl-async.el`). The argument plist is copied, not modified. `:on-done` is wrapped so that an `e` event is recorded before the original callback runs. A `b` event is recorded at start.
- **Id.** `xl-async-<incrementing counter>`; the `b` and `e` events share it.
- **Name.** The `:name` argument, or else the program name from `:command` (the file-name part of its first element).
- **Arguments.** The `e` event records `exit` (the exit code) and `bytes` (the output length).
- **Watch for.** A cancelled task (`xl-async-cancel`) does not call `:on-done`, so it has a `b` and no `e`. At stop, every unfinished id receives an `e` with `cancelled-or-running` set to t, so Perfetto shows no unterminated track.

### 5. Garbage collection (`gc`)

- **Mechanism.** `post-gc-hook`. At start, `gc-elapsed` and `gcs-done` are recorded. Each time the hook runs, the increase in `gc-elapsed` gives the duration of that collection, and an `X` event starts at "now minus duration".
- **Arguments.** `gcs-done`, and the collection's sequence number.
- **Watch for.** `post-gc-hook` must not allocate heavily, since that can trigger further collections. Events are written into positions preallocated in the ring buffer.

### 6. Redisplay and font-lock (`redisplay`)

- **Mechanism.**
  - `pre-redisplay-functions`: an instant event `redisplay` at the start of each redisplay. This can be frequent, so at most one is recorded per millisecond.
  - `:around` advice on the Lisp function `jit-lock-function`: one `X` event per fontification.
- **Arguments.** The buffer that was fontified (subject to the option), the major mode, and the character count.
- **Watch for.** The end of a redisplay cannot be observed, so a redisplay has only its start marker. The duration of font-lock is exact.

### 7. Functions the user names (`functions`)

- **Mechanism.** `:around` advice named `xl-perfetto--function-advice`, so it can be removed precisely.
- **Refusing C primitives.** If `(subr-primitive-p (indirect-function fn))` is true, tracing is refused by default, with the reason (trampolines). The exception: if `eln-opt--trampoline-cached-p` reports that the trampoline is already cached, or this Emacs has no native compilation (`native-comp-available-p` is nil), tracing is allowed.
- **Refusing the tracer's own functions** (prefix `xl-perfetto-`).
- **Non-local exits.** The advice body uses `unwind-protect`, so the span ends on error, and the error propagates as usual.

### 8. Marks (`marks`)

- `xl-perfetto-trace-mark` produces an `i` instant event whose name is the text the user typed.

### 9. Counters (`counters`)

- After each GC, and once a second (reusing the process-scan timer), a `C` event with the entries of `memory-use-counts`, `gcs-done`, the number of live processes, and the number of buffers.

## Time, buffer and re-entry guard

- **Ring buffer.** A vector of length `xl-perfetto-trace-ring-size`, with a write pointer and a count of overwritten events. When full, the oldest events are overwritten, and the overwrite count is written as `dropped-events` in the metadata.
- **In-memory event form.** A small vector `[PH NAME CAT TS DUR TID ARGS ID]`, where `ARGS` is a plist or nil. No JSON is built while recording; conversion happens at stop.
- **Re-entry guard.** A dynamic variable `xl-perfetto--inside`. When it is non-nil, all instrumentation calls the original function directly and records nothing. Recording binds it to t.
- **Start time.** `xl-perfetto--t0`, in integer microseconds.

## Lifecycle

**Start** (inside `condition-case`: if any step fails, the steps already done are rolled back and the error is reported):

1. Allocate the ring buffer, and record `t0`, `gc-elapsed` and `gcs-done`.
2. Write the `M` events that name the tracks.
3. Install instrumentation in category order: hooks, advice, wrapping existing processes, and starting the scan timer. Each installed item is recorded in an "installed" list.
4. Start the timer that stops recording automatically.
5. Turn on the mode-line indicator.

**Stop** (idempotent, inside `unwind-protect`, so instrumentation is always removed):

1. Remove everything in the "installed" list, in reverse order: hooks, advice, process-function wrappers and timers.
2. Add an `e` event for each unfinished xl-async id.
3. Turn off the mode-line indicator.
4. Convert the buffer to JSON (`json-serialize`) and write the file.
5. Show the path and the event count in the echo area, and return the path.

**Other cases.**

- If Emacs exits while recording, nothing is done and the buffer is discarded. No `kill-emacs-hook` is installed, since recording is meant to be temporary.
- Calling start during a recording only reports it; it does not restart.

## Writing the file

- File name: `emacs-trace-<YYYYMMDD-HHMMSS>.json`, in `xl-perfetto-trace-directory`.
- Method: write a temporary file and rename it, as `eln-opt--write-eld` does, so a partly written file is never seen.
- Size: 100,000 events are about 15 to 25 MB, which Perfetto UI opens easily.
- Opening: ui.perfetto.dev parses the file locally in the browser, so the file is not uploaded. The offline build of Perfetto also works without a network.

## Following the eln-opt worker

When the main Emacs starts recording, it writes a marker file `traces/recording.eld` (containing the recording directory, the start time and the limit), and deletes it at stop. The eln-opt background worker is a separate Emacs process. Between files (after its existing 5-second pause), it checks this marker. If the marker is present and not expired, the worker starts its own recording in the same directory, with a file name beginning `eln-opt-trace-`. When the marker disappears or expires, it stops and writes its file, and prints `ELN-OPT trace <file>` in the worker log. The main Emacs needs no knowledge of the worker, and uses no hooks, only direct calls.

- The marker is part of the recording: `xl-perfetto--start` writes it and `xl-perfetto--stop` deletes it. Writing and deleting never make a recording fail.
- An expired marker is treated as stale, so a crashed main Emacs does not leave a permanent marker.
- While the worker is compiling a large file, it does not check the marker; it waits until that file finishes. A recording that starts late therefore misses the beginning.
- The recording and worker tests bind `xl-perfetto-marker-file` to a temporary file, so they never touch the real marker.

Spans recorded inside the worker (only when recording):

| Name | Meaning |
|---|---|
| `eln-opt worker` | The whole run |
| `reach: roots` / `lazy roots` / `graph` / `propagate` / `file scores` | Steps of the reachability analysis |
| `reach analysis` | The whole analysis (only when the plan needs native candidates) |
| `byte candidates` | Byte-compile candidates |
| `byte compiling <file>` / `native compiling <file>` | Each file's compile |
| `byte <defun>` | Each top-level form of a byte compile, named by the function it defines |
| `warm new eln: <file>` | Warming a freshly made eln (loading it once so the system does its first check) |
| `warm all eln` | The warming scan of the whole eln directory |

Native compilation has no per-function entry: `native-compile` compiles a whole file at once, so native spans stop at file level. Top-level byte-compile forms (each `defun`) can be recorded per function, because `byte-compile-top-level` is a native-compiled Lisp function, not a C primitive, so advising it in the worker needs no trampoline.

Measured (a batch main process recording, with the real worker command as the child, and the plan limited to one file): the worker's trace had 116 spans, of which graph building took 2.6 s, the whole analysis 6.5 s, and warming 1.0 s. The main and worker processes have different pids, so their files are two separate timelines in ui.perfetto.dev.

## Performance budget

- Target: no more than 5 µs per recorded event (Apple Silicon). Typing should not feel any difference.
- Method: trace an empty function, call it 100,000 times, and compare the time with and without tracing. The command `xl-perfetto-trace-benchmark` was removed (it existed only for one-off measurement, so it is not kept). Tests do not assert timings, to avoid flakiness on slow machines.
- Measured (2026-11-25, Emacs 31.1 `aarch64-apple-darwin23.6.0`, `emacs --batch`, 100,000 calls, three runs): **about 3.2 µs per call when byte-compiled**, which meets the 5 µs target. Loading the `.el` source directly costs about 7.1 µs per call, because the recording path itself runs interpreted. A normal install compiles through `compile-lisp` into `.elc`, so 3.2 µs is the figure for run time. Commands and timer events are recorded once each, so this figure is not multiplied for them, and it is not noticeable when typing. The test only prints this number and does not assert it.

## Privacy and security

- Not recorded: key sequences, buffer text, minibuffer input, process output, environment variables.
- Recorded: function names, command names, timer function names, process names, exit codes, byte counts, and buffer and file names (can be turned off).
- Trace files are written locally only. Before sharing a trace, remember that it contains buffer and file names. Set `xl-perfetto-trace-record-names` to nil if needed.
- Stopping always removes all instrumentation, so no permanent tracing advice is left behind.

## Files and integration

- New file: `tuned-emacs/lisp/xl-perfetto-trace.el`. Its header follows `tuned-emacs/lisp/xl-dired.el` (Author, Version, `Package-Requires: ((emacs "30.1") (xl-async "0.1.0"))`, Commentary). xl-async is an optional dependency: when it is not loaded, item 4 is skipped.
- New test: `tuned-emacs/tests/xl-perfetto-trace-tests.el`, run on its own with `emacs --batch -Q -L tuned-emacs/lisp`, in the style of `tuned-emacs/tests/xl-dired-tests.el`.
- `tuned-emacs/install/emacs-setup.sh`: `cmd_test` runs this test file.
- `tuned-emacs/tests/smoke-tests.el`: the file list in `xl-personal-file-is-framework-neutral` includes `tuned-emacs/lisp/xl-perfetto-trace.el`.
- `tuned-emacs/init.el`: one line, `(require 'xl-perfetto-trace)`. It installs nothing until recording starts, so its load cost is small.
- Key bindings: not decided in the first version; left to the user.

## Test plan

ERT. Every test runs in a temporary directory and confirms at the end that no instrumentation remains.

1. **No trace after start and stop.** `pre-command-hook`, `post-command-hook`, `post-gc-hook` and `pre-redisplay-functions` are restored exactly; `timer-event-handler`, `jit-lock-function` and `xl-async-run` carry no tracer advice; no tracer timers remain; each process's filter and sentinel are `eq` to their values before start.
2. **Commands.** Simulate pre and post with `run-hooks`, bind `this-command` to a symbol, and get an `X` event with the right name and a non-negative `dur`. No key appears in the arguments.
3. **Timers.** Run a named function with `run-at-time 0`, wait for it, and get the matching event with `idle` nil. An idle timer gets `idle` t.
4. **GC.** Call `garbage-collect`, and get a `gc` event with a non-negative `dur`.
5. **Processes.** Start a process with a filter before recording (for example `printf`, or Emacs in batch). While recording it produces output, and a `filter` event appears with only the byte count in its arguments. After stop, the filter is `eq` to the original. A process created during recording is wrapped after one scan.
6. **xl-async.** Recording `xl-async-run` of a short command gives `b` and `e` with the same id. A task cancelled with `xl-async-cancel` receives an `e` at stop.
7. **User functions.** Trace a function that signals an error: the span still ends and the error propagates. Tracing a C primitive is refused (on a native-compiled Emacs without a cached trampoline). Tracing a function with the `xl-perfetto-` prefix is refused.
8. **Re-entry guard.** Inside a traced function, call another traced boundary: no infinite recursion, and a reasonable event count.
9. **Ring buffer.** Set the size to 10, record 25 events, keep the last 10, and get `dropped-events` 15.
10. **Automatic stop.** Set a very short maximum, wait, and check that recording has stopped, the file was written, and the instrumentation was removed.
11. **Output file.** `json-parse-string` succeeds; there is a `traceEvents` key; every event has `ph`, `ts` and `pid`; `dur` on `X` events is non-negative; the `M` track-name events exist; the metadata has `emacs-version` and `dropped-events`.
12. **Rollback on a failed start.** Simulate an error partway through installation: instrumentation already installed is removed, and the state is not recording.
13. **Name recording switch.** With `xl-perfetto-trace-record-names` nil, events contain no buffer or file names.

## Acceptance criteria

- `bash tuned-emacs/install/emacs-setup.sh test` passes completely, including the new test.
- In a real GUI Emacs, record 10 seconds while typing, switching buffers and opening an xl-dired preview. The file opens in ui.perfetto.dev, and shows tracks for commands, timers, GC, xl-async and font-lock.
- Not recording: no tracer function remains in the global hooks, and `advice-member-p` is nil for every tracer advice.

## Possible future work

- Merge external samplers: on Linux, put the output of `perf script` and the Elisp events in one trace (the clocks must be aligned).
- Add `xl-perfetto-with-span` to the critical paths of eln-opt and xl-dired.
- Produce flame graphs from Emacs's built-in `profiler`. It has no timestamps, so it gives call trees only, not a timeline.
- An on-demand snapshot: one command records for 10 seconds, stops by itself and opens the file.

## Known limitations

- The C layer is not visible: a redisplay has only a start marker, with no per-phase durations. Key-reading latency can only be inferred from gaps between commands.
- For processes that are not xl-async, output in the first `xl-perfetto-trace-process-scan-interval` seconds after creation may be missed.
- During a recursive edit (for example, running a command inside the minibuffer), the span of the outer command is discarded rather than shown nested.
