# tuned-perf-trace: a Perfetto timeline of the Emacs Lisp layer

Status: implemented (2026-11-25); redisplay frames added (2026-11-28). Implementation notes are in [`tuned-perf-trace-prompt.md`](tuned-perf-trace-prompt.md).

## In one sentence

Record what Emacs does at the Lisp level over a period of time: commands, timers, process callbacks, asynchronous tasks from tuned-async, garbage collection, font-lock, redisplay frames with jank, and any functions you name. The result is a Chrome JSON trace file. Open it at [ui.perfetto.dev](https://ui.perfetto.dev) to see the timeline and where the time went.

**Motivation.** During a slow Chinese input-method session, guessing and batch measurements found that every commit ran an `ispell-completion-at-point` for about 230 ms. A timeline shows that directly.

## Goals and non-goals

**Goals**

- Emacs Lisp only. No changes to Emacs's C code, and no external programs.
- Zero cost when not recording: no hooks, no advice, no timers.
- Bounded cost while recording: a few microseconds per event, with a cap on the number of events.
- The same behaviour on macOS, Linux and Windows.
- Output that Perfetto UI opens directly, with no conversion.

**Non-goals (not in the first version)**

- Inside the C layer: the individual redisplay phases, key reading, font rendering. Elisp cannot see these. A redisplay frame gets a start, an end and the Lisp phases inside it, but not the C layout and drawing phases.
- Merging external samplers (Linux `perf`, macOS Instruments).
- Perfetto's protobuf format. JSON is enough.
- Tracing every function. Only the boundaries listed below, plus functions the user names.

## Principles

1. **No changes to C code.**
2. **No advice on C primitives.** Advising a C primitive (where `subr-primitive-p` is true) makes a native-compiled Emacs build a trampoline synchronously, which takes about 1 second and freezes the interface. This repository has been caught by that. So `make-process`, `set-process-filter`, `garbage-collect` and similar functions are never advised. `tuned-perf-trace-function` also refuses C primitives (see "Functions the user names").
3. **Zero cost by default.** Every hook, advice, process-function wrapper and timer exists only between `tuned-perf-trace-start` and `tuned-perf-trace-stop`. Stopping removes all of them, including on error (`unwind-protect`).
4. **Bounded.** Events go into a fixed-size ring buffer that overwrites the oldest events when full. A recording has a maximum length and stops automatically.
5. **No I/O while recording.** The file is written once, at stop.
6. **No content.** Key sequences, buffer text, minibuffer input and process output are not recorded. Only names, durations and sizes are (see "Privacy").
7. **The tracer does not trace itself.** A dynamic variable guards against re-entry: if code inside the tracer triggers a traced boundary again (for example, allocating inside `post-gc-hook` and causing another GC), the nested call is skipped.

## User interface

File: `tuned-emacs/lisp/tuned-perf-trace.el`. Feature `tuned-perf-trace`. Public prefix `tuned-perf-`; internal prefix `tuned-perf--`.

### Commands

| Command | Effect |
|---|---|
| `tuned-perf-trace-start` | Start recording. With a prefix argument, asks for the maximum length in seconds. If already recording, says so and does not restart. |
| `tuned-perf-trace-stop` | Stop recording, remove all instrumentation, write the file, show its path in the echo area, and return the path. Says so if not recording. |
| `tuned-perf-trace-function` (a Lisp function, not a command) | Trace one function during this recording (`:around` advice). It can be registered before recording starts; it takes effect when recording starts. It does not appear in the command palette. |
| `tuned-perf-untrace-function` (a Lisp function) | Stop tracing a function. |
| `tuned-perf-trace-mark` | Ask for some text and add an instant marker to the timeline, for example "start typing Chinese". |
| `tuned-perf-trace-open` | Open ui.perfetto.dev with `browse-url`, and show the most recent trace file in the system file manager, so it can be dragged into the page. |

### Macro for code

```elisp
(tuned-perf-with-span "scan" (scan-dir dir))
```

When not recording, this costs one variable check and then runs BODY. When recording, it records a span, and a non-local exit also ends the span.

### Options (defcustom, group `tuned-perf`)

| Variable | Default | Meaning |
|---|---|---|
| `tuned-perf-trace-categories` | all | Categories to record: `commands`, `keys`, `timers`, `processes`, `tuned-async`, `gc`, `redisplay`, `functions`, `modes`, `completion`, `input`, `files`, `counters`. |
| `tuned-perf-trace-ring-size` | 100000 | Number of events the ring buffer holds. |
| `tuned-perf-trace-max-seconds` | 120 | Maximum recording length. Stops and writes the file when reached. |
| `tuned-perf-trace-directory` | `traces/` under `user-emacs-directory` | Output directory, created if missing. |
| `tuned-perf-trace-record-names` | t | Whether buffer and file names are recorded. When nil, only the major mode of a buffer is recorded. |
| `tuned-perf-trace-process-scan-interval` | 0.5 | Seconds between scans for new processes during recording (see "Processes"). |
| `tuned-perf-trace-frame-budget` | 0.0167 | Budget of one redisplay frame in seconds. A longer frame is marked `jank`, like a missed vsync (see "Redisplay"). |
| `tuned-perf-trace-frame-min-interval` | 0.008 | Shortest gap between two recorded frames. Faster redisplays are coalesced into the next recorded frame; a janky frame is always recorded. 0 records every redisplay. |
| `tuned-perf-trace-redisplay-detail` | t | Record one marker per redisplayed window and a span for each Lisp phase inside redisplay. When nil, only the frame span remains. |

### Recording indicator

While recording, the mode line shows `● REC` and the seconds recorded (through `global-mode-string`; removed at stop). tuned-spinner is not used, because it is per buffer and recording is global.

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
| 5 | redisplay | Redisplay frames, per-window markers, Lisp phase spans and font-lock |
| 6 | functions | User-named functions and `tuned-perf-with-span` |
| 7 | marks | User marks |
| 8 | modes | Major-mode changes |
| 9 | completion | `completion-at-point`, vertico and corfu |
| 10 | files | `find-file`, `save-buffer` and the save hooks |
| 11 | input | pyim, the Chinese input method |

Asynchronous tuned-async tasks use async events (`b`/`e`), which Perfetto shows on their own async tracks. Counters use `C` events, one counter track each.

**Event examples**

```json
{"name":"thread_name","ph":"M","pid":123,"tid":1,"args":{"name":"commands"}}
{"name":"self-insert-command","cat":"commands","ph":"X","ts":1200,"dur":85,"pid":123,"tid":1,
 "args":{"buffer":"notes.org","mode":"org-mode"}}
{"name":"git status","cat":"tuned-async","ph":"b","id":"tuned-async-7","ts":3000,"pid":123,"tid":3}
{"name":"git status","cat":"tuned-async","ph":"e","id":"tuned-async-7","ts":41000,"pid":123,"tid":3,
 "args":{"exit":0,"bytes":5120}}
{"name":"redisplay frame","cat":"redisplay","ph":"X","ts":42000,"dur":20000,"pid":123,"tid":5,
 "args":{"windows":2,"fontified":830,"jank":true,"interval-ms":16.7,
         "key-latency-ms":21.3,"command-latency-ms":22.1,"after-command-ms":1.4,
         "resized":true,"buffer":"notes.org","mode":"org-mode"}}
{"name":"redisplay frame","ph":"C","ts":62000,"pid":123,
 "args":{"ms":20.0,"interval-ms":16.7,"windows":2}}
{"name":"^","cat":"redisplay","ph":"i","s":"t","ts":42000,"pid":123,"tid":5,
 "args":{"frame-ms":20.0,"budget-ms":16.7,"windows":2}}
{"name":"start typing Chinese","cat":"marks","ph":"i","s":"t","ts":60000,"pid":123,"tid":7}
```

## Event sources

For each source: the mechanism, the events it produces, the arguments recorded, and what to watch for.

### 1. Commands (`commands`)

- **Mechanism.** `pre-command-hook` records the start time and `this-command`. `post-command-hook` produces an `X` event. Both hooks go on the global value and are placed first and last (`add-hook` with DEPTH -100 and 100), so the measured time covers other hooks.
- **Name.** The symbol name of `this-command`, or `"(anonymous command)"`.
- **Arguments.** `buffer` (controlled by `tuned-perf-trace-record-names`) and `mode`. `this-command-keys` is **not** recorded, because it may contain text being typed.
- **M-x.** `execute-extended-command` is advised too: the command it runs gets its own span, named `M-x NAME`, nested inside the `execute-extended-command` span, so the command's own time is visible.
- **Keys (`keys`).** When a special key starts a command, an instant `key RET` is recorded at that moment, on the commands track. `tuned-perf-trace-keys` picks the keys (`return`, `tab`, `escape`; default `return`). Printable keys are never recorded. The time is when the command loop takes the key, which is the closest point Emacs offers to the key press.
- **Key to window.** Read a `key RET`, the command span after it, and the first `redisplay` event after that: their gap is the time from the key to the drawn window, for example `magit-status`.
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
  2. Processes created during recording: `make-process` cannot be advised (principle 2), so a timer scans `process-list` every `tuned-perf-trace-process-scan-interval` seconds and wraps any process not yet wrapped. The drawback: output in the first 0.5 seconds of a new process may be missed. Processes started by tuned-async are recorded exactly (see item 4).
- **Name.** `filter <process name>` or `sentinel <process name>`.
- **Arguments.** The filter records the byte count of each output chunk (length only, never content). The sentinel records the event string, such as `"finished\n"`.
- **At stop.** `remove-function` is called on every wrapped process, including processes that have already exited. A list records which processes were wrapped.

### 4. tuned-async tasks (`tuned-async`)

- **Mechanism.** `:around` advice on the Lisp function `tuned-async-start`, the one place every job starts, so jobs made directly (package installs, native compiles) are recorded as well as those made through `tuned-async-run`. A `b` event is recorded at start. A finally handler on the process records the `e` event when the job ends, however it ends.
- **Id.** `tuned-async-<incrementing counter>`; the `b` and `e` events share it.
- **Name.** The `:name` argument, or else the program name from `:command` (the file-name part of its first element).
- **Arguments.** The `e` event records `status` (done, timeout or cancelled), `exit` (the exit code) and `bytes` (the output length).
- **Watch for.** A cancelled job ends at once with status `cancelled`. At stop, a job still running gets an `e` with `cancelled-or-running` set to t, so Perfetto shows no unterminated track.

### 5. Garbage collection (`gc`)

- **Mechanism.** `post-gc-hook`. At start, `gc-elapsed` and `gcs-done` are recorded. Each time the hook runs, the increase in `gc-elapsed` gives the duration of that collection, and an `X` event starts at "now minus duration".
- **Arguments.** `gcs-done`, and the collection's sequence number.
- **Watch for.** `post-gc-hook` must not allocate heavily, since that can trigger further collections. Events are written into positions preallocated in the ring buffer.

### 6. Redisplay (`redisplay`), modelled on a SurfaceFlinger trace

Emacs has no post-redisplay hook. The track therefore builds the closest thing Lisp can observe: one span per **frame**, the windows that frame repainted, the Lisp **phases** inside it, the fontification it caused, the input latency that led to it, and whether it missed its budget.

- **Frame span.** `pre-redisplay-functions` runs in each window just before that window is redisplayed. The first call of a redisplay opens a frame and schedules a zero-delay timer. The timer runs as soon as redisplay returns to the command loop and closes the frame. The `X` span `redisplay frame` therefore covers the Lisp hooks plus the C drawing in between. Windows that Emacs redisplays more than once in one pass are counted once.
- **Sampling.** A program that calls `redisplay` in a loop (an asynchronous completion UI, for example) can produce thousands of frames per second. Frames closer than `tuned-perf-trace-frame-min-interval` (default 8 ms, 125 Hz) are coalesced: their window markers, phase spans and font-lock spans are discarded and only the next recorded frame is written, with `coalesced` naming how many fast frames it stands for. A janky frame is always recorded, and the first frame after a gap is always recorded.
- **Frame arguments.**
  - `windows`: number of distinct windows repainted.
  - `fontified`: characters fontified by `jit-lock-function` during the frame.
  - `jank`: t when the frame lasted longer than `tuned-perf-trace-frame-budget`.
  - `interval-ms`: time since the previous recorded frame started.
  - `coalesced`: number of fast frames skipped since the previous recorded frame, when non-zero.
  - `key-latency-ms`, `command-latency-ms`, `after-command-ms`: time from the last recorded special key, from the command's `pre-command-hook`, and from the command's end.
  - `scrolled`, `resized`, `state-changed`, `buffer-changed`: which Lisp phases ran in this frame.
  - `buffer` and `mode` of the selected window (subject to `tuned-perf-trace-record-names`).
  - `cut`: t when `tuned-perf-trace-stop` closed a frame that was still open.
- **Jank event.** A janky frame writes an instant event named `^` at the frame start, on the redisplay track, with `frame-ms`, `budget-ms`, `windows`, `buffer` and `mode`. It is a marker, not a counter curve, and it is written even when the frame is closer than the sampling interval.
- **Frame counter.** When `counters` is enabled, each recorded frame also writes a `C` event `redisplay frame` with `ms`, `interval-ms`, `windows` and, when non-zero, `coalesced`. Perfetto draws these as a frame-time graph.
- **Per-window markers.** With `tuned-perf-trace-redisplay-detail` (default t), every redisplayed window gets one instant `redisplay <buffer>` per recorded frame, with `buffer`, `mode`, `selected`, `width` and `height`. This is the closest analogue of a SurfaceFlinger layer. Markers are held in memory until the frame is chosen for recording, so a coalesced frame writes none.
- **Lisp phase spans.** Also with detail, first and last functions bracket the global value of each redisplay hook: `window-state-change-functions`, `window-size-change-functions`, `window-buffer-change-functions`, `window-scroll-functions` and `window-state-change-hook`. Buffer-local values, if a buffer has any, run before the bracket, so the span covers the global part; the phase flag still says that the hook ran. The resulting spans are named after the hook and carry the buffer and mode when a window is known. They are kept until the frame is recorded, so a coalesced frame writes none. They are the parts of a redisplay that Lisp can time; the C layout and drawing remain the gap inside the frame.
- **Fontification.** `:around` advice on `jit-lock-function` records one `X` event per fontification, with buffer, file, mode and character count. The span is held until the frame is recorded, and the character counts are added to the open frame's `fontified`.
- **Summary.** The trace metadata records `redisplay-frames` (all frames seen), `redisplay-recorded-frames`, `redisplay-coalesced-frames`, `redisplay-jank-frames` and `redisplay-max-frame-ms`.
- **Watch for.** A redisplay triggered inside a command (an explicit `(redisplay)`, for example) is measured until the command loop resumes, so it may include the rest of that command. The frame end is the command loop, not the compositor: there is no vsync or presentation feedback in Lisp.

### 7. Functions the user names (`functions`)

- **Mechanism.** `:around` advice named `tuned-perf--function-advice`, so it can be removed precisely.
- **Refusing C primitives.** If `(subr-primitive-p (indirect-function fn))` is true, tracing is refused by default, with the reason (trampolines). The exception: this Emacs has no native compilation (`native-comp-available-p` is nil), and then tracing is allowed.
- **Refusing the tracer's own functions** (prefix `tuned-perf-`).
- **Non-local exits.** The advice body uses `unwind-protect`, so the span ends on error, and the error propagates as usual.

### 8. Marks (`marks`)

- `tuned-perf-trace-mark` produces an `i` instant event whose name is the text the user typed.

### 9. Major-mode changes (`modes`)

- **Mechanism.** The hooks `change-major-mode-hook` and `after-change-major-mode-hook`, not advice, so every switch is timed: a file opened, a mode called directly (`org-mode`), or a mode switched in a buffer.
- **Name.** `mode <new mode>`, with `from` and `to` as arguments.

### 10. Completion (`completion`)

- **Mechanism.** `:around` advice on three functions (Lisp):
  - `completion-at-point`: one call runs every function in `completion-at-point-functions` (cape, ispell, ...). Corfu's own popup does not go through it.
  - `vertico--update`: the minibuffer candidate list, on each keystroke.
  - `corfu--update`: the in-buffer popup, on each keystroke (when corfu is loaded).
- **Name.** `completion-at-point`, `vertico--update`, `corfu--update`.
- **Arguments.** `candidates` (the count only, never the text) and the buffer and mode, unless names are off.

### 11. Chinese input (`input`)

- **Mechanism.** `:around` advice on four pyim functions (Lisp). A composition enters `pyim-input-method`, which reads keys until the text is committed; each key rebuilds the candidate list in `pyim-process-run`, which redraws the popup through `pyim-page--refresh`, and the commit runs `pyim-select-word`. pyim is loaded when recording starts if it was not loaded yet.
- **Name.** `pyim-input-method` (one span per composition), `pyim-process-run` (per key), `pyim-page--refresh` (the popup), `pyim-select-word` (the insert and the `after-change` hooks that follow).
- **Arguments.** `candidates` (the count only) on `pyim-process-run`, and the buffer and mode on the composition span unless names are off. The pinyin and the text are never recorded.
- **Watch for.** This fills a gap the command track cannot see: while the input method reads keys, no command runs, so without this category typing Chinese looks like idle time between commands. A slow composition shows in `pyim-process-run` (dictionary lookup and candidate generation), in `pyim-page--refresh` (posframe drawing), or in `pyim-select-word` (the buffer's `after-change` hooks, for example Org's element cache).

### 12. Files (`files`)

- **Mechanism.** `:around` advice on `find-file-noselect` and `save-buffer` (both Lisp). The save hooks `before-save-hook` and `after-save-hook` are timed separately, so a slow format-on-save shows as its own span.
- **Name.** `find-file`, `save-buffer`, `before-save-hook`, `after-save-hook`.

### 13. Counters (`counters`)

- The only counter is the redisplay frame counter described in item 6: one `C` event per recorded frame with `ms`, `interval-ms`, `windows` and, when non-zero, `coalesced`.
- Memory and object counters are not recorded. The first version sampled `memory-use-counts` after each GC and once a second, but those curves did not answer the questions the trace is used for, so they were removed.

## Time, buffer and re-entry guard

- **Ring buffer.** A vector of length `tuned-perf-trace-ring-size`, with a write pointer and a count of overwritten events. When full, the oldest events are overwritten, and the overwrite count is written as `dropped-events` in the metadata.
- **In-memory event form.** A small vector `[PH NAME CAT TS DUR TID ARGS ID]`, where `ARGS` is a plist or nil. No JSON is built while recording; conversion happens at stop.
- **Re-entry guard.** A dynamic variable `tuned-perf--inside`. When it is non-nil, all instrumentation calls the original function directly and records nothing. Recording binds it to t.
- **Start time.** `tuned-perf--t0`, in integer microseconds.

## Probes

The recorder keeps the ring, the JSON writer, the recording state and a probe registry. `tuned-perf-trace.el` holds the probes for Emacs's own machinery: commands, timers, processes, GC, redisplay, files and modes. Probes for packages outside Emacs core (`tuned-async`, vertico, corfu and pyim) live in `tuned-perf-probes.el`, which the recorder loads when a recording starts. The core still records if that file is absent, and a package that loads during a recording installs its probe at that moment.

Each probe registers itself with `tuned-perf-define-probe` (feature, category, install): built-in probes use the always-present `emacs` feature, and an optional package installs only when it is loaded. The repeated span advice is generated by `tuned-perf-define-span-advice`, so the timing shape exists once. The macro names the target in each generated docstring and accepts one extra sentence for advice that has something specific to say.

## Lifecycle

**Start** (inside `condition-case`: if any step fails, the steps already done are rolled back and the error is reported):

1. Allocate the ring buffer, and record `t0`, `gc-elapsed` and `gcs-done`.
2. Write the `M` events that name the tracks.
3. Install instrumentation in category order: hooks, advice, wrapping existing processes, and starting the scan timer. Each installed item is recorded in an "installed" list.
4. Start the timer that stops recording automatically.
5. Turn on the mode-line indicator.

**Stop** (idempotent, inside `unwind-protect`, so instrumentation is always removed):

1. Close a redisplay frame that is still open, marking it `cut`.
2. Remove everything in the "installed" list, in reverse order: hooks, advice, process-function wrappers and timers.
3. Add an `e` event for each unfinished tuned-async id.
4. Turn off the mode-line indicator.
5. Convert the buffer to JSON (`json-serialize`) and write the file.
6. Show the path and the event count in the echo area, and return the path.

**Other cases.**

- If Emacs exits while recording, nothing is done and the buffer is discarded. No `kill-emacs-hook` is installed, since recording is meant to be temporary.
- Calling start during a recording only reports it; it does not restart.

## Writing the file

- File name: `emacs-trace-<YYYYMMDD-HHMMSS>.json`, in `tuned-perf-trace-directory`.
- Method: write a temporary file and rename it, so a partly written file is never seen.
- Size: 100,000 events are about 15 to 25 MB, which Perfetto UI opens easily. A 120 s recording at the default redisplay sampling rate holds at most about 15,000 frame events, so it stays well under that cap.
- Opening: ui.perfetto.dev parses the file locally in the browser, so the file is not uploaded. The offline build of Perfetto also works without a network.

## Following from another process

When the main Emacs starts recording, it writes a marker file `traces/recording.eld` (containing the recording directory, the start time and the limit), and deletes it at stop. A separate Emacs process can follow it: between units of work it calls `tuned-perf-follow-marker` with a file prefix. While the marker is present and not expired, that process starts its own recording in the same directory, with file names beginning with the prefix. When the marker disappears or expires, it stops and writes its file. The main Emacs needs no knowledge of the follower, and uses no hooks, only direct calls. The background native compiler is such a follower: see [tuned-eln-opt.md](tuned-eln-opt.md).

- The marker is part of the recording: `tuned-perf--start` writes it and `tuned-perf--stop` deletes it. Writing and deleting never make a recording fail.
- An expired marker is treated as stale, so a crashed main Emacs does not leave a permanent marker.
- While a follower is busy with one unit of work, it does not check the marker; it waits until that unit finishes. A recording that starts late therefore misses the beginning.
- The recording and follower tests bind `tuned-perf-marker-file` to a temporary file, so they never touch the real marker.

## Performance budget

- Target: no more than 5 µs per recorded event (Apple Silicon). Typing should not feel any difference.
- Method: trace an empty function, call it 100,000 times, and compare the time with and without tracing. The command `tuned-perf-trace-benchmark` was removed (it existed only for one-off measurement, so it is not kept). Tests do not assert timings, to avoid flakiness on slow machines.
- Measured (2026-11-25, Emacs 31.1 `aarch64-apple-darwin23.6.0`, `emacs --batch`, 100,000 calls, three runs): **about 3.2 µs per call when byte-compiled**, which meets the 5 µs target. Loading the `.el` source directly costs about 7.1 µs per call, because the recording path itself runs interpreted. A normal install compiles through `compile-lisp` into `.elc`, so 3.2 µs is the figure for run time. Commands and timer events are recorded once each, so this figure is not multiplied for them, and it is not noticeable when typing. The test only prints this number and does not assert it.
- Redisplay instrumentation costs one zero-delay timer per redisplay, plus one frame span and (with `counters`) one counter event per **recorded** frame. Window markers, phase spans and font-lock spans are held in memory and written only for recorded frames. Sampling at `tuned-perf-trace-frame-min-interval` bounds the event rate, so a redisplay loop cannot fill the ring buffer in seconds.

## Privacy and security

- Not recorded: key sequences, buffer text, minibuffer input, process output, environment variables.
- Recorded: function names, command names, timer function names, process names, exit codes, byte counts, and buffer and file names (can be turned off).
- Trace files are written locally only. Before sharing a trace, remember that it contains buffer and file names. Set `tuned-perf-trace-record-names` to nil if needed.
- Stopping always removes all instrumentation, so no permanent tracing advice is left behind.

## Files and integration

- New file: `tuned-emacs/lisp/tuned-perf-trace.el`. Its header follows `tuned-emacs/lisp/tuned-dired.el` (Author, Version, `Package-Requires: ((emacs "30.1") (tuned-async "0.1.0"))`, Commentary). tuned-async is an optional dependency: when it is not loaded, item 4 is skipped.
- New test: `tuned-emacs/tests/tuned-perf-trace-tests.el`, run on its own with `emacs --batch -Q -L tuned-emacs/lisp`, in the style of `tuned-emacs/tests/tuned-dired-tests.el`.
- `tuned-emacs/install/emacs-setup.sh`: `cmd_test` runs this test file.
- `tuned-emacs/tests/smoke-tests.el`: the file list in `tuned-personal-file-is-framework-neutral` includes `tuned-emacs/lisp/tuned-perf-trace.el`.
- `tuned-emacs/init.el`: one line, `(require 'tuned-perf-trace)`. It installs nothing until recording starts, so its load cost is small.
- Key bindings: not decided in the first version; left to the user.

## Test plan

ERT. Every test runs in a temporary directory and confirms at the end that no instrumentation remains.

1. **No trace after start and stop.** `pre-command-hook`, `post-command-hook`, `post-gc-hook`, `pre-redisplay-functions` and the five window change hooks are restored exactly; `timer-event-handler`, `jit-lock-function` and `tuned-async-run` carry no tracer advice; no tracer timers remain; each process's filter and sentinel are `eq` to their values before start.
2. **Commands.** Simulate pre and post with `run-hooks`, bind `this-command` to a symbol, and get an `X` event with the right name and a non-negative `dur`. No key appears in the arguments.
3. **Timers.** Run a named function with `run-at-time 0`, wait for it, and get the matching event with `idle` nil. An idle timer gets `idle` t.
4. **GC.** Call `garbage-collect`, and get a `gc` event with a non-negative `dur`.
5. **Processes.** Start a process with a filter before recording (for example `printf`, or Emacs in batch). While recording it produces output, and a `filter` event appears with only the byte count in its arguments. After stop, the filter is `eq` to the original. A process created during recording is wrapped after one scan.
6. **tuned-async.** Recording `tuned-async-run` of a short command gives `b` and `e` with the same id. A task cancelled with `tuned-async-cancel` receives an `e` at stop.
7. **User functions.** Trace a function that signals an error: the span still ends and the error propagates. Tracing a C primitive is refused (on a native-compiled Emacs without a cached trampoline). Tracing a function with the `tuned-perf-` prefix is refused.
8. **Re-entry guard.** Inside a traced function, call another traced boundary: no infinite recursion, and a reasonable event count.
9. **Ring buffer.** Set the size to 10, record 25 events, keep the last 10, and get `dropped-events` 15.
10. **Automatic stop.** Set a very short maximum, wait, and check that recording has stopped, the file was written, and the instrumentation was removed.
11. **Output file.** `json-parse-string` succeeds; there is a `traceEvents` key; every event has `ph`, `ts` and `pid`; `dur` on `X` events is non-negative; the `M` track-name events exist; the metadata has `emacs-version` and `dropped-events`.
12. **Rollback on a failed start.** Simulate an error partway through installation: instrumentation already installed is removed, and the state is not recording.
13. **Name recording switch.** With `tuned-perf-trace-record-names` nil, events contain no buffer or file names.
14. **Chinese input.** With pyim (or a stub) present, the four entry points carry the advice while recording; calling them writes `input` events with the candidate count, and stopping removes the advice.
15. **Redisplay frame.** Run `pre-redisplay-functions`, let the zero-delay timer close the frame, and get an `X` event `redisplay frame` with `windows` and `jank`, plus its `C` counter event. Stopping while a frame is open closes it once, with `cut` set.
16. **Window markers.** With detail on, a window is marked once even when its redisplay hook runs twice in one frame; with detail off, no window marker and no phase span is written.
17. **Redisplay phases.** Running `window-size-change-functions` and `window-scroll-functions` gives spans named after the hooks, and the frame says `resized` and `scrolled`.
18. **Jank.** A frame stretched past `tuned-perf-trace-frame-budget` has `jank` t, writes a `^` instant event whose arguments give the frame time and budget, and the metadata counts it. The frame counter carries no `jank` field.
19. **Latency.** After a key, a command and a post-command, the next frame carries `key-latency-ms`, `command-latency-ms` and `after-command-ms`.
20. **No memory counters.** A GC and a scan write no `memory` or `counters` event.
21. **Coalescing.** With a 50 ms minimum interval, three fast frames are merged into the next recorded one; their window markers and phase spans are discarded; the recorded frame says `coalesced 3`; and the metadata counts recorded and coalesced frames.

## Acceptance criteria

- `bash tuned-emacs/install/emacs-setup.sh test` passes completely, including the new test.
- In a real GUI Emacs, record 10 seconds while typing, switching buffers and opening an tuned-dired preview. The file opens in ui.perfetto.dev, and shows tracks for commands, timers, GC, tuned-async, redisplay frames with jank, and font-lock.
- Not recording: no tracer function remains in the global hooks, and `advice-member-p` is nil for every tracer advice.

## Possible future work

- Merge external samplers: on Linux, put the output of `perf script` and the Elisp events in one trace (the clocks must be aligned).
- Add `tuned-perf-with-span` to the critical paths of tuned-dired.
- Produce flame graphs from Emacs's built-in `profiler`. It has no timestamps, so it gives call trees only, not a timeline.
- An on-demand snapshot: one command records for 10 seconds, stops by itself and opens the file.
- Label why a frame happened from the deltas of `redisplay--all-windows-cause` and `redisplay--mode-lines-cause`. Both are internal, version-dependent cause codes, so the first version records only which Lisp phases ran.

## Known limitations

- The C layer is not visible. A frame span measures from the first Lisp redisplay hook until the command loop resumes, not the compositor's presentation or a vsync; the individual C phases (layout, drawing, bidi) still cannot be timed. Key-reading latency is still inferred from the last recorded key and the command hooks.
- Frames closer than `tuned-perf-trace-frame-min-interval` are coalesced. Their individual durations are not in the trace; the next recorded frame says how many were merged. Set the interval to 0 to see every redisplay, at the risk of filling the ring buffer during a redisplay loop.
- For processes that are not tuned-async, output in the first `tuned-perf-trace-process-scan-interval` seconds after creation may be missed.
- During a recursive edit (for example, running a command inside the minibuffer), the span of the outer command is discarded rather than shown nested.
