# eln-opt: background compilation in tiers

Status: implemented and in use. The implementation is `tuned-emacs/lisp/eln-opt.el`; the tests are `tuned-emacs/tests/eln-opt-tests.el`.
Numbers come from Emacs 31.1 on a Mac (Emacs.app) and Emacs 30.1 on a Raspberry Pi 4 (4 cores, 3.8 GB).

## Goal

Emacs code runs in one of three states: interpreted source, byte code, or native code (eln). eln-opt moves frequently used code to the faster states in the background, without the user noticing.

- **Transparent.** The running Emacs adds no hooks, no advice and no analysis. It has one one-shot idle timer. Analysis and compilation run in a background child process.
- **Light.** Startup cost is small. When idle, only a fingerprint comparison runs. Real work happens only when the fingerprint changes.
- **Background, low priority.** One file at a time, spaced out so it never competes with the session.
- **Safe.** It never loads untrusted code, never recompiles repeatedly because of a version mismatch, resumes after an interruption, and can be turned off.
- **Effective next start.** New results take effect on the next start, not in the running session.

Not in scope: changing Emacs's C code; depending on external programs; replacing Emacs's own compilation machinery. eln-opt fills the gaps that Emacs does not cover.

## Three tiers

| Tier | State | Who promotes it |
|---|---|---|
| 0 | Interpreted source (no `.elc`, or `.elc` older than the source) | The goal is to leave this tier |
| 1 | Byte code `.elc` | The worker's `byte-compile`, on every platform |
| 2 | Native `.eln` | The worker's native compile, only on supported platforms, only for hot files |

The tier 2 threshold should follow measured gains, not only heat. Loop-heavy pure Lisp is typically 1 to 3 times faster natively. Code that mostly calls C primitives or allocates gains little: `xl-git-status--entries` is 2.9 times faster as byte code than interpreted, but native adds only another 1.1 times over byte code. So low-scoring, allocation-heavy files are not worth compiling natively.

## User commands

| Command | Effect |
|---|---|
| `M-x eln-opt-compile-now` | Start the worker now, bypassing idle and quiet checks. Shows `*eln-opt*`. |
| `M-x eln-opt-report` | Show the ranking, the next files to compile, files already at tier 1 or 2, and why some files were not compiled. |
| `M-x eln-opt-stop-worker` | Stop the running worker. A file being compiled keeps its in-progress marker, so the next run treats it as interrupted. No automatic start for the rest of the session; `eln-opt-compile-now` still works. |
| `M-x eln-opt-reset` | Delete the elns eln-opt made, the failure list, in-progress markers and the quiet state. Asks for `yes` first. |

Report details: while the child process is still working, the report shows only that. Within 60 seconds, with an unchanged fingerprint, the last result is shown again (`eln-opt-report-cache-seconds`). A manual start is refused for 30 seconds after a worker finishes (`eln-opt-manual-gap`).

Command line, through `install/emacs-setup.sh`: `eln-opt` (platform check), `eln-opt-compile`, `eln-opt-report`, `eln-opt-status`, `eln-opt-reset`, and `compile-lisp` (our own Lisp to byte code, no libgccjit needed).

## From startup to compilation

At startup, after the requires in `init.el`:

1. `eln-opt.el` loads in about 20 ms (byte code, Pi). `comp.el` is **not** loaded here. It is loaded only when needed by the worker, trampoline builds or reset.
2. `eln-opt-auto-arm` installs one idle timer of `eln-opt-auto-idle` seconds (default 5). There is no polling and no analysis. In batch runs (setup, tests, the worker itself) nothing is installed.

When the idle timer fires (after the session has been idle that long):

10a. If the last run compiled something (it reported `done`), the idle timer is armed again, so the next idle period continues the work. At most `eln-opt-auto-max-runs` (60) runs per session. With no new work, the session is marked quiet and no further runs start.

3. The start decision (`eln-opt-auto--decision`, about 0.1 ms) returns a reason for not starting, or nothing. `ELN_OPT=0` disables everything; `eln-opt-auto-enable` set to nil disables it; a worker already running blocks a second one; fewer than `eln-opt-min-processors` (2) logical cores disables automatic start, because on one core the worker and the session compete for it (single-core users can run `eln-opt-compile-now` by hand); and the **quiet state**: the last worker found nothing to do and nothing has changed.
4. The fingerprint (`eln-opt--quiet-fingerprint`, about 1.5 ms) covers the Emacs binary, `load-path`, and the modification times of our own Lisp files. It does not include usage data (savehist, recentf).
5. The worker starts: the same Emacs binary and profile, under `taskpolicy -b` (macOS) or `nice -n 19`. A watchdog checks the output every 60 seconds.

In the worker (a background batch Emacs):

6. Take the lock (`owner.eld` records the pid and time). A lock whose pid is gone, or that is older than an hour, is stale and is taken over. If the lock cannot be taken, print `busy` and exit.
7. Read any in-progress markers left behind (see "Safety").
8. Platform check, cached in `platform.eld`, keyed by the binary's path, size and mtime.
9. Build the plan:
   - Byte-compile candidates: files loaded from source that have no current `.elc`. Lazily loaded roots (for example `org`) are loaded first, so the call graph is complete.
   - Native candidates, when the platform supports it: a full analysis (about 6 s on the Pi), sorted by score (expected gain per KB of byte code), taking files with ratio ≥ 3 that have no eln.
   - At most 30 items in total, byte-compiles first. An empty plan marks the session quiet and exits.
10. Process the plan one item at a time, sleeping `eln-opt-worker-sleep` (1 s) between files: write the in-progress marker, compile, delete the marker, and after a successful native compile warm the new eln (see "First-load check"). Print `done` or `failed`, and record a failure in the failure list. A failed file does not stop the others.
10b. On a platform with native compilation, warm the whole eln directory once (see "First-load check"). This also covers elns made by other means and elns the worker was stopped before warming.
10c. Build the predicted trampolines that are still missing, at most `eln-opt-trampoline-limit` (50) per run, and print how many were built. This is the only place eln-opt builds trampolines during normal use.
11. Finish: print `finished N` (or `finished N, failed M`) and release the lock. The main Emacs shows `compiled N file(s); they load on the next start`.
12. Next start: Emacs loads the new `.elc` and eln files by itself.

Rough time on the Pi: each file takes seconds to compile (org takes about a minute), plus the spacing between files (1 second by default). Thirty files take a few minutes.

## Candidate rules

A file enters the plan only if all of these hold:

- **Trusted.** `file-truename` is under a trusted root: elpa, Emacs's `lisp-directory`, `tuned-emacs/lisp`, or `tuned-emacs/xueliang.el`. A symbolic link pointing outside a root is rejected.
- **Has source.** A `.el`, or for tier 2 an Emacs built-in library's `.el.gz`.
- **Defines functions.** Not an autoload or loaddefs file (which only contains stubs), and not `init.el`, `early-init.el` or `custom.el` (a stale `init.elc` is a common trap).
- **No opt-out.** The file does not contain `no-native-compile: t` or `no-byte-compile: t`.
- **Not in the failure list** (forced compiles excepted).
- Tier 2 also requires score ≥ `eln-opt-min-score` (3.0), compared at the one decimal the report shows and unpacked source ≤ `eln-opt-max-source-bytes` (1 MB).

## Ranking (reachability)

The analysis reads data only. It runs none of the code and compiles nothing.

**Roots.** Commands the user actually ran (from savehist, with a capped weight per command), common major modes (recentf files mapped through `auto-mode-alist`), the hooks and keymap commands of common modes, hooks that run on every key or redisplay (`post-command-hook` and similar, the highest weight), and global and leader keymaps (very low weight). A mode's keymap and hooks count as roots only when the mode is in the common set, which prunes many rarely used mode settings. Important lazily loaded roots (weight ≥ 0.5) are loaded first, or their functions would be missing from the graph.

**Cold-start baseline.** On a new machine, savehist and recentf are empty, so the ranking has no evidence. `tuned-emacs/data/eln-opt-baseline.eld` lists function names that are generally worth handling (evil and winum hooks that run on every key, the completion stack, and known trampoline primitives). Each is a root with weight 1.0. As the user's own data grows, the baseline's weight shrinks in proportion, and it stops mattering entirely once the user has `eln-opt-rank-baseline-users` (50) commands and files. The file holds function and primitive names only, no code and no paths.

**Edges.** Function symbols in a function's constant vector (recursively for nested closures). When a function reads a variable and calls `funcall` or `apply`, and the variable's value is a function (for example `completion-at-point-functions`), an edge is added. Functions with advice are connected both ways. What cannot be seen (lambdas and timers created at run time, process filters and sentinels) is not counted.

**Scoring.**

- Heat propagates along edges: `R(callee) += 0.5 · R(caller) / sqrt(out-degree)`, for at most three levels (`eln-opt-rank-heat-levels`).
- Benefit B is computed from byte code (LAP). Loop bodies are found from backward jumps. Instructions inside a loop are weighted by `4^depth` (recursion counts as one more level). Inline operations (`car`, `cdr`, arithmetic, `eq`, `aref` and so on) add to the score. Buffer operations, heavy C work (`insert`, `re-search-*`, `string-match` and so on) and allocating calls (`substring`, `concat`, `format`, `split-string` and so on) subtract.
- File gain = Σ R · B (internal). Score = gain ÷ byte-code size in KB. The score is the only number eln-opt shows, and it is the one compared with `eln-opt-min-score`.
- Functions that are already native are skipped. Ownership uses `eln-opt--defun-files`, a table built in one pass over `load-history`, which agrees with `symbol-file` for all native functions.

The coefficients (4.0, 0.5 and the per-opcode values) are empirical. They have not been calibrated offline, which would need a profiler run on the user's machine.

## Platforms

- The only test is `native-comp-available-p`; the operating system name is not used. Where it is false (a Windows build without libgccjit, for example), the worker still runs and does byte compilation only. The report still shows the same Status table, with tier 2 at 0.
- Native compilation can be present but broken (wrong libgccjit version, or a driver or linker error). A one-function canary file checks it once, and the result is cached. Failure is recorded as unavailable until the binary changes.
- `comp.el` is loaded only when needed. `comp-native-version-dir`, `native-comp-eln-load-path` and `comp-el-to-eln-filename` are already preloaded under `-Q`, so the startup path does not need `comp.el`.
- Without `comp` (some Windows builds), the state directory name is derived from the binary's key, not from `comp-native-version-dir`, which is undefined in that case.

## When Emacs uses an eln (measured)

| Situation | Is the eln used? |
|---|---|
| eln exists, loaded through a real path or a symbolic link | Yes |
| Source content changed (the content hash in the file name changed) | No. The next compile removes the old eln at the same path. |
| `.elc` is newer than `.eln` | **No, and nothing warns.** |
| Only `.elc`, no `.el` | No (the `.el.gz` case below is an exception) |
| Only `.el.gz` (Emacs built-in libraries) | Yes: `native-compile` on a `.el.gz` produces the eln name Emacs looks for |
| eln written to a different eln-cache (`-Q` writes to `~/.emacs.d/eln-cache` by default) | Never |
| eln damaged (random bytes, truncated, empty) | `require` fails with `native-lisp-load-failed`; it does not fall back to `.elc` |
| Name matches but content came from different source | Loaded silently |
| `load-no-native` is set | All elns are skipped and `.elc` is used; preloaded core code stays native |

An eln file name is `<name>-<path-hash>-<content-hash>.eln`, under `eln-cache/<comp-native-version-dir>/`. The version directory depends on the binary build (Emacs.app has `31_1-032f33bc` and `31_1-38fe94a6`). comp writes a temporary file and renames it, so a running Emacs never sees a partial file.

eln-opt writes only through comp (`native-compile`, `comp-trampoline-compile`, `comp-delete-or-replace-file`). It never copies or overwrites files in place.

**Built-in `.el.gz` libraries.** Emacs's own org, winner, which-key, savehist, repeat, delsel and others ship only as `.el.gz`. Calling `native-compile` on the `.el.gz` produces the eln Emacs looks for, without changing the startup path. Loading a `.elc` by its full path always loads byte code. Only a load without a suffix, which is how `require` works, looks for an eln.

## Feature analysis cost

The call graph needs each function's features (about 5000 functions). The current code does not cache them: each run reads the source files and recomputes the graph. The graph timings in the Performance section describe this uncached analysis. An earlier design proposed a per-file feature cache; it is not in the current code, and a persistent cache is listed under "Open questions".

## Recording (perfetto)

When the main Emacs records with `xl-perfetto-trace-start`, the worker checks for the marker file `traces/recording.eld` between files. If it is there, the worker records as well and writes `eln-opt-trace-*.json`. Each worker phase is a span: the reachability steps, the candidates, each file's compile, each top-level form, and warming. See [xl-perfetto-trace.md](xl-perfetto-trace.md), section "Following the eln-opt worker".

## First-load check (macOS)

On macOS, the first time any process maps a new native file, the system checks it. A new `.eln` costs about 0.22 s, almost regardless of size (37 KB and 410 KB both cost this). The result is cached per file, across processes. A later load takes about 3 ms. The cache is per file, not per content: copying an already-checked eln to a new path makes the first load cost 0.22 s again.

The consequence: after the worker compiles a batch of elns, the next GUI start is the first process to load them, and the cost falls on the main thread.

Measured on a MacBook Air, a normal start loading 31 files from the user's eln directory:

| Situation | GUI init |
|---|---|
| All already checked | 1.19 s |
| 31 new files (new copies with the same content) | 7.48 s |
| New files, first loaded by a background `emacs -Q` | 0.90 s |

The fix (`eln-opt--warm`): after the worker makes each eln, a one-shot `emacs -Q --batch` loads it once. At the end of the run the whole directory is warmed once more. In the child, `load-path` is nil and the file's top-level code stops at the first `require`. The purpose is only to trigger the system check, not to run the code. stdin is closed at once, and the child is stopped after `eln-opt-warm-timeout` (120 s). This is macOS only. The worker runs under `taskpolicy -b`, and at background priority the system check is much slower (31 files take about 8 s, against about 0.4 s in the foreground), but it does not occupy the main thread.

Correction: the 13-second starts that prompted this work were first attributed to the first-load check. The slow-start log later showed that 12.1 s was spent in the `gcc`, `clang` and `g++` runs that ffap makes at load time, which has nothing to do with elns. The cause and fix are in [startup.md](startup.md). The first-load cost described here is real and was reproduced with new copies, but it was not the cause of those slow starts.

Trampolines compiled by `comp-trampoline-compile` are loaded immediately, and were already checked by the process that compiled them, so they need no warming.

**Who builds them.** The worker builds the predicted trampolines after it has compiled its files (step 10c), at most `eln-opt-trampoline-limit` per run. This happens in the background worker only; the running session never compiles a trampoline for eln-opt. The setup script and the `trampolines` command build the same set on demand.

## Trampolines

Advising or redefining a C primitive needs a trampoline (`subr--trampoline-*`). Without a cached one, the first use compiles it synchronously on the main thread, for about one second, and the interface freezes. So they are built ahead of time.

- **Prediction.** The `.elc` files are read as data, not loaded. The byte code is searched for `advice-add` or `fset` constants followed by a primitive constant, and for the **`byte-fset` opcode** that `cl-letf` on `symbol-function` compiles to (found during implementation: look at the three constants before it). Top-level `(byte-code ...)` forms in `.elc` files are handled the same way. A primitive that is already advised is unwrapped with `advice--cd*r` before it is checked.
- **Supplement.** Primitives advised after init (the static scan misses cases such as `set-window-buffer`).
- **Err on the side of extra.** A missed trampoline means a freeze, while an extra one costs one batch compile. At most 50 per run (`eln-opt-trampoline-limit`).
- The static scan sees only what is in `.elc` files. Advice added at run time needs the post-init supplement, for example `buffer-name` in `magit-files.el`, `message` in `magit-git.el`, and `region-beginning` and `region-end` in `rect.el`.

## Safety

**Trust boundary.** Emacs trusts an eln by its file name and does not verify its content. So eln-opt makes sure that untrusted input never becomes code that will be loaded.

- Only files under trusted roots are compiled (see "Candidate rules"). Compiling runs macros, but these files were already byte-compiled once when they were installed, so native compilation does not introduce new code execution.
- savehist and recentf are used only as weights: read with `read`, never `eval`'d. Command names are looked up with `intern-soft` and never called. recentf paths never become compile targets.
- Data stays on the machine. Reports and status show only file names, function names and primitive names. savehist may contain `kill-ring` (clipboard content). The analysis takes only the `extended-command-history` data and parses nothing else.

**In-progress markers** (`in-progress.eld`). Before a compile, the file, attempt count and time are written; they are deleted after the compile. If a marker is still present when a worker starts holding the lock, the previous compile was interrupted (quit, crash, power loss). It is retried while the attempt count is below `eln-opt-interrupt-limit` (2). At the limit it goes into the failure list, until the source changes.

- Markers are read only while the lock is held. Their path must pass the trust check, and they are used only to record failures. If a marker cannot be written, nothing is compiled. If the marker or its version directory is a symbolic link, the worker refuses to run; it neither reads nor deletes the marker, and the user must remove it by hand.
- A user quitting Emacs in the middle of a compile counts as an attempt. There is no `kill-emacs-hook`, so this is not distinguished.

**Failure list** (`failed.eld`). Keyed by the source's truename and content MD5. A failed file is skipped until its source changes. Without this, one bad file would stop every run at the same place. Forced compiles ignore it.

**Lock.** Only one worker may run at a time. A lock is taken over only when its pid is gone or the lock is over an hour old.

**Watchdog.** The worker prints a line whenever it starts a file. If the main Emacs receives no output for 600 seconds, it kills the worker and records the current file in the failure list.

**Emergency switch.** With `ELN_OPT=0 emacs`, `early-init.el` sets `load-no-native`, so every eln is skipped and `.elc` is used. `ELN_OPT=0` also overrides forced compiles.

**What this cannot protect against.** If an attacker has taken over the same user account and can write to eln-cache, they can equally write to elpa and the init files. An eln has the same trust level as a `.elc`. What eln-opt guarantees is narrower: the pipeline itself does not widen the attack surface, and problems can be detected, isolated and switched off.

## Performance

**Full analysis on the Pi** (`eln-opt-rank--compute`):

| Stage | Before | After |
|---|---|---|
| Full analysis | 17.9 s | 6.3 s |
| Call graph | 8.2 s | 4.8 s |
| Native list | 7.9 s | 0.56 s |
| Layer table | 8.1 s | 0.72 s |
| Report formatting | 27 ms | 27 ms |

The changes: the native list uses a hash table built in one pass, instead of calling `symbol-file` for each function (which scanned all of `load-history` every time). The opt-out check per file in graph building is done once. Instruction classification uses hash tables. Loop-weight powers are cached. Results and report text are identical to the earlier version, item by item.

**Startup cost (Pi).**

| Item | Time |
|---|---|
| Loading eln-opt (byte code) | About 20 ms (it was 180 ms, from a top-level `require comp`) |
| Fingerprint check when idle | About 1.5 ms |
| Deciding | About 0.1 ms |

**Main remaining cost.** Feature extraction for byte-code functions takes about 3.5 s per run (about 360 µs per function, the per-instruction overhead of Emacs Lisp; micro-tuning does not help). A real reduction needs per-function analysis results cached by file modification time, which would benefit both the report and the worker. Making the results match exactly is harder, because the call lists depend on which functions are currently defined, so this is a separate step.

## Known limitations

- **Usage changes do not trigger a run.** The fingerprint does not include savehist or recentf, so a new hot spot that appears only through usage does not start a run by itself. A run happens when the profile, a package or eln-opt changes, or when `eln-opt-compile-now` is run. This is deliberate; there is no time-based fallback.
- **The analysis counts itself.** eln-opt's own files are part of the call graph, so `eln-opt` often ranks near the top. To exclude them, the candidate rules would need an exception.
- **Single-core machines.** Automatic start is off by default (`eln-opt-min-processors`).
- **Windows and Linux.** Only unit tests and stubs (for `boundp` and `executable-find`) have run. Nothing has run on the real machines.
- **GUI idle timer.** It was exercised in batch mode by calling `eln-opt-auto--start` by hand. Confirming that the timer fires in a GUI still needs a manual check.
- **Crash isolation** is not implemented. Only the failure list and the in-progress markers prevent repeated failures.

## Tests

`tuned-emacs/tests/eln-opt-tests.el` is an ERT suite. All files are written to temporary folders, and `cl-letf` replaces the platform check, compilation, sleeping and similar functions. It covers: the platform gate and canary; trampoline prediction and supplementing; the lock; the ledger and the failure list; in-progress markers (an interrupted attempt is retried, two failures stop it, an untrusted path, a marker that cannot be written, a symbolic link); candidate rules (trusted roots, opt-outs, size, score); the worker's order and stopping at the first failure; forced compiles; report formatting; the reset confirmation; and that the `load-history` mapping agrees with `symbol-file`.

`bash tuned-emacs/install/emacs-setup.sh test` runs the smoke, eln-opt, xl-dired, perfetto and xl-package-fetch suites.

## Probes and experiment scripts

`tuned-emacs/doc/eln-opt-probes/` holds the early experiment scripts. Their README explains how to run them and the known gaps: ranking, trampoline prediction, the eln version and loading rules, and timing comparisons between interpreted, byte-compiled and native code. They are a historical record and are not part of the build.

## Open questions

- **Tier 2 threshold.** `eln-opt-min-score` and the scores are empirical and need offline profiling to calibrate.
- **Persistent cache.** Caching each function's analysis by file modification time would bring the Pi analysis down to about one or two seconds. Not implemented.
- **Gains from Emacs's built-in libraries.** The native gain from `.el.gz` libraries such as org still needs measuring in everyday use.
- **Linux scheduling.** The `ionice` path and the inotify-related parts are untested.
