# xl-eln-opt: background native compilation of what you use

Status: implemented and in use. Code: `lisp/xl-eln-opt.el`. There is no ERT suite for this module yet.

Emacs can native-compile your Lisp in two ways, and both are poor fits for a personal setup. Compiling everything that loads spends time and disk on code you rarely run. Letting the JIT compile on first use blocks the main thread while it works. `xl-eln-opt` takes a third path: a child Emacs compiles, while you are idle, only the files behind the commands, modes and minor modes you actually use.

## When it runs

- `xl-eln-opt-arm` (called from `init.el`) starts one idle timer of `xl-eln-opt-idle` seconds (default 5.0). It does nothing in batch, or when `xl-eln-opt-mode` is 0.
- The timer fires once per session. If no child from an earlier run is alive and the history yields at least one file to compile, it starts a child with `xl-eln-opt-start`. Otherwise it does nothing.
- There is no polling and no hook on your commands. The main Emacs compiles nothing itself.

## Where the sources come from

| Source | Read from | Becomes |
| --- | --- | --- |
| Commands you ran | `extended-command-history` in the savehist file | The `symbol-file` of each command |
| Minor modes that were enabled | `xl-eln-opt-minor-modes` in the savehist file | The `symbol-file` of each mode |
| Major modes of recent files | `auto-mode-alist`, applied to the names in `recentf-list` | The `symbol-file` of each mode |
| Recent Emacs Lisp files | `recentf-list` | The file itself (`.el` or `.el.gz`) |

- The savehist and recentf files are read as data, with `read-eval` bound to nil. Nothing in them is evaluated.
- Minor modes are recorded by `savehist-save-hook`, which lists the modes enabled globally or in some buffer at each save. The list keeps at most 200 names. A mode that is enabled during a session is therefore known to xl-eln-opt only after the next save.
- A function's source is the `.el` (or `.el.gz`) next to the file that defines it. A C primitive has no Lisp source and is skipped.

## Which files are compiled

Each source gets one verdict. The first rule that applies decides:

1. Outside the trusted roots: skipped, "outside the trusted roots".
2. Opted out: skipped, "opts out with `no-native-compile`" (or `no-byte-compile`).
3. Larger than `xl-eln-opt-max-source-bytes` (1 MB, uncompressed): skipped, with the size and the limit.
4. An `.eln` exists and is not older than the source: "native already".
5. Failed before, and the source has not changed since: "failed at <time>: <reason>".
6. Otherwise: "to compile".

Trusted roots are Emacs's `lisp-directory`, the package folder, the profile folder (`user-emacs-directory`), and `tuned-emacs/` when `xl-dotfiles` is set. A file counts only when it is a regular file, not a symbolic link, and its true name lies under a root.

A file opts out by setting `no-native-compile` or `no-byte-compile` to `t`. The cookie counts on the `-*-` first line, or on a line of the local variables (a line that starts with the name, after optional spaces or `;`).

## The child process

`xl-eln-opt-start` runs a batch Emacs with the profile's `early-init.el` and `init.el`, then calls `xl-eln-opt-run`:

- A pid file, `xl-warm/pid` in the profile, allows one run at a time. A zombie process (exited but not yet reaped) does not count as alive.
- For each file, in history order: follow the trace marker (see Tracing), then `native-compile` to the `.eln` path given by `comp-el-to-eln-filename`. The file succeeds when the `.eln` exists afterwards.
- A failure (an error, or no `.eln` written) is recorded with the source, its modification time, the reason and the time. The record is kept in `xl-warm/failed.eld` until the source changes.
- Because the child loads the profile, advice added at load time makes Emacs build the trampolines of advised C primitives in the child, so the main Emacs does not build them on its first use.

The pid and failed-list folder keeps the name `xl-warm/` from the module's earlier name. It is a folder on disk and was not renamed.

## Forcing a compile

- `xl-eln-opt-compile-file FILE`: compiles FILE now, even if it is native already or failed before.
- `xl-eln-opt-compile FUNCTION`: compiles the source of FUNCTION. An autoload is loaded first, so a command that is not loaded yet still works.

A forced compile ignores the failed list and the size limit. It does not ignore the trusted-root rule or an opt-out cookie.

## The report

`xl-eln-opt-report` starts a child that prints the report, and the buffer `*xl-eln-opt report*` shows a loader until the report arrives. Nothing is compiled.

- Status line: how many sources come from the history, how many entries have no source, and how many files the next run compiles.
- A table with one row per source: its name without `.el` or `.el.gz`, its size in KB, its status (native, to compile, failed, skipped), the history entries that use it, and the force command with its full path.
- "Not compiled, and why": the skipped and failed sources, with their reasons.
- "Not yet compiled": history entries with no source, with the reason. An autoloaded command gets a hint, `(xl-eln-opt-compile 'name)`.

## Cleaning

`xl-eln-opt-clean` deletes every `.eln` in the user's eln folder except the subr trampolines, which are costly to rebuild, and it deletes the failed list. It refuses while a child is running. Interactively it asks for a yes-or-no answer first.

## Tracing

While `xl-perfetto-trace-start` records in the main Emacs, a child run follows the recording with `xl-perfetto-follow-marker` (prefix `xl-eln-opt-trace`). The child writes its own `xl-eln-opt-trace-*.json` into the same folder. The run is the span `xl-eln-opt run`, and each file is a span named `native <file>`. The follow happens between files, so a file that is being compiled is not checked for the marker until it finishes. See [xl-perfetto-trace.md](xl-perfetto-trace.md).

## Customization

| Variable | Default | Meaning |
| --- | --- | --- |
| `xl-eln-opt-mode` | 1 | 0: only when you run a command; 1: also once per session, after the idle delay |
| `xl-eln-opt-idle` | 5.0 | Seconds of idle time before the once-per-session check |
| `xl-eln-opt-max-source-bytes` | 1048576 | Largest source, uncompressed, that is compiled without force |

## Known limits

- Not yet verified: that `.eln` files built here are picked up by a GUI start after a restart.
- No ERT suite covers this module yet.
- The minor-mode list changes only when savehist saves.
- The idle check runs once per session. A long session is not re-checked until the next start.
