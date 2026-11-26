# Brief: implement xl-perfetto-trace.el

This is the brief that was given to the implementing agent. The feature is now implemented; the brief is kept as a record of the constraints and the verification steps.

---

You are working in the `~/workspace/dotfiles` repository. The task is to implement a new Emacs Lisp module, `tuned-emacs/lisp/xl-perfetto-trace.el`. It records what Emacs does at the Lisp level over a period of time, writes a Chrome JSON trace file, and lets the user view the timeline at ui.perfetto.dev.

## Read these first

1. `AGENTS.md`: the repository's code style and rules.
2. `tuned-emacs/doc/xl-perfetto-trace.md`: **the full design. Implement it as written.** Command names, variable names, event format, track numbers, lifecycle, test plan and acceptance criteria are all there. For any detail the design does not cover, choose a reasonable approach and list it in the final report.
3. `tuned-emacs/lisp/xl-async.el`: the arguments of `xl-async-run` (`:command`, `:name`, `:on-done`, `:slot`, `:owner`, `:timeout`), and `xl-async-cancel`. You will add advice to `xl-async-run`.
4. `tuned-emacs/lisp/xl-dired.el` and `tuned-emacs/tests/xl-dired-tests.el`: what an `xl-*` module and its tests should look like (file header, defgroup, defcustom, temporary directories in tests, and how tests wait).
5. `cmd_test` in `tuned-emacs/install/emacs-setup.sh`: how the tests are run.

## Must follow

- **No changes to Emacs's C code, and no external programs.**
- **No advice on any C primitive** (a function for which `subr-primitive-p` is true, such as `make-process`, `set-process-filter` or `garbage-collect`). The reason is that native-comp trampolines are compiled synchronously and freeze the interface. Processes are wrapped with `add-function :around (process-filter proc)`. New processes are found by a timer that scans during recording; the design describes how.
- **Zero cost when not recording.** Hooks, advice, timers and process-function wrappers exist only between start and stop. Stop must be idempotent and must always remove all instrumentation (`unwind-protect`). A failure in the middle of start must roll back.
- **No content recorded.** No key sequences, buffer text, minibuffer input or process output.
- Style: `lexical-binding: t`, 2-space indentation, kebab-case. Public names use the `xl-perfetto-` prefix; internal names use `xl-perfetto--`. Use only ordinary Emacs APIs. No framework-specific names such as `crafted-` or `spacemacs/` (a smoke test checks this).
- Change only these files:
  - create `tuned-emacs/lisp/xl-perfetto-trace.el`
  - create `tuned-emacs/tests/xl-perfetto-trace-tests.el`
  - `tuned-emacs/install/emacs-setup.sh`: add a step to `cmd_test` that runs the new test file
  - `tuned-emacs/tests/smoke-tests.el`: add the new file to the file list in `xl-personal-file-is-framework-neutral`
  - `tuned-emacs/init.el`: add one line, `(require 'xl-perfetto-trace)`
  - `tuned-emacs/doc/xl-perfetto-trace.md`: only write the measured results under "Performance budget", and change the status to implemented
- Do not change any other file. If the working tree has someone else's uncommitted changes, leave them alone, and do not commit them.
- **Do not commit** unless the user explicitly asks. If the user asks for a commit, the last line of the message is `Signed-off-by: Xueliang Zhong <xueliang.zhong@gmail.com>`.

## How to verify

1. Byte-compilation produces no warnings, except "function not defined" warnings from optional dependencies:
   ```bash
   emacs --batch -Q -L tuned-emacs/lisp -f batch-byte-compile tuned-emacs/lisp/xl-perfetto-trace.el
   ```
   Delete the generated `.elc` afterwards.
2. Run the new test on its own:
   ```bash
   emacs --batch -Q -L tuned-emacs/lisp -l tuned-emacs/tests/xl-perfetto-trace-tests.el -f ert-run-tests-batch-and-exit
   ```
3. Run every test suite:
   ```bash
   bash tuned-emacs/install/emacs-setup.sh test
   ```
   They must all pass. If something fails for reasons unrelated to your change, report it; do not change someone else's tests.
4. Make one real recording in batch: start, run a few commands, a timer, one `garbage-collect`, and one `xl-async-run`; stop; read the file back with `json-parse-string`; and confirm that every track has events.

Acceptance in a GUI (opening the recorded file at ui.perfetto.dev) cannot be done from the agent's environment. Say so in the report and leave it to the user.

## Report when finished

Write the report in the user's language, lead with the result, and include:

- What was done, and which files changed.
- The test results: how many tests in each suite, and whether all pass.
- Decisions made where the design did not decide, one sentence each, with the reason.
- What was not done or not verified, such as the GUI acceptance, and why.
