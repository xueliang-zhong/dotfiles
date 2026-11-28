# Lisp modules

The modules in `lisp/` are loaded with `require`. Some of them, such as `xl-async`, `xl-spinner` and `xl-git-status`, can be installed without the rest of this layer. `xl-key-bindings` and `personal.el` depend on `xl-dotfiles` and on each other, so they are part of the layer, not standalone packages.

## Key bindings: `lisp/xl-key-bindings.el`

Recreates the Spacemacs key habits that Crafted does not provide:

- The SPC tree (groups such as `b`, `f`, `g`, `j`, `p`, `q`, `r`, `s`, `t`, `w`) and `M-m`.
- `SPC i s` lists the snippets for the current mode; `SPC i S` creates one. The keys match Spacemacs' auto-completion layer.
- `,` and `SPC m` are the major-mode leaders. They work in Org and Emacs Lisp.
- `fd` leaves insert state, evil-surround is on, and `SPC 1` to `SPC 9` select windows.
- The home buffer (`SPC b h`) lists recent files and opens one by its number.
- `SPC q r` restarts Emacs. The restart returns to the current file and point, and a dired buffer returns to its directory. From a temporary buffer with no file, it returns to the home buffer. The state is kept in `xl-restart-place.eld` under `user-emacs-directory` and is used once; cancelling a restart deletes it.
- It creates `xl-leader-map`, which `personal.el` extends.

It loads before `personal.el`, so personal bindings override these defaults (for example `SPC SPC` and `SPC x`).

## Asynchronous Git status: `lisp/xl-git-status.el`

A status summary that does not call Magit. It shows `Head:`, untracked files, unstaged changes, staged changes (with hunks under each file) and the ten most recent commits, in the same order as `magit-status`.

**Auto-refresh.** While the status window is visible, changes in the work tree refresh it without `g`. The watcher uses `file-notify`. macOS and Linux do not watch recursively, so the root, `.git`, and every directory containing a tracked or untracked file are watched separately. Lock files, editor backups and `.git` entries that do not affect status (objects, logs) are ignored. Changes are merged, and the refresh runs 0.5 seconds after the last change (`xl-git-status-refresh-delay`). Auto-refresh is skipped while the window is hidden or the Git queue is busy, and `xl-git-status-auto-refresh` turns it off. It has been tested in a GUI Emacs on macOS; the inotify path on Linux is not yet tested.

**Loader.** On first load, the mode line's leftmost slot and the centre of the window both show a spinner. The centre one is an overlay `before-string` at the start of the buffer and does not change the text. A status that is ready within 0.1 seconds shows no spinner. Later refreshes keep the old content on screen and replace it when the new content is ready, so there is no blank frame or `Loading...` flash. If a slow operation shows the loader, the label changes to `Refreshing` in the mode line and disappears when the render finishes.

**Keys.** The buffer is read-only. `g` refreshes, `q` quits, and `SPC g g` opens it. Folding works as in Magit: file diffs start collapsed, `TAB` toggles the file or hunk under point, and `S-TAB` toggles all file diffs.

- `s` stages the hunk, file or section at point; `u` unstages; `X` discards (with a `y/n` prompt; this also deletes untracked files); `S` and `U` stage and unstage everything.
- `p` opens a push menu, drawn with `set-transient-map` and which-key, with the target ref in the label. `p` pushes to the push remote, `u` to the upstream, and `e` to another remote, chosen in the minibuffer.
- Every write operation of a repository goes through one queue, keyed by the repository root (`xl-git-status--queue-key`), and `Gwrite`/`Gcommit` join the same queue. One Git write per repository runs at a time, so `s s s` runs in order and a `Gwrite` waits for a running pull instead of failing on `.git/index.lock`. The loader appears only if an operation takes longer than `xl-spinner-show-delay` (0.1 s).
- The loader sits in the mode line, not the header line. A header line that appears and disappears moves the whole buffer by a line; the mode line stays put, so a slow push does not shift the window. It is at the far left, because `mode-line-process` is cut off when a split window is narrow.
- After the queue drains, the status is refreshed once, silently.
- After `s` or `u`, point stays on the same file or hunk. Hunks are matched by content, so renumbering does not move point. If the target disappears, point goes to the nearest heading.
- With no HEAD commit, `u` falls back to `git rm --cached`. Failures are reported with `message`, so Git's output is not swallowed. A push without an upstream reports Git's own hint.
- `RET` opens a file on a file line, jumps to the hunk's line on a hunk, and shows the commit on a commit line.
- The commit view is asynchronous too: it opens at once with the hash and a spinner, and fills in the diff when `git show` returns. Opening the same commit again discards the older result.

Headings, status, file names, hashes, branch and tag names, and diff lines use the theme's `font-lock` and `diff-mode` colours.

## Git commands: `lisp/xl-git-commands.el`

Commands in the style of vim-fugitive:

- `Gstatus` (`:Gstatus`) opens the status buffer.
- `Gblame` (`SPC g b`, or `:Gblame`) opens a blame column beside the current file. Each source line has its commit, author and date. Commit hashes use different colours, and uncommitted lines are dimmed. The column scrolls with the source window and highlights the current line. It is an ordinary window, so `C-w C-w` moves into it, and `:only`, `C-w c` and `:x` close it. `git blame` runs in the background: the window opens at once with placeholder lines matching the source line count, so scrolling and point stay aligned. The loader is shown in the mode line and, while the window is still empty, at its centre. Errors are written in place without moving the placeholders. `RET` shows the commit on the line, `g` refreshes and `q` closes.
- `Gwrite` (`:Gwrite`) stages the current file.
- `Gcommit` (`:Gcommit`) commits the current file with the message `Update <file name>`.

`Gwrite` and `Gcommit` queue their `git` command on the repository, the same queue the status buffer uses for staging, push and pull, then the queue's drain refreshes that repository's status buffer. Failures are shown with `message`. A slow pre-commit hook therefore does not block Emacs, the two cannot race over `.git/index.lock`, a `Gwrite` before a `Gcommit` always runs first, and the commands do not depend on evil.

## Shared async layer: `lisp/xl-async.el`

The status, GBlame, commit view and G commands each used to have their own `make-process`, sentinel, output buffer and stale-result check. They now all use `xl-async-run`:

```elisp
(xl-async-run
 :command '("git" "blame" "--line-porcelain" "--" "init.el")
 :directory root
 :slot 'blame                          ; per-buffer slot: a new request cancels the old one
 :stdin patch                          ; optional: written to standard input
 :on-done (lambda (output code) ...))  ; called in the owner buffer
```

Killing the owner buffer kills its process, and changing major mode does not lose ownership. `:owner nil` is for commands that should outlive their buffer, such as the `Gcommit` hook. `xl-async-enqueue` is a serial queue: one job at a time, and when a job fails the next command is tried. A cons queue key is shared by every buffer that passes it, and a job may set its own `:owner` (or `nil`); the status buffer and the G commands use one key per repository. Status uses it for `s`, `u`, `X`, push, fetch, pull, `Gwrite` and `Gcommit`.

See [xl-git-write-queue.md](xl-git-write-queue.md) for the repository queue, and [xl-async.md](xl-async.md) for the design of the async layer.

## Org source blocks: `lisp/xl-org-babel-async.el`

`:async yes` on a source block runs it in the background, and `C-c C-c` returns at once. Without the header argument, blocks run synchronously as before. Blocks with `:session` cannot run asynchronously, so they run synchronously with a message.

Output has two stages, so long output does not slow editing:

- While running, all output goes to `*Org Babel Output: file:line*`. Below the block only the last `xl-org-babel-live-lines` (default 20) lines are shown.
- After the run, the result written to the file is the first `xl-org-babel-result-lines` (default 200) lines, followed by a note. The full output stays in the output buffer.

The result location is a marker, so text edited above it while the block runs does not misplace the result. If the block is deleted, the result stays only in the output buffer. `C-c C-k` cancels the block at point and keeps the output so far. Running the block again cancels the run in progress. `:timeout SECONDS` stops the process after the timeout.

A running block shows a spinner on its `#+RESULTS:` line (an overlay, so the file is not changed). The mode line shows `●·· 2 blocks running`, so a background job is visible even when its block is off screen.

Blocks run in parallel and are not queued. Blocks that depend on each other must be ordered by hand.

`xl-async.el` gained two options for this: `:on-output` (called for each chunk of output) and `:timeout` (kills the process and exits with 124, as coreutils `timeout` does).

## Consult spinner: `lisp/xl-consult-spinner.el`

Asynchronous consult lists such as `consult-ripgrep` and `consult-find` show a status character before the prompt: `*` while running and `:` when done. This module replaces the running character with a pulsing dot (`·` `•` `●` `•`, one frame every 0.15 s, a 0.6 s cycle; `. o O o` when the font cannot draw dots). The frames come from `xl-spinner.el`. The width does not change, so typed text does not move. It wraps `consult--async-indicator`, so only asynchronous lists are affected.

## Shared loader: `lisp/xl-spinner.el`

The goal is a loader that is visible but does not move the screen. It has two layers.

**Kernel.** Only the clock and the frame counter. The caller decides where to draw. The commit view uses it to draw in an overlay on an empty buffer, so nothing is pushed aside.

```elisp
(xl-spinner-start
 (lambda (text)                       ; text is nil when stopping; the callee clears it
   (overlay-put my-overlay 'display text))
 :limit 600)                          ; optional: at most 600 frames (one minute at 0.1 s)
```

**Mode-line slot.** For buffers that already have content: status refreshes and the write queue, GBlame and the package menu. It sits at the far left of the mode line, where a narrow split cannot cut it off. Inserting a line in the buffer would push the content down, so it is never placed in the buffer.

```elisp
(xl-spinner-mode-line-install)              ; once, in the major mode
(xl-spinner-mode-line-start "Pushing main") ; appears after 0.1 s
(xl-spinner-mode-line-label "Refreshing")   ; change the label
(xl-spinner-mode-line-stop)                 ; or (xl-spinner-mode-line-stop "git blame failed: ...")
```

The following rules keep the window still; each has a test.

- **Frames use only glyphs from the mode line's own font.** A glyph missing from the font is borrowed from a fallback font, and on a GUI that can make the mode line taller, so the loader makes the window jump. In tests with JetBrains Mono, Braille characters and `▰` made the mode line grow from 23 to 24 pixels. The default `xl-spinner-style` is `auto`: on a GUI, dots (`●··`, `·●·`, `··●`) are used only when `char-displayable-p` says the mode-line font can draw them, otherwise ASCII `[|] [/] [-] [\]`. In a terminal, dots are used only in a UTF-8 locale with single-width characters. Set `xl-spinner-style` to `ascii` if the terminal font lacks dots.
- **Wait 0.1 s before showing, then keep the loader at least 0.3 s** (`xl-spinner-show-delay`, `xl-spinner-min-duration`). An operation that finishes before the delay never shows anything, and a very short one does not flash.
- **The slot only grows while busy.** Changing the label from `Pushing main to origin` to `Refreshing` pads with spaces, so the buffer name does not move. A new operation during the linger time reuses the same loader and only changes the label.
- **Colour, not size, makes it noticeable.** The dot uses the `xl-spinner` face, which inherits the theme's keyword colour. While busy, the mode-line background is tinted 20% toward that colour (`xl-spinner-tint`; set it to nil to turn this off). Box and height do not change, and terminals with fewer than 256 colours get no tint.
- **0.1 s per frame** (`xl-spinner-interval`). It is visibly moving without flickering, and it sends half the redraw traffic of a 0.05 s interval over SSH.

Measured in a GUI Emacs with JetBrains Mono at 18 pt: from idle, through appearance, tinting, three frames and a label change, back to hidden, the mode line stayed 23 pixels tall, and the text height and `window-start` did not change.

## Personal settings: `personal.el` and `example/xueliang.el`

`personal.el` is the file to edit. It starts empty, with comments that explain where to begin. `example/xueliang.el` is the author's full configuration, grouped by topic below; it loads instead of `personal.el` when the profile stub sets `xl-load-example` (the dotfiles `justfile` sets `XL_EXAMPLE=1` for its recipes; running `emacs-setup.sh` directly leaves it unset). Copy the parts you want into `personal.el`.

- Platform, paths and fonts. All text faces use the selected font. The theme is set here. `~/.emacs-profiles/local.el` is loaded if it exists, and holds machine-specific paths. It is not in the repository.
- macOS UK keyboard: Option-3 should type `#`, but Emacs treats Option as Meta and receives `M-3`. In insert, replace and Emacs states, in the minibuffer (including evil's `:` and search prompts) and in isearch, Option-3 types `#`. In normal state `M-3` keeps its numeric-prefix meaning.
- Global settings: backups, recentf, which-key, indentation, long lines not wrapped, parent directories created on save.
- evil habits: `vs` and `sp`, `C-u` scrolls up, `*` searches the whole symbol, and IME switching.
- Completion: Crafted turns on vertico and corfu, and this file adds only the changes. The candidate list is 38.2% of the frame height. `SPC r l` repeats the last list. In a candidate list, `C-.` opens embark actions, `C-q` and `M-w` export to a buffer. corfu pops up after a pause (0.15 s, on Windows 0.2 s) and two characters. Popup colours follow the theme and update when the theme changes. Orderless matches candidates only in the minibuffer; buffers use plain prefix matching, because matching every in-buffer candidate with orderless cost three times as much. cape's sources are added globally, dabbrev scans only the current buffer, and it is skipped after a Chinese word (where the candidates would be whole sentences) and in buffers over 200,000 characters (where the scan costs several milliseconds per keystroke). On macOS, leaving insert state switches back to the English layout without waiting for `macism`.
- Chinese input: pyim, an input method in Emacs Lisp, instead of the macOS IME. The macOS Chinese IME handles keys inside Emacs's C key handler, so the first key after switching languages waited for it (about 200 ms, invisible to the Lisp tracer); pyim has no system round trip. `F2` turns it on and off (`C-\` also works). It passes keys through in the minibuffer, in isearch and in Evil normal/visual state, so single-key commands stay commands. Candidates appear at point through posframe, falling back to the minibuffer. Two word lists are enabled: pyim-basedict (about 100,000 words, from libpinyin), and THUOCL from `pyim-tsinghua-dict`, which `init.el` fetches from Git (pinned like Crafted) because it is in no ELPA; the second one ranks everyday words such as 今天, 输入法 and 北京大学 correctly, where the first offers 进天, 术入发 and 北经大学. Neither list has tones, so pinyin is typed without them. The macOS input source should stay on English, otherwise the system IME consumes the keys first.
- Snippets: `SPC i s` lists the snippets for the current mode with yasnippet, and `SPC i S` creates one. yasnippet loads on first use.
- Org: `org-modules` stays empty, but `org-tempo` is required explicitly, so `<s` then TAB expands to a `#+begin_src` block.
- Persistent scratch: `SPC b s` opens `*scratch*`. persistent-scratch restores it at startup, saves it after 30 seconds idle and at exit, and `C-x C-s` saves at once. The file is `~/.emacs-profiles/crafted/persistent-scratch.eld`. Pressing `SPC b s` again switches to the existing scratch window rather than splitting. Batch runs (tests, the setup script) neither read nor write this file.
- dired, eshell, Org and magit settings and helper functions.
- Function keys F2 to F12, and personal commands under `SPC`. F2 toggles Chinese input (pyim), the other keys keep their vim/Neovim habits.
- F7 and `SPC c c` run `just` through `compile`, which keeps the output in `*compilation*`. On failure, `next-error` jumps to the `file:line` that failed.

This file must not refer to `spacemacs/`, `doom` or `crafted-` symbols, so that replacing the framework does not require changes to it.
