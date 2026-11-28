# Windows

Setup on Windows runs from Git Bash. The same profile and packages are used as on macOS, with these differences.

## Prerequisites

1. **A working Emacs.** Use the GNU build for Windows from `https://ftp.gnu.org/gnu/emacs/windows/emacs-31/`. Download `emacs-31.1.zip` or the installer. Do not download `emacs-31-deps-mingw-w64-src.zip`; it contains library sources, not a program.
2. **Unpack and add to `PATH`.** For example, unpack to `C:\Emacs` and add `C:\Emacs\bin` to the user `PATH`. Keep the file modification times when unpacking; if they are lost, Emacs loads source instead of byte code and starts noticeably slower.
3. **Git Bash and `just`.** Both must be on `PATH`. If `emacs --version` works in Git Bash, Emacs is ready.

## Setup

```bash
just emacs-windows-setup                                 # with just
bash tuned-emacs/bin/tuned install                       # without just
```

The CLI:

1. Checks that `emacs` exists and is version 30 or later.
2. Sets the user environment variable `HOME` to your user folder, if it is not set. Emacs started from the Start menu only reads this variable. Without it, `~` points to `%APPDATA%`, and paths such as `~/workspace/...` are not found.
3. Moves any existing `~/.emacs`, `~/.emacs.el` and `~/.emacs.d` into `~/.emacs-profiles/backup-<date-time>/`. Nothing is deleted. Emacs reads `~/.emacs` and `~/.emacs.el` before `~/.emacs.d`, so they must go.
4. Clones Crafted at the pinned commit and writes the two stubs directly into `~/.emacs.d`. Unlike macOS, there is no symbolic link.
5. Loads the configuration in batch mode, which installs the packages, then generates the autoloads, byte-compiles, builds the trampolines and writes `packages.lock`.

The CLI can be rerun. If `~/.emacs.d` already points at the profile it created, it is kept, and only the stubs and missing packages are updated.

## Platform differences

| Item | On Windows |
| --- | --- |
| Native compilation | The official dependency bundle has no libgccjit, so there is no native compilation. JIT is off in any case, so nothing changes. |
| dired | Windows has no `ls`. Emacs uses the built-in `ls-lisp` emulation, which supports `-alh`. |
| Fonts and size | JetBrains Mono, then Consolas, is used for `default`, `fixed-pitch`, `fixed-pitch-serif` and `variable-pitch`. The Windows font size is set separately to 120. |
| Completion | corfu waits 0.2 seconds, and the second typed character, before querying the completion backends. `cape-dabbrev` scans only the current buffer, and skips Chinese words and buffers over 200,000 characters. |
| macOS only | The macism input-source switch (started asynchronously), `s-x` and the UK keyboard Option-3 fix are skipped on Windows. |
| Search and fzf | `SPC *`, `SPC s p` and `SPC p s` need `rg` (ripgrep): install it with `winget install BurntSushi.ripgrep.MSVC`. eshell's fzf integration uses `consult-fd` when `fd` exists, otherwise `consult-find`. |
| Org shell blocks | They run in `cmd.exe`, so blocks written for bash may fail. |
| Machine-specific paths | Put links, note folders and similar paths in `~/.emacs-profiles/local.el`. Keep that file out of the repository. |

## Line endings

The `.gitattributes` file at the repository root keeps text files as LF on Windows too. Git for Windows converts files to CRLF by default, which makes the bash scripts in the `justfile` fail with `\r: command not found`, and adds a trailing `\r` to the commit read from `crafted.lock`.

A clone made before `.gitattributes` existed keeps CRLF endings. If the working tree has no uncommitted changes, renormalise it with:

```bash
git rm -r -q --cached . && git reset --hard
```

## Not yet verified on real Windows

The installation flow was tested on macOS, with a temporary `HOME`. It covered moving the old configuration aside, writing the stubs, installing packages and running a second time. These steps have only been checked on Windows itself and still need confirmation:

- setting the user `HOME` variable (`powershell.exe` and `setx`);
- path conversion with `cygpath`;
- whether the official Emacs package includes the gnutls DLLs that HTTPS needs for package installation.

Watch the output of these steps the first time you run setup on Windows.
