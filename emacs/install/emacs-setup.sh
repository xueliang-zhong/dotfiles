#!/usr/bin/env bash
# emacs-setup.sh - install, run and check the XL Emacs config without `just`.
#
# The `emacs-*` recipes in the dotfiles justfile are thin wrappers around
# this script, so a machine without `just` can do the same setup:
#
#   bash emacs/install/emacs-setup.sh setup-macos     # dotfiles + Crafted + profile
#   bash emacs/install/emacs-setup.sh setup-linux
#   bash emacs/install/emacs-setup.sh setup-windows   # from Git Bash on Windows
#   bash emacs/install/emacs-setup.sh crafted-install [DIR]
#   bash emacs/install/emacs-setup.sh smoke|test|check
#   bash emacs/install/emacs-setup.sh trampolines
#   bash emacs/install/emacs-setup.sh jit
#   bash emacs/install/emacs-setup.sh compile-lisp
#   bash emacs/install/emacs-setup.sh jit-run
#   bash emacs/install/emacs-setup.sh jit-reset
#   bash emacs/install/emacs-setup.sh jit-report
#   bash emacs/install/emacs-setup.sh jit-status
#   bash emacs/install/emacs-setup.sh make-default|restore-spacemacs
#   bash emacs/install/emacs-setup.sh try [EMACS-ARGS...]
#
# Overrides: EMACS=/path/to/emacs, PROFILE=other, PROFILE_DIR=/some/dir,
#            DOTFILES=/path/to/dotfiles, XL_SKIP_DOTFILES=1 (skip env.sh),
#            XL_VERBOSE=1 (stream each setup step's output).
set -euo pipefail

script_dir=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd -P)
DOTFILES=${DOTFILES:-$(cd "$script_dir/../.." && pwd -P)}
EMACS=${EMACS:-emacs}
PROFILE=${PROFILE:-crafted}
PROFILES=${PROFILES:-${HOME:-$USERPROFILE}/.emacs-profiles}
PROFILE_DIR=${PROFILE_DIR:-$PROFILES/$PROFILE}

say() { printf '%s\n' "$*"; }
die() {
    printf 'emacs-setup: %s\n' "$*" >&2
    exit 1
}

# Step headings for the multi-step setup commands.  Noisy commands run
# under `run_quiet'; the long profile/trampoline step prints its own
# package/trampoline progress lines.  NO_COLOR turns the colours off.
progress_total=0
progress_done=0
progress_started=${SECONDS:-0}
progress_colors=0

progress_init() {
    progress_total=$1
    progress_done=0
    progress_started=${SECONDS:-0}
    progress_colors=0
    if [ -t 1 ] && [ "${TERM:-dumb}" != dumb ]; then
        progress_colors=1
    fi
}

progress_color() {
    [ "$progress_colors" = 1 ] && [ -z "${NO_COLOR+x}" ]
}

progress_fail() {
    local title=${1:-}
    if progress_color; then printf '\033[31m'; fi
    printf '[%d/%d] FAILED  %s\n' "$progress_done" "$progress_total" "$title"
    if progress_color; then printf '\033[0m'; fi
}

progress_step() {
    local title=$1
    shift
    progress_done=$((progress_done + 1))
    if progress_color; then
        printf '\033[1m[%d/%d]\033[0m %s\n' \
            "$progress_done" "$progress_total" "$title"
    else
        printf '[%d/%d] %s\n' "$progress_done" "$progress_total" "$title"
    fi
    if "$@"; then
        return 0
    else
        progress_fail "$title"
        return 1
    fi
}

progress_finish() {
    local message=${1:-Done} elapsed
    elapsed=$(( ${SECONDS:-0} - progress_started ))
    if progress_color; then
        printf '\033[1;32m%s\033[0m (%ss)\n' "$message" "$elapsed"
    else
        printf '%s (%ss)\n' "$message" "$elapsed"
    fi
}

# Run a step with only its own progress lines visible: init.el prints
# package/trampoline counts when XL_PROGRESS is set, and emacs_batch hides
# the usual "Loading ..." / "Contacting host" messages.  XL_VERBOSE=1
# streams the full output instead.
run_progress() {
    if [ "${XL_VERBOSE:-0}" = 1 ]; then
        "$@"
    else
        XL_PROGRESS=1 "$@"
    fi
}

# Run a command with its output hidden; print the captured log only when it
# fails.  Setup uses this for the noisy steps (copying dotfiles, cloning,
# loading the profile and downloading packages).  XL_VERBOSE=1 streams it.
# The command runs in a subshell so a `die' inside it fails only this step:
# its message stays in the log and is printed below.
run_quiet() {
    local log status
    if [ "${XL_VERBOSE:-0}" = 1 ]; then
        "$@"
        return
    fi
    log=$(mktemp "${TMPDIR:-/tmp}/xl-setup.XXXXXX" 2>/dev/null ||
        printf '%s' "${TMPDIR:-/tmp}/xl-setup.$$.log")
    if ( "$@" ) >"$log" 2>&1; then
        rm -f "$log"
    else
        status=$?
        printf '\n--- output of the failed step ---\n' >&2
        cat "$log" >&2
        rm -f "$log"
        return "$status"
    fi
}

# Paths written into the stubs and passed to git must be ones native Emacs
# and Git for Windows understand: on Git Bash turn /c/Users/me into C:/Users/me,
# elsewhere keep the path as it is.
winpath() {
    if command -v cygpath >/dev/null 2>&1; then
        cygpath -m "$1"
    else
        printf '%s' "$1" | tr '\\' '/'
    fi
}

require_emacs() {
    command -v "$EMACS" >/dev/null 2>&1 ||
        die "emacs not found: put Emacs 30+ on PATH, or set EMACS=/path/to/emacs"
    local ver
    ver=$("$EMACS" --batch --eval '(princ emacs-major-version)' 2>/dev/null || true)
    [ "${ver:-0}" -ge 30 ] ||
        die "Need Emacs 30 or newer, found: ${ver:-none}"
}

require_profile() {
    [ -r "$PROFILE_DIR/init.el" ] ||
        die "no profile at $PROFILE_DIR; run: $0 crafted-install"
}

# Load the profile in batch exactly as a GUI start does: early-init, Crafted's
# before-init-hook, then init.el.  Any init error fails the command.
emacs_batch() {
    if [ "${XL_PROGRESS:-0}" = 1 ]; then
        # Setup asks init.el for compact progress lines; hide Emacs messages.
        "$EMACS" --batch --init-directory "$PROFILE_DIR" \
            --eval '(setq inhibit-message t)' \
            -l "$PROFILE_DIR/early-init.el" \
            --eval '(run-hooks (quote before-init-hook))' \
            -l "$PROFILE_DIR/init.el" "$@"
    else
        "$EMACS" --batch --init-directory "$PROFILE_DIR" \
            -l "$PROFILE_DIR/early-init.el" \
            --eval '(run-hooks (quote before-init-hook))' \
            -l "$PROFILE_DIR/init.el" "$@"
    fi
}

install_dotfiles() {
    if [ "${XL_SKIP_DOTFILES:-0}" = 1 ]; then
        say "Skipping the dotfiles install (XL_SKIP_DOTFILES=1)"
    else
        (cd "$DOTFILES" && bash env.sh)
    fi
}

write_profile_stubs() {
    local dir=$1 dotfiles=$2 src=$3
    {
        echo ';;; early-init.el -*- lexical-binding: t; -*-'
        echo ';; Generated by emacs/install/emacs-setup.sh; edit emacs/ in dotfiles.'
        printf '(defvar xl-dotfiles "%s/")\n' "$dotfiles"
        printf '(defvar xl-crafted-home "%s/")\n' "$src"
        echo '(load (expand-file-name "emacs/early-init.el" xl-dotfiles))'
    } >"$dir/early-init.el"
    {
        echo ';;; init.el -*- lexical-binding: t; -*-'
        echo ';; Generated by emacs/install/emacs-setup.sh; edit emacs/ in dotfiles.'
        echo '(load (expand-file-name "emacs/init.el" xl-dotfiles))'
    } >"$dir/init.el"
}

# Clone Crafted Emacs at the locked commit and generate the profile stubs in DIR.
cmd_crafted_install() {
    local dir=${1:-$PROFILE_DIR}
    local lock="$DOTFILES/emacs/frameworks.lock"
    local url rev src dotfiles
    [ -r "$lock" ] || die "cannot read $lock"
    # tr: a CRLF checkout (Git for Windows' default) leaves "\r" on the commit id
    read -r _ url rev < <(tr -d '\r' <"$lock")
    [ -n "$url" ] && [ -n "$rev" ] ||
        die "cannot read url and commit from $lock"

    dotfiles=$(winpath "$DOTFILES")
    src="$(winpath "$PROFILES")/src/crafted-emacs"
    dir=$(winpath "$dir")
    mkdir -p "$(dirname "$src")" "$dir"

    if [ ! -d "$src/.git" ]; then
        say "Cloning $url into $src"
        git clone --quiet "$url" "$src" ||
            die "git clone failed (network, proxy, or $src not writable)"
    fi
    git -C "$src" cat-file -e "$rev^{commit}" 2>/dev/null ||
        git -C "$src" fetch --quiet origin ||
        die "git fetch failed in $src"
    git -C "$src" checkout --quiet "$rev" ||
        die "cannot check out commit $rev in $src"

    write_profile_stubs "$dir" "$dotfiles" "$src"
    say "Crafted profile ready: $dir"
}

cmd_smoke() {
    require_emacs
    require_profile
    emacs_batch --eval '(message "smoke ok: Emacs %s, %s" emacs-version user-emacs-directory)'
}

cmd_test() {
    require_emacs
    require_profile
    emacs_batch -l "$DOTFILES/emacs/tests/smoke-tests.el" \
        -f ert-run-tests-batch-and-exit
    # xl-jit runs in its own batch process, without the profile.
    "$EMACS" --batch -Q -L "$DOTFILES/emacs/lisp" \
        --eval '(setq load-prefer-newer t)' \
        -l "$DOTFILES/emacs/tests/xl-jit-tests.el" \
        -f ert-run-tests-batch-and-exit
}

# Tier 1 of xl-jit: byte-compile our own Lisp so it does not run interpreted.
# Runs with the profile loaded, so macros from installed packages (evil, ...)
# expand correctly; compiling in a bare batch Emacs would turn them into calls.
# Only files whose .el is newer than their .elc are compiled, so a second run
# compiles nothing.  Any failure makes the command fail.  No libgccjit needed.
cmd_compile_lisp() {
    require_emacs
    require_profile
    emacs_batch --eval "
(let* ((dir (expand-file-name \"emacs\" \"$DOTFILES\"))
       (files (append (directory-files (expand-file-name \"lisp\" dir) t \"\\\\.el\\\\'\")
                      (list (expand-file-name \"xueliang.el\" dir))))
       (done 0) (failed nil))
  (dolist (f files)
    (when (file-newer-than-file-p f (concat f \"c\"))
      (let ((byte-compile-verbose nil))
        (if (byte-compile-file f) (setq done (1+ done)) (push f failed)))))
  (when (equal (getenv \"XL_PROGRESS\") \"1\")
    (princ (format \"byte-compiled %d file(s)\\n\" done)))
  (when failed
    (message \"byte-compile failed: %S\" failed)
    (kill-emacs 1)))
"
}

# One background run: the profile is loaded so package macros and the load
# path are the real ones.  Lock, fingerprint and audit live in xl-jit.el.
cmd_jit_run() {
    require_emacs
    require_profile
    emacs_batch --eval '(progn (require (quote xl-jit)) (princ (concat (xl-jit-run-summary (xl-jit-run)) "\n")))'
}

cmd_jit_status() {
    require_emacs
    "$EMACS" --batch -Q --init-directory "$PROFILE_DIR" \
        --eval '(setq load-prefer-newer t)' \
        -L "$DOTFILES/emacs/lisp" -l xl-jit \
        --eval '(pp (xl-jit-status))'
}

cmd_jit_report() {
    require_emacs
    require_profile
    emacs_batch --eval '(progn (require (quote xl-jit-rank)) (xl-jit-report))'
}

cmd_jit_reset() {
    require_emacs
    "$EMACS" --batch -Q --init-directory "$PROFILE_DIR" \
        --eval '(setq load-prefer-newer t)' \
        -L "$DOTFILES/emacs/lisp" -l xl-jit \
        --eval '(princ (format "xl-jit reset: %d eln file(s) removed\n" (xl-jit-reset)))'
}

# Background native-compilation gate (emacs/ideas/xl-jit.md, step 1).  Runs
# in a bare batch Emacs whose state folder is the profile's, and never loads
# the profile, so it stays quick.  Always exits 0: "not available" is an answer.
cmd_jit() {
    require_emacs
    "$EMACS" --batch -Q --init-directory "$PROFILE_DIR" \
        --eval '(setq load-prefer-newer t)' \
        -L "$DOTFILES/emacs/lisp" -l xl-jit \
        -f xl-jit-platform-report
}

cmd_check() {
    cmd_smoke
    cmd_test
}

# When config advises a C primitive (evil: select-window, read-key-sequence,
# ...), a native-built Emacs compiles a trampoline for it on the main thread,
# about 1 s each, if none is cached: a cold cache gave a white frame for 12 s.
# This loads the profile (so it is a smoke test too), then runs the idle-timer
# loads (org, magit, eshell, diff-hl, ...) so their advice gets a trampoline.
cmd_trampolines() {
    require_emacs
    require_profile
    emacs_batch --eval '
(if (not (native-comp-available-p))
    (if (equal (getenv "XL_PROGRESS") "1")
        (princ "trampolines: native compilation not available\n")
      (message "profile ok; this Emacs has no native compilation, so no trampolines to build"))
  (let ((timers (copy-sequence timer-idle-list))
        (total (length timer-idle-list))
        (index 0))
    (dolist (timer timers)
      (setq index (1+ index))
      (when (equal (getenv "XL_PROGRESS") "1")
        (princ (format "trampolines %d/%d\n" index total)))
      (ignore-errors (timer-event-handler timer)))
    ;; xl-jit (step 2): build the trampolines the idle loads could not reach,
    ;; for advice added while a command runs.  Gated by the platform check.
    (require (quote xl-jit))
    (let ((built (xl-jit-trampolines-build)))
      (when (equal (getenv "XL_PROGRESS") "1")
        (princ (format "xl-jit trampolines: %S\n" built))))
    (let ((count (length (directory-files (expand-file-name comp-native-version-dir (car native-comp-eln-load-path)) nil "\\`subr--trampoline-"))))
      (if (equal (getenv "XL_PROGRESS") "1")
          (princ (format "trampolines cached: %d\n" count))
        (message "profile ok; subr trampolines cached: %d" count)))))
'
}

# Spacemacs' ~/.emacs.d moves to ~/.emacs-profiles/spacemacs (nothing deleted).
cmd_make_default() {
    case "$(uname -s)" in
        MINGW* | MSYS* | CYGWIN*) die "On Windows use: $0 setup-windows" ;;
    esac
    if [ -L "$HOME/.emacs.d" ]; then
        local target
        target=$(readlink "$HOME/.emacs.d")
        [ "$target" = "$PROFILE_DIR" ] && { say "~/.emacs.d already points to Crafted"; return 0; }
        die "~/.emacs.d is a link to $target, stop"
    fi
    if pgrep -f "Emacs.app/Contents/MacOS|bin/emacs" >/dev/null; then
        die "quit all Emacs first"
    fi
    if [ -e "$HOME/.emacs.d" ]; then
        [ -e "$PROFILES/spacemacs" ] && die "$PROFILES/spacemacs exists, stop"
        mv "$HOME/.emacs.d" "$PROFILES/spacemacs"
        say "Moved ~/.emacs.d to $PROFILES/spacemacs"
    fi
    ln -s "$PROFILE_DIR" "$HOME/.emacs.d"
    say "Default Emacs now starts Crafted (~/.emacs.d -> $PROFILE_DIR)"
}

# Undo cmd_make_default: put Spacemacs back as ~/.emacs.d.
cmd_restore_spacemacs() {
    [ -L "$HOME/.emacs.d" ] || die "~/.emacs.d is not a link to Crafted, stop"
    [ -d "$PROFILES/spacemacs" ] || die "no $PROFILES/spacemacs, stop"
    rm "$HOME/.emacs.d"
    mv "$PROFILES/spacemacs" "$HOME/.emacs.d"
    say "Spacemacs restored as ~/.emacs.d"
}

# Loading the profile installs missing packages and is the smoke test; the
# trampoline step is a separate load so each phase has its own title.
cmd_setup_macos() {
    [ "$(uname -s)" = Darwin ] ||
        die "run this on macOS; on Linux use setup-linux, on Windows setup-windows"
    progress_init 6
    progress_step "Install dotfiles" run_quiet install_dotfiles
    progress_step "Crafted profile" run_quiet cmd_crafted_install "$PROFILE_DIR"
    progress_step "Install Emacs packages" run_progress cmd_smoke
    progress_step "Prebuild advice trampolines" run_progress cmd_trampolines
    progress_step "Byte-compile own Lisp" run_quiet cmd_compile_lisp
    progress_step "Make Crafted the default Emacs" cmd_make_default
    progress_finish "macOS setup complete"
}

cmd_setup_linux() {
    case "$(uname -s)" in
        Linux) ;;
        Darwin) die "run this on Linux; on macOS use setup-macos" ;;
        MINGW* | MSYS* | CYGWIN*) die "run this in Linux; on Windows use setup-windows" ;;
        *) die "no setup for $(uname -s)" ;;
    esac
    progress_init 6
    progress_step "Install dotfiles" run_quiet install_dotfiles
    progress_step "Crafted profile" run_quiet cmd_crafted_install "$PROFILE_DIR"
    progress_step "Install Emacs packages" run_progress cmd_smoke
    progress_step "Prebuild advice trampolines" run_progress cmd_trampolines
    progress_step "Byte-compile own Lisp" run_quiet cmd_compile_lisp
    progress_step "Make Crafted the default Emacs" cmd_make_default
    progress_finish "Linux setup complete"
}

cmd_windows_check() {
    require_emacs
    say "Emacs $("$EMACS" --batch --eval '(princ emacs-version)')"
}

# Started from the Start menu, Emacs uses %HOME% as ~ and falls back to
# %APPDATA% when it is unset; pin it to the profile folder Git Bash uses.
cmd_windows_pin_home() {
    if command -v powershell.exe >/dev/null 2>&1; then
        local userhome
        userhome=$(powershell.exe -NoProfile -Command "[Environment]::GetEnvironmentVariable('HOME','User')" | tr -d '\r')
        if [ -z "$userhome" ]; then
            setx HOME "$(cygpath -w "$HOME")" >/dev/null
            say "Set user variable HOME=$(cygpath -w "$HOME") (programs started from now on see it)"
        fi
    fi
}

# Emacs prefers ~/.emacs and ~/.emacs.el over ~/.emacs.d; move old setups aside
cmd_windows_move_old_configs() {
    local backup f
    backup="$PROFILES/backup-$(date +%Y%m%d-%H%M%S)"
    for f in .emacs .emacs.el .emacs.d; do
        [ -e "$HOME/$f" ] || continue
        grep -qsE "Generated by (just emacs-crafted-install|emacs/install/(xl-emacs|emacs-setup)\.sh)" \
            "$HOME/$f/init.el" && continue
        mkdir -p "$backup"
        mv "$HOME/$f" "$backup/"
        say "Moved ~/$f to $backup"
    done
}

# Installs Crafted straight into ~/.emacs.d (Git Bash `ln -s` copies instead
# of linking), sets the HOME user variable so Start-menu Emacs finds it, moves
# any old ~/.emacs, ~/.emacs.el, ~/.emacs.d aside, then installs packages.
cmd_setup_windows() {
    case "$(uname -s)" in
        MINGW* | MSYS* | CYGWIN*) ;;
        *) [ -n "${XL_FORCE_WINDOWS:-}" ] || die "run this from Git Bash on Windows" ;;
    esac
    local d
    progress_init 6
    progress_step "Check Emacs" cmd_windows_check
    progress_step "Pin HOME for Start-menu Emacs" cmd_windows_pin_home
    progress_step "Move old configurations aside" cmd_windows_move_old_configs
    d=$(winpath "$HOME/.emacs.d")
    progress_step "Crafted profile" run_quiet cmd_crafted_install "$d"
    PROFILE_DIR="$d" progress_step "Install Emacs packages" run_progress cmd_smoke
    PROFILE_DIR="$d" progress_step "Byte-compile own Lisp" run_quiet cmd_compile_lisp
    progress_finish "Done. Start Emacs from the Start menu or run: runemacs"
}

cmd_try() {
    require_emacs
    require_profile
    "$EMACS" --init-directory "$PROFILE_DIR" "$@" &
}

usage() {
    cat <<'EOF'
usage: emacs-setup.sh COMMAND [ARGS]

setup:
  setup-macos            install dotfiles + Crafted profile, make it ~/.emacs.d
  setup-linux            same on Linux
  setup-windows          same on Windows (run in Git Bash, into ~/.emacs.d)
  crafted-install [DIR]  clone Crafted at the locked commit, write stubs

use / check:
  try [ARGS]             start Emacs with the profile, passing ARGS through
  smoke                  load the profile in batch; fails on any init error
  test                   run the ERT suite against the loaded profile
  check                  smoke + test; run this after a config change
  trampolines            pre-build advice trampolines so a GUI start is fast
  jit                    say whether xl-jit can run here (native compilation)
  compile-lisp           byte-compile our own lisp/ and xueliang.el where stale
  jit-run                one background xl-jit run (audit, then work when due)
  jit-reset              delete the elns xl-jit recorded, and its ledger
  jit-report             print the xl-jit ranking; compiles nothing
  jit-status             ledger counts, eln bytes, and the last run's result

profile:
  make-default           point ~/.emacs.d at the profile (Spacemacs moves aside)
  restore-spacemacs      undo make-default

environment: EMACS, PROFILE, PROFILES, PROFILE_DIR, DOTFILES,
             XL_SKIP_DOTFILES=1 (skip env.sh), XL_FORCE_WINDOWS=1,
             XL_VERBOSE=1 (show each step's output; setup hides it by default)
EOF
}

case "${1:-}" in
    crafted-install) shift; cmd_crafted_install "$@" ;;
    smoke) shift; cmd_smoke "$@" ;;
    test) shift; cmd_test "$@" ;;
    check) shift; cmd_check "$@" ;;
    trampolines) shift; cmd_trampolines "$@" ;;
    jit) shift; cmd_jit "$@" ;;
    compile-lisp) shift; cmd_compile_lisp "$@" ;;
    jit-run) shift; cmd_jit_run "$@" ;;
    jit-reset) shift; cmd_jit_reset "$@" ;;
    jit-report) shift; cmd_jit_report "$@" ;;
    jit-status) shift; cmd_jit_status "$@" ;;
    make-default) shift; cmd_make_default "$@" ;;
    restore-spacemacs) shift; cmd_restore_spacemacs "$@" ;;
    setup-macos) shift; cmd_setup_macos "$@" ;;
    setup-linux) shift; cmd_setup_linux "$@" ;;
    setup-windows) shift; cmd_setup_windows "$@" ;;
    try) shift; cmd_try "$@" ;;
    help | -h | --help) usage ;;
    *) usage; exit 2 ;;
esac
