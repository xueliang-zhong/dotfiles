#!/usr/bin/env bash
# Compatibility shim for the old emacs-setup.sh command names.
# Use tuned-emacs/bin/tuned; this file is kept for one version.
set -euo pipefail

script_dir=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd -P)
new=$(cd "$script_dir/../bin" && pwd -P)/tuned
cmd=${1:-}

case "$cmd" in
    setup-macos | setup-linux | setup-windows | crafted-install) hint=install ;;
    smoke | autoloads | compile-lisp | trampolines) hint=sync ;;
    test) hint=test ;;
    try) hint=run ;;
    make-default) hint=default ;;
    restore-spacemacs) hint=restore-spacemacs ;;
    *) hint=$cmd ;;
esac

if [ "$cmd" = check ]; then
    printf 'emacs-setup.sh is deprecated; use: %s sync && %s test\n' \
        "$new" "$new" >&2
else
    printf 'emacs-setup.sh is deprecated; use: %s %s\n' "$new" "$hint" >&2
fi

exec "$new" "$@"
