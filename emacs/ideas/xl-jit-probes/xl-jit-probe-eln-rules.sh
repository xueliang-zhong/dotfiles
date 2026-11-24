#!/bin/bash
# Probe, not part of the config: when does `load' use an .eln?
# Builds a toy package and a private eln cache in a temporary folder, then
# prints one line per case from the "eln 的版本和加载规则" table in
# ../xl-jit.md.  Touches nothing outside that folder.
set -e

EMACS="${EMACS:-emacs}"
LAB="$(mktemp -d -t xl-jit-eln-lab)"
trap 'rm -rf "$LAB"' EXIT
mkdir -p "$LAB/real/pkg" "$LAB/cache"
ln -s "$LAB/real" "$LAB/link"
PKG="$LAB/real/pkg"

printf ';;; foo.el --- t -*- lexical-binding: t; -*-\n(defun foo-f (n) (let ((s 0)) (dotimes (i n) (setq s (+ s i))) s))\n(provide (quote foo))\n' > "$PKG/foo.el"

# Batch Emacs whose first eln directory is the private cache.
lab_emacs() {
    "$EMACS" --batch -Q \
        --eval "(setq native-comp-eln-load-path (list \"$LAB/cache/\" (car (last native-comp-eln-load-path))) native-comp-jit-compilation nil)" \
        "$@" 2>&1 | grep -v '^Loading\|^Compiling\|^Wrote' || true
}

# check LABEL DIR [EXTRA-SETUP]: load foo from DIR and say whether foo-f is native.
check() {
    lab_emacs --eval "(progn $3 (add-to-list 'load-path \"$2\")
      (condition-case e
          (progn (require 'foo)
                 (princ (format \"%-40s native=%S\n\" \"$1\" (native-comp-function-p (symbol-function 'foo-f)))))
        (error (princ (format \"%-40s error: %S\n\" \"$1\" (car e))))))"
}

byte_compile() { lab_emacs --eval "(byte-compile-file \"$PKG/foo.el\")" > /dev/null; }
native_compile() { lab_emacs --eval "(native-compile \"$1\")" > /dev/null; }
eln_file() { ls "$LAB"/cache/*/foo-*.eln | head -1; }

byte_compile
check "elc only, no eln" "$PKG"
native_compile "$PKG/foo.el"
check "eln built, load via real path" "$PKG"
check "eln built, load via symlink path" "$LAB/link/pkg"

sleep 1; touch "$PKG/foo.elc"
check ".elc newer than .eln" "$PKG"
native_compile "$PKG/foo.el"
check ".eln rebuilt after that" "$PKG"

mv "$PKG/foo.el" "$LAB/foo.el.bak"
check "no .el source" "$PKG"
gzip -c "$LAB/foo.el.bak" > "$PKG/foo.el.gz"
check "only .el.gz source" "$PKG"
rm "$PKG/foo.el.gz"; mv "$LAB/foo.el.bak" "$PKG/foo.el"

echo ';; edited' >> "$PKG/foo.el"; byte_compile
check "source edited, eln not rebuilt" "$PKG"
native_compile "$PKG/foo.el"
echo "eln files after rebuilding the edited source: $(ls "$LAB"/cache/*/ | wc -l | tr -d ' ')"

sleep 1; touch "$PKG/foo.el"
check ".el newer than .elc, load-prefer-newer t" "$PKG" "(setq load-prefer-newer t)"

"$EMACS" --batch -Q --eval "(progn (add-to-list 'load-path \"$PKG\") (require 'foo)
  (princ (format \"%-40s native=%S\n\" \"default eln cache (-Q)\" (native-comp-function-p (symbol-function 'foo-f)))))" 2>&1 | tail -1

ELN="$(eln_file)"; cp "$ELN" "$LAB/good.eln"
head -c 4000 "$LAB/good.eln" > "$ELN"
check "truncated eln" "$PKG"
: > "$ELN"
check "empty eln" "$PKG"
check "empty eln, load-no-native t" "$PKG" "(setq load-no-native t)"

printf ';;; -*- lexical-binding: t; -*-\n(defun foo-f (_n) 0)\n(provide (quote foo))\n' > "$LAB/other.el"
lab_emacs --eval "(native-compile \"$LAB/other.el\" \"$LAB/other.eln\")" > /dev/null
cp "$LAB/other.eln" "$ELN"
check "eln built from another source" "$PKG"
