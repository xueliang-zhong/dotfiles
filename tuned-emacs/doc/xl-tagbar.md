# Tagbar: `lisp/xl-tagbar.el`

A Tagbar for Emacs: a narrow side window that lists the imenu tags of the
buffer it watches, grouped into sections, with the tag enclosing point
highlighted. `F8` toggles it (`xl-tagbar-toggle`).

The list is not a second imenu implementation. It reads the candidates of
`consult-imenu` (`consult-imenu--items-safe`), so it inherits the sections,
the faces and the mode-specific configuration from
`consult-imenu-config`. Jumping uses `consult-imenu--jump`, which handles
both plain markers and the special targets of some backends. With Embark
loaded, `C-.` on a line acts on the tag name: `embark-act` can describe the
symbol, find its references, copy it, and so on.

Any buffer with an imenu backend works. In Emacs Lisp the sections are
Functions, Variables, Macros and the like; in Org the list is the heading
tree. A buffer without imenu shows `No tags in this buffer`.

```
▾ 𝑥 Variables
  𝑥 my-var
▾ ƒ Functions
  ƒ my-fun
```

## Icons

Each section and tag is prefixed with an icon for its kind: functions
and methods, variables and fields, macros, constants, types, classes,
interfaces, enums, structs, namespaces, modules, packages, headings and
a generic tag. The kind comes from the `consult-imenu` type key when the
mode has a `consult-imenu-config` entry, and from the section title
otherwise, so modes without a type configuration still get sensible
icons.

`xl-tagbar-icon-style` selects the glyphs:

- `auto` (default) uses the Nerd Font icons when
  `xl-tagbar-nerd-font` (default `Symbols Nerd Font Mono`) is installed
  and the frame is graphical, and the Unicode fallback otherwise.
- `nerd` forces the Nerd Font glyphs, for a terminal whose font is
  patched.
- `unicode` forces the fallback symbols (`ƒ`, `𝑥`, `μ`, `◈`, `▣`, ...).
- `none` turns icons off.

The Nerd Font glyphs are drawn with an explicit `:family` face, so the
main programming font does not need to be patched. `xl-tagbar-icons`
maps each kind to its `(KIND NERD-FONT UNICODE)` pair; add a kind or
change a glyph there. The icon colour comes from the theme's
`font-lock` face for the kind.

## Keys

| Key | Action |
| --- | --- |
| `RET`, `mouse-1` | Jump to the tag in the source window |
| `n`, `p` | Preview the next or previous tag, keeping the tagbar selected |
| `TAB` | Fold or unfold the section heading at point |
| `g` | Rebuild the list now |
| `q` | Close the sidebar |
| `C-.` | Embark actions on the tag at point |

`F8` toggles the sidebar: it opens it for the selected window, or closes it
when it is already open.

## Following the buffer

The sidebar is attached to the window that was selected when it opened,
not to whatever window is selected later. These events update it:

- **Edits** are collected and rebuilt after `xl-tagbar-refresh-delay`
  (0.3 s) of idle time, using the modified tick to skip a rebuild when
  nothing changed.
- **Saves and reverts** rebuild at once.
- **A buffer switch in the watched window** re-attaches the sidebar to the
  new buffer.
- **Point movement** moves the current-tag overlay and scrolls the sidebar
  enough to keep the tag visible. `xl-tagbar-follow-point` turns this off.

The current tag is the last tag that starts at or before point. Nested
imenu items (Org subheadings) therefore select the innermost entry.

## Window and buffer

There is one `*Tagbar*` buffer, in `xl-tagbar-mode` (derived from
`special-mode`). It opens in a dedicated side window on the right with
`xl-tagbar-width` columns (34 by default); `xl-tagbar-side` selects the
left side instead. The window width is preserved across frame resizes.

The tagbar does not take the selection: opening it, refreshing it and
following point all leave focus where it was. `n`, `p` and `RET` move the
source window's point; only `RET` leaves the tagbar for the source buffer.

All hooks are buffer-local and are removed when the sidebar closes, so a
buffer that was watched once carries no cost afterwards.

## Embark target

`xl-tagbar--embark-target` is prepended to `embark-target-finders` when
Embark loads. On a tag line it returns `(symbol . NAME)` with the bare name
(the section prefix is not part of the name), which gives Embark the
function, variable and symbol actions instead of a generic line target.

## Tests

`tests/xl-tagbar-tests.el` covers the sections and their faces, jumping,
next/previous preview, folding, idle refresh, the current-tag overlay,
opening and closing the side window, the Embark target and the keymap.
