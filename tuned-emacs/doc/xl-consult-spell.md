# Async spell checking: `xl-consult-spell`

`xl-consult-spell` puts an ispell-compatible spell checker behind a consult list. The checker runs asynchronously; its misspelled words become consult candidates. Typing narrows the list, preview follows the selection, and `RET` jumps to the occurrence.

The point is to replace the synchronous `ispell-buffer` loop with a list that behaves like `consult-line` or `consult-ripgrep`, without adding another spell-checking engine.

## Commands

| Command | Scope |
| --- | --- |
| `xl-consult-spell` | The whole buffer, including unsaved text |
| `xl-consult-spell-region` | The active region, widened to whole lines |
| `xl-consult-spell-doctor` | Which checker Emacs found and how to install one |

Default bindings are in the `SPC e` errors group:

| Key | Command |
| --- | --- |
| `SPC e t` | `xl-consult-spell` |
| `SPC e r` | `xl-consult-spell-region` |

## How it works

The program is chosen from `ispell-program-name` first, then `aspell`, `hunspell` and `ispell`. Each one is run in list mode:

| Program | Arguments |
| --- | --- |
| `aspell` | `list --encoding=utf-8` |
| `hunspell` | `-l -i utf-8` |
| `ispell` | `-l` |

The buffer text is written to standard input through `xl-async-run`. When the checker exits, its words are located in the buffer in the order they were printed. A word that cannot be found is skipped; the rest keep their positions.

The finished list is passed to `consult--read`:

- candidates are grouped by buffer with `consult--prefix-group`;
- preview and `RET` use `consult--jump-state` on a marker at the word;
- `C-n` / `C-p` move through the candidates;
- `M-x xl-consult-spell-doctor` explains a missing checker.

List mode reports words without positions, so the positions are reconstructed by searching the buffer. This is reliable for ordinary prose and comments. A word that appears several times is matched in order. A word made only of punctuation falls back to a plain search.

## Corrections

The list is for navigation. After `RET`, correct the word with `ispell-word` (`M-$`) or edit it directly. There is no correction menu inside the consult list, because list mode does not return suggestions.

This is the deliberate trade-off for keeping the implementation small and using only the checker that is already installed.

## Cross-platform rules

The module uses:

- argument lists, never `sh -c` or a shell string;
- `executable-find` for every program;
- UTF-8 process coding for the checker;
- CR removal for Windows output;
- `xl-async` for stdin and ownership;
- word boundaries and a plain-search fallback for positions.

There is no `xargs`, `grep`, `/tmp` or unexpanded `~`. `aspell` and `hunspell` have Windows builds; `ispell` is available on macOS and most Linux systems.

## Why not a project scan

One checker process per file is slow, and the reconstructed positions become less reliable across many files. The supported scopes are the buffer and the region; use `consult-ripgrep` or a project-wide CLI if a repository-wide check is needed.

## Doctor

`M-x xl-consult-spell-doctor` shows the selected program, its path and version, and the install hint when nothing was found. It does not start a checker.

## Tests

`tests/xl-consult-spell-tests.el` covers CRLF removal, candidate properties and grouping, marker lookup, repeated words, word boundaries, apostrophes, region widening, per-program arguments, program discovery, the no-checker error and the doctor output.

The tests do not require a real checker; program discovery and version output are mocked.
