# eln-opt probes

The numbers in [`../eln-opt.md`](../eln-opt.md) come from these scripts. They are experiments, not part of the configuration: `init.el` never loads them. They are a starting point if the eln-opt implementation is reworked, but the gaps listed below must be filled first.

Every script reads the profile only. Anything written goes to a temporary directory.

## Running

The ranking and trampoline probes need an initialised profile. To keep savehist, recentf and persistent-scratch from writing to the real profile, run them in a temporary init directory. `elpa-31.1` and `eln-cache` are symbolic links to the real directories, and the probes only read them:

```bash
P=~/.emacs-profiles/crafted
SB=$(mktemp -d -t eln-opt-sandbox)
cp $P/early-init.el $P/init.el $SB/
ln -s $P/elpa-31.1 $SB/elpa-31.1
ln -s $P/eln-cache $SB/eln-cache
PROBE_PROFILE=$P emacs --batch --init-directory $SB -l $SB/early-init.el -l $SB/init.el -l tuned-emacs/doc/eln-opt-probes/eln-opt-probe-rank.el
```

Replace the last file name with `eln-opt-probe-trampolines.el` to run the trampoline prediction.

The other two need no initialised profile:

```bash
bash tuned-emacs/doc/eln-opt-probes/eln-opt-probe-eln-rules.sh
```

```bash
emacs --batch -Q -l tuned-emacs/doc/eln-opt-probes/eln-opt-probe-bench.el
```

## Scripts

| File | What it measures | Result (2026-11-24) |
| --- | --- | --- |
| `eln-opt-probe-rank.el` | Ranks hot files from byte code and savehist/recentf | 5792 functions, 213 roots, about 2.4 s |
| `eln-opt-probe-trampolines.el` | Reads `.elc` files as data, predicts the trampolines needed, and compares with the cache | 1543 `.elc` files, about 10 s; 15 predicted, 9 cached, 6 missing |
| `eln-opt-probe-eln-rules.sh` | Uses a toy package to test when `load` uses an eln | 14 cases; results in the table of the design document |
| `eln-opt-probe-bench.el` | Time of two `xl-git-status` functions interpreted, byte-compiled and native | Byte code is 1.6 to 2.9 times faster than interpreted; native differs from byte code by 0.9 to 1.1 times, within measurement noise |

## Known gaps (to fill when implementing for real)

`eln-opt-probe-rank.el`:

- Functions are attributed to files through `load-history`, so `*-autoloads.el` files appear in the ranking. Use `symbol-file` instead.
- The frequency of each command in savehist is not capped, so `package.elc` came first.
- The benefit score does not subtract allocating calls (`substring`, `concat`, `format`, `split-string`).
- `no-native-compile`, `no-byte-compile` and `native-comp-jit-compilation-deny-list` are not respected.
- There is no check against trusted roots. The probe only analyses and never compiles, so this is harmless for now; the real implementation must check before compiling.

`eln-opt-probe-trampolines.el`:

- The predicted `append` and `type-of` entries may be false positives. The design accepts over-compiling rather than excluding them, but a count limit is needed.
- The probe only reports; it does not compile. The real implementation uses `comp-trampoline-compile`.
