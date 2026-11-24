# xl-jit 探针

[`../xl-jit.md`](../xl-jit.md) 里的数字都是用这几个脚本测出来的。它们是实验代码，不属于配置，init.el 不会加载它们。实现 xl-jit 时可以从这里起步，但要按设计文档补上下面列出的缺口。

所有脚本都只读你的 profile，写入只发生在临时目录里。

## 运行

排名和 trampoline 两个探针需要先跑完一遍 init。为了不让 savehist、recentf、persistent-scratch 写进真实的 profile，在一个临时 init 目录里跑。elpa 和 eln-cache 用符号链接指向真实目录，探针只读不写：

```bash
P=~/.emacs-profiles/crafted
SB=$(mktemp -d -t xl-jit-sandbox)
cp $P/early-init.el $P/init.el $SB/
ln -s $P/elpa-31.1 $SB/elpa-31.1
ln -s $P/eln-cache $SB/eln-cache
PROBE_PROFILE=$P emacs --batch --init-directory $SB -l $SB/early-init.el -l $SB/init.el -l emacs/ideas/xl-jit-probes/xl-jit-probe-rank.el
```

把最后一行的 `xl-jit-probe-rank.el` 换成 `xl-jit-probe-trampolines.el`，就是 trampoline 预测。

另外两个不需要 init：

```bash
bash emacs/ideas/xl-jit-probes/xl-jit-probe-eln-rules.sh
```

```bash
emacs --batch -Q -l emacs/ideas/xl-jit-probes/xl-jit-probe-bench.el
```

## 脚本

| 文件 | 测什么 | 2026-11-24 的结果 |
| --- | --- | --- |
| `xl-jit-probe-rank.el` | 从字节码和 savehist/recentf 推出热点文件排名 | 5792 个函数、213 个根节点，约 2.4 秒 |
| `xl-jit-probe-trampolines.el` | 把 .elc 当作数据读，预测需要的 trampoline，并和缓存对比 | 1543 个 .elc，约 10 秒；预测 15 个，已缓存 9 个，缺 6 个 |
| `xl-jit-probe-eln-rules.sh` | 用一个玩具包测 `load` 在什么情况下会用 eln | 14 种情况，结果见设计文档里的表 |
| `xl-jit-probe-bench.el` | `xl-git-status` 的两个函数在解释执行、字节码、native 下的耗时 | 字节码比解释执行快 ×1.6 到 ×2.9；native 和字节码的差别在 ×0.9 到 ×1.1 之间，属于测量噪声 |

## 已知缺口（正式实现时要补上）

`xl-jit-probe-rank.el`：
- 函数按 `load-history` 归属到文件，所以 `*-autoloads.el` 会上榜。要改成 `symbol-file`。
- savehist 里单个命令的频次没有封顶，所以 `package.elc` 排到了第一。
- 收益分没有扣除内存分配类调用（`substring`、`concat`、`format`、`split-string`）。
- 没有尊重 `no-native-compile`、`no-byte-compile` 和 `native-comp-jit-compilation-deny-list`。
- 没有可信根目录检查。它只做分析、不编译，所以暂时无害；正式实现里编译之前必须检查。

`xl-jit-probe-trampolines.el`：
- 剩下的 `append`、`type-of` 两个预测可能是误报。按设计"宁可多编"，不需要排除，但要设数量上限。
- 只输出报告，不编译。正式实现用 `comp-trampoline-compile`。
