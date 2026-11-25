# eln-opt：后台的分层编译

状态：已实现并在使用。实现在 `tuned-emacs/lisp/eln-opt.el`，测试在 `tuned-emacs/tests/eln-opt-tests.el`。
数字来自 Emacs 31.1（Mac，Emacs.app）和 Emacs 30.1（Raspberry Pi 4，4 核 3.8 GB）。

## 目标

Emacs 的代码有三种运行状态：源码解释执行、字节码、native（eln）。eln-opt 在后台把用得多的代码提升到更快的状态，而不让用户感觉到：

- **透明**：运行中的 Emacs 不加 hook、不加 advice、不做分析，只有一个一次性的 idle timer。分析和编译都在后台子进程里做。
- **轻**：启动时的开销很小，空闲时只做一次指纹比对；真正的工作只在指纹变化时发生。
- **只在后台、低优先级地编**，一次一个文件，每个文件之间休息。
- **安全**：不加载不可信的代码，不因为版本不匹配反复编译，中断能恢复，出问题能关掉。
- **新结果下次启动生效**，不在当前会话里热替换。

不做的事：不改 Emacs 的 C 代码；不依赖外部程序；不替代 Emacs 自带的编译机制，只补它没覆盖的部分。

## 三层

| 层 | 状态 | 谁负责提升 |
|---|---|---|
| 0 | 源码解释执行（没有 `.elc`，或 `.elc` 比源码旧） | 目标是离开这一层 |
| 1 | 字节码 `.elc` | worker 的 byte-compile，所有平台 |
| 2 | native `.eln` | worker 的 native-compile，只在支持的平台，只给热文件 |

Tier 2 的门槛应按实测收益定，而不只看热不热。循环密集的纯 Lisp 代码 native 通常快 1 到 3 倍；大量调用 C 原语或分配内存的代码几乎没有差别（`xl-git-status--entries` 字节码比解释快 2.9 倍，native 只比字节码再快 1.1 倍）。所以分数低、分配多的文件不值得 native。

## 用户能做什么

| 命令 | 作用 |
|---|---|
| `M-x eln-opt-compile-now` | 立刻启动 worker（绕过空闲和安静状态），显示 `*eln-opt*` |
| `M-x eln-opt-compile-function` | 编译某个函数所在的源文件（字节码，然后 native），不受安静状态和失败列表影响 |
| `M-x eln-opt-report` | 显示排名、下一次要编的文件、已经是 Tier 1/2 的文件、为什么某些文件没编 |

  报告的连续调用：子进程还在算就只显示它；60 秒内且指纹没变就直接显示上次结果（`eln-opt-report-cache-seconds`）。
  手动启动：worker 结束后 30 秒内拒绝再次启动（`eln-opt-manual-gap`）。
| `M-x eln-opt-stop-worker` | 立刻停止正在跑的后台 worker；正在编译的文件保留进行中标记（下次当作被打断的尝试），本次会话不再自动启动，`eln-opt-compile-now` 仍然可用 |
| `M-x eln-opt-reset` | 删除 eln-opt 生成的 eln、失败列表、进行中标记、安静状态。先要求输入 `yes` |

命令行：`bash tuned-emacs/install/emacs-setup.sh eln-opt`（平台是否可用）、`eln-opt-compile`、`eln-opt-report`、`eln-opt-status`、`eln-opt-reset`、`compile-lisp`（我们自己的 lisp 编成字节码，不依赖 libgccjit）。

## 启动到编译：时间线

启动时（`init.el` 里 `require` 之后）：

1. 加载 `eln-opt.el`：约 20 毫秒（字节码，Pi）。comp.el **不在这里加载**，只在需要时（worker、trampoline、reset）才加载。
2. `eln-opt-auto-arm` 挂一个一次性的 idle timer，时长 `eln-opt-auto-idle`（默认 5 秒）。没有轮询，也不做分析。batch 运行时（setup、测试、worker 自己）什么都不挂。

idle timer 触发时（只在 Emacs 连续空闲满那么久之后）：

10a. 一次运行编译了东西（有 `done`），就重新挂一次 idle timer，下一段空闲时继续；一个会话最多 `eln-opt-auto-max-runs`（60）次。没有新工作时记为安静状态，之后不再启动。
3. 决定是否启动（`eln-opt-auto--decision`，约 0.1 毫秒）：`ELN_OPT=0` 直接禁用；`eln-opt-auto-enable` 为 nil 禁用；已有 worker 在跑；逻辑核少于 `eln-opt-min-processors`（2）就不自动启动（单核上 worker 和会话抢同一个核，单核用户可以手动运行 `eln-opt-compile-now`）；**安静状态**：上次 worker 没有找到任何事要做，并且指纹没变。
4. 指纹（`eln-opt--quiet-fingerprint`，约 1.5 毫秒）：Emacs 二进制、`load-path`、我们自己的 lisp 文件的修改时间。它不包含用法数据（savehist、recentf）。
5. 启动 worker：同一个 Emacs 二进制，同一个 profile，`taskpolicy -b`（macOS）或 `nice -n 19`。一个 watchdog 每 60 秒检查一次输出。

worker（后台批处理 Emacs）：

6. 拿锁（`owner.eld` 记 pid 和时间；pid 不存在或超过 1 小时就视为过期，接管）。拿不到锁就输出 `busy` 退出。
7. 读取残留的进行中标记（见"保护"）。
8. 平台判断（缓存在 `platform.eld`，key 是二进制的路径、大小和 mtime）。
9. 排计划：
   - 字节码候选：以源码加载、没有最新 `.elc` 的文件（先加载懒加载的根，比如 `org`，再计算）；
   - native 候选：平台支持时，做完整分析（Pi 上约 6 秒），按性价比（ratio = 分数 ÷ 大小 KB）从高到低，取 ratio ≥ 3 且没有 eln 的文件；
   - 合计最多 30 个，字节码在前。计划为空就记为安静状态，然后退出。
10. 逐个处理，每两个文件之间睡 5 秒（`eln-opt-worker-sleep`）：写进行中标记 → 编译 → 删除标记 →（native 成功时）预热新 eln → 输出 `done` 或 `failed` 并记入失败列表。失败的文件记入失败列表，worker 继续编下一个，所以一个坏文件不会挡住其他文件。
10b. 平台支持 native 时，把整个 eln 目录预热一遍（见"首次加载检查"），覆盖别的途径生成的文件，以及 worker 在编译和预热之间被停掉的文件。已经检查过的文件每个约 3 毫秒，77 个约 0.6 秒。
11. 结束：输出 `finished N`（有失败时 `finished N, failed M`），释放锁。主 Emacs 在回显区显示 `compiled N file(s); they load on the next start`。
12. 下次启动：Emacs 自己加载新的 `.elc` 和 eln。

Pi 上的大致耗时：每个文件的编译（小文件几秒，org 约 1 分钟）加上 5 秒间隔，30 个文件大约 5 分钟以上。

## 候选规则

一个文件要进入计划，必须同时满足：

- **可信**：`file-truename` 在可信根下（elpa、Emacs 的 `lisp-directory`、dotfiles 的 `tuned-emacs/lisp`、`tuned-emacs/xueliang.el`）；指向根外的符号链接被拒绝。
- **有源码**：`.el`，或者 Emacs 自带库的 `.el.gz`（Tier 2）。
- **定义了函数**，不是 autoloads、loaddefs（只有桩函数），也不是 init.el、early-init.el、custom.el（过期的 `init.elc` 是常见的坑）。
- **没有 opt-out**：文件里没有 `no-native-compile: t` 或 `no-byte-compile: t`。
- **不在失败列表里**（强制编译除外）。
- Tier 2 额外要求：分数与成本的比（ratio）≥ `eln-opt-min-ratio`（3.0），源码解压后 ≤ `eln-opt-max-source-bytes`（1 MB）。

## 排名（可达性分析）

分析只读数据，不执行任何代码，也不编译任何东西。

**根**：用户实际用过的命令（savehist，单个命令的权重有上限）、常用 major mode（recentf 的文件经 `auto-mode-alist` 映射）、常用 mode 的 hook 和 keymap、每次按键或重绘都会跑的 hook（`post-command-hook` 等，权重最高）、全局和 leader keymap（权重很低）。某个 mode 的 keymap 和 hook 只有在 mode 出现在常用集合里时才算根，这样大量很少用的 mode 设置会被剪掉。懒加载的重要根（权重 ≥ 0.5）会先加载，否则它们的函数不在图里。

**冷启动的基线**：新机器上 savehist 和 recentf 还是空的，排名没有依据。`tuned-emacs/data/eln-opt-baseline.eld` 列出一般都值得处理的函数名（每次按键都会跑的 evil、winum hook，completion 栈，以及已知的 trampoline 原语），每个作为权重 1.0 的根。随着用户自己的数据增长，它的权重按比例下降，用户的命令和文件数达到 `eln-opt-rank-baseline-users`（50）时完全不再起作用。文件只含函数名和原语名，不含代码或路径。

**边**：函数常量向量里的函数符号（嵌套闭包递归）；读取了变量并且调用 `funcall`/`apply` 时，变量的值如果是函数（比如 `completion-at-point-functions`），连边；有 advice 的函数双向连边。看不到的部分（运行时才创建的 lambda 和 timer、进程的 filter 和 sentinel）不计入。

**打分**：
- 热度沿边传播：`R(被调用者) += 0.5 · R(调用者) / sqrt(出边数)`，最多 3 层（`eln-opt-rank-heat-levels`）；
- 收益 B：从字节码（LAP）里算。先用向后跳转找出循环体，循环体里的指令按 `4^嵌套深度` 加权（递归视为多一层）；内联运算（`car`、`cdr`、算术、`eq`、`aref` 等）加分；buffer 操作、C 重活（`insert`、`re-search-*`、`string-match`……）和分配类调用（`substring`、`concat`、`format`、`split-string`……）扣分；
- 文件分 = Σ R·B；ratio = 分 ÷ 字节码大小（KB）；
- 已经是 native 的函数跳过；函数归属用 `eln-opt--defun-files`（一次扫描 `load-history` 建的表，与 `symbol-file` 对全部 native 函数一致）。

系数（4.0、0.5、各操作码的分值）是经验值，没有离线校准。校准需要离线跑 profiler，用户那边不运行 profiler。

## 平台

- 唯一的判断依据是 `native-comp-available-p`，不按操作系统名。不可用时（没有 libgccjit 的 Windows 构建等），worker 仍然启动，但只做字节码编译，报告里仍然显示同样的 Status 表（Tier 2 为 0）。
- 带了 native 编译但编不出来（libgccjit 版本不对、驱动或链接器出错）：用一个单函数的金丝雀文件判断一次，结果缓存；失败就记为不可用，直到二进制变化。
- comp.el 只在需要时加载。`comp-native-version-dir`、`native-comp-eln-load-path`、`comp-el-to-eln-filename` 在 `-Q` 下本来就是预加载的，所以启动路径不需要它。
- 没有 comp 时（比如某些 Windows 构建），状态目录名用二进制的 key 生成，而不是 `comp-native-version-dir`，因为后者这时未定义。

## eln 的加载规则（实测）

| 情况 | 会用 eln 吗 |
|---|---|
| eln 存在，经真实路径或符号链接加载 | 会 |
| 源码内容改了（文件名里的内容哈希变了） | 不会；下次编译时 comp 会删掉同一路径的旧 eln |
| `.elc` 比 `.eln` 新 | **不会，也没有任何提示** |
| 只有 `.elc`，没有 `.el` | 不会（`.el.gz` 例外，见下） |
| 只有 `.el.gz`（Emacs 自带库） | 会：`native-compile` 对 `.el.gz` 算出的 eln 名字就是 Emacs 要找的那个 |
| eln 写进了别的 eln-cache（`-Q` 默认写 `~/.emacs.d/eln-cache`） | 永远不会 |
| eln 损坏（随机字节、截断、空文件） | `require` 报 `native-lisp-load-failed`，不会退回 `.elc` |
| 名字对、内容来自别的源码 | 静默加载 |
| 设置了 `load-no-native` | 跳过所有 eln，回到 `.elc`；预加载的核心代码仍是 native |

eln 文件名是 `<名字>-<路径哈希>-<内容哈希>.eln`，放在 `eln-cache/<comp-native-version-dir>/` 下。版本目录随二进制构建变化（Emacs.app 里有 `31_1-032f33bc` 和 `31_1-38fe94a6` 两个）。comp 写 eln 时先写临时文件再 `rename`，正在运行的 Emacs 不会崩溃。

`eln-opt` 只通过 comp 写入（`native-compile`、`comp-trampoline-compile`、`comp-delete-or-replace-file`），从不 `cp` 或原地覆盖。

**内置库的 `.el.gz`**：Emacs 自带的 org、winner、which-key、savehist、repeat、delsel 等只有 `.el.gz`。直接对 `.el.gz` 调用 `native-compile` 就能得到 Emacs 要找的 eln，不需要改启动流程。用完整路径 `load` 一个 `.elc` 永远加载字节码，只有不带后缀的 `load`（`require` 就是这样）才会去找 eln。

## 特征缓存

图的构建（每个函数的特征，约 5000 个）按源文件缓存在 `eln-opt/<版本>/features.eld`：

- 指纹：每个 load 文件内容的 SHA-256（读 600 个文件、19.5 MB 只要 0.08 秒）。分析代码的指纹是加载进来的 `eln-opt.el`（`.elc` 或 `.el`）的 SHA-256；代码变了，所有缓存失效。
- 本次运行先查本次结果，再查磁盘；同一个文件的指纹每次运行只算一次。
- 安全：只用 `read`（`read-eval` 关闭）；文件超过 20 MB、是符号链接、版本或分析指纹不符、或任何一项形状不对，整个文件都不用。
- 可靠：写临时文件再 rename，读者只会看到旧文件或新文件；本次没看到的文件从缓存中删除。
- 开关：`ELN_OPT_FEATURE_CACHE=0` 关掉缓存（读和写都不做）。
- 实测（同一个 Emacs，真实 profile）：图的构建冷启动 5.26 秒，热启动 0.65 秒，没有重新分析任何函数；缓存关闭和缓存热启动的结果逐字节相同（5127 个函数）。

剩下的 0.6 秒是 `no-native-p`：它为每个文件读一遍来查 opt-out 标记，可以和指纹的读取合并。

## 录制（perfetto）

主 Emacs 用 `xl-perfetto-trace-start` 录制时，worker 在两个文件之间检查标记文件 `traces/recording.eld`，有就同时录制，写出 `eln-opt-trace-*.json`。worker 的每个阶段（可达性分析的各步、候选、每个文件的编译、每个顶层表单、warm）都是一个 span。细节见 `ideas/xl-perfetto-trace.md`"跟随 eln-opt worker"一节。

## 首次加载检查（macOS）

macOS 在任何进程第一次映射一个新的原生文件时检查它：每个新 `.eln` 约 0.22 秒，跟大小关系不大（37 KB 和 410 KB 都是这样），结果按文件缓存，跨进程有效；再加载只要约 3 毫秒。缓存按文件而不是按内容：把一个检查过的 eln 复制到新路径，第一次加载又是 0.22 秒。

后果：worker 编完一批 eln 后，下一次 GUI 启动是第一个加载它们的进程，这笔开销就落在主线程上。

实测（MacBook Air，正常启动加载用户 eln 目录里的 31 个文件）：

| 情况 | GUI init |
|---|---|
| 都已检查过 | 1.19 秒 |
| 31 个都是新文件（内容相同的新副本） | 7.48 秒 |
| 新文件，先在后台 `emacs -Q` 加载一遍 | 0.90 秒 |

做法（`eln-opt--warm`）：worker 每编出一个 eln，就用一个一次性的 `emacs -Q --batch` 加载它一次；运行结束时再对整个目录做一次。子进程里 `load-path` 是 nil，文件的顶层代码在第一个 `require` 就停下，目的只是触发系统检查，不是运行代码；stdin 立即关闭，超过 `eln-opt-warm-timeout`（120 秒）就停掉。只在 macOS 上做。worker 在 `taskpolicy -b` 下运行，后台优先级下系统检查慢很多（31 个文件约 8 秒，前台约 0.4 秒），但不占主线程。

更正：用户实际遇到的几次 13 秒启动，起初归因于这里，后来慢启动记录器（`early-init.el`）的日志显示，12.1 秒卡在 ffap 加载时运行的 `gcc`/`clang`/`g++` 上，与 eln 无关，原因和修法见 `tuned-emacs/README.md` 的 `early-init.el` 一节。这里的首次加载开销是真实的，用新副本复现过，但不是那几次慢启动的原因。

Trampoline 由 `comp-trampoline-compile` 编译后当场加载，已经在编译它的进程里检查过，不需要另外预热。

## Trampoline

advice 或重定义 C 原语时需要 trampoline（`subr--trampoline-*`）。缓存里没有时，第一次会在主线程同步编译约 1 秒，界面冻结。所以要预先编译。

- **预测**：把 `.elc` 当数据读（不加载），在 LAP 里找"`advice-add` 或 `fset` 常量后面紧跟一个原语常量"，以及 `cl-letf` 改 `symbol-function` 编译成的 **`byte-fset` 操作码**（实现时才发现，要看它前面最近三个常量）。顶层代码在 `.elc` 里是 `(byte-code ...)`，同样处理。已经被 advice 的原语用 `advice--cd*r` 取出原始 subr 再判断。
- **补充**：init 之后已经被 advice 的原语（静态扫描会漏掉 `set-window-buffer` 这类）。
- **宁可多编**：漏掉一个就是一次冻结，多编一个只花一次 batch 编译。每次最多 50 个（`eln-opt-trampoline-limit`）。
- 静态扫描只能覆盖 `.elc` 里有的东西。运行时才加的 advice 要靠 init 之后的补充，比如 `magit-files.el` 的 `buffer-name`、`magit-git.el` 的 `message`、`rect.el` 的 `region-beginning` 和 `region-end`。

## 安全

**信任边界**：Emacs 只按文件名信任 eln，不验证内容，所以 eln-opt 保证不可信的输入永远不会变成会被加载的代码。
- 只编译可信根下的文件（见"候选规则"）；编译会执行宏，但这些文件装包或安装时已经字节编译过一次，native 编译不引入新的代码执行。
- savehist 和 recentf 只当权重数据：只用 `read` 读取，从不 `eval`；命令名只用 `intern-soft` 查找，从不调用；recentf 的路径不会成为编译对象。
- 数据只留在本机：报告和状态里只写文件名、函数名和原语名。savehist 里可能有 `kill-ring`（剪贴板内容），分析只取 `extended-command-history` 一类的数据，其它不解析。

**进行中标记** `in-progress.eld`：编译前写下文件、尝试次数和时间，编完删除。下次持锁启动时如果还在，说明上次的编译被打断（退出、崩溃、断电）：尝试次数没到 `eln-opt-interrupt-limit`（2）就重试，到了就记入失败列表，直到源码改变。
- 标记只在持锁时读取；它的路径先过可信检查，只用来记录失败；写不进去就不编译；标记或它所在的版本目录是符号链接就拒绝运行，不读也不删（需要用户手动删除）。
- 用户主动退出也算一次尝试：没有 `kill-emacs-hook`，所以不做这种判断。

**失败列表** `failed.eld`：key 是源码的 truename 和内容 md5，失败的文件一直跳过，直到源码改变。没有它，同一个坏文件会让每次运行都在同一处停下。强制编译忽略它。

**锁**：同时只能有一个 worker。锁过期（pid 不在或超过 1 小时）才接管。

**看门狗**：worker 每开始一个文件都输出一行。主 Emacs 600 秒没收到任何输出，就杀掉 worker，并把当时的文件记入失败列表。

**应急开关**：`ELN_OPT=0 emacs` 时，`early-init.el` 设置 `load-no-native`，所有 eln 被跳过，回到 `.elc`。`ELN_OPT=0` 也会覆盖强制编译。

**做不到的部分**：如果同一用户的账号已经被攻破、对方能写 eln-cache，它同样能改 elpa 和 init 文件。eln 与 `.elc` 处于同一信任级别。eln-opt 能保证的是：流水线本身不扩大攻击面，出问题时能检测、能隔离、能关掉。

## 性能

**Pi 上的完整分析**（`eln-opt-rank--compute`）：

| 阶段 | 改前 | 改后 |
|---|---|---|
| 完整分析 | 17.9 秒 | 6.3 秒 |
| 调用图（graph） | 8.2 秒 | 4.8 秒 |
| native 列表 | 7.9 秒 | 0.56 秒 |
| 层级表 | 8.1 秒 | 0.72 秒 |
| 报告格式化 | 27 毫秒 | 27 毫秒 |

改动：native 列表用一次扫描建的哈希表代替每个函数的 `symbol-file`（后者每次都扫整个 `load-history`）；图构建里每个文件的 opt-out 判断只做一次；指令分类用哈希表；循环权重的幂次做了缓存。结果和报告文本与改前逐项一致。

**启动开销**（Pi）：

| 项目 | 耗时 |
|---|---|
| 加载 eln-opt（字节码） | 约 20 毫秒（之前 180 毫秒，来自顶层 `require comp`） |
| 空闲后的指纹比对 | 约 1.5 毫秒 |
| 决定函数 | 约 0.1 毫秒 |

**剩下的主要开销**：字节码函数的特征提取，每次约 3.5 秒（每个函数约 360 微秒，是 Emacs Lisp 的逐条指令开销，微调无效）。要明显再降，需要按文件修改时间缓存每个函数的分析结果，报告和 worker 都能受益。要保证结果完全一致比较复杂（调用列表依赖哪些函数当前已定义），所以是单独的下一步。

## 已知限制

- **用法变化不触发**：指纹不包含 savehist 和 recentf，所以只因为用法变化而出现的新热点，不会主动触发。要等 profile、包或 eln-opt 自己有变化，或者手动运行 `eln-opt-compile-now`。这是有意的取舍（没有时限兜底）。
- **分析会计入自己**：eln-opt 自己的文件也在调用图里，`eln-opt` 常常排在前面。如果不想让它参与，需要在候选规则里排除。
- **单核机器**：默认不自动启动（`eln-opt-min-processors`）。
- **Windows 和 Linux**：只有单元测试和模拟（用 `boundp`、`executable-find` 的桩），没有在真机上运行。
- **GUI 的 idle timer**：在批处理里模拟过（手动调用 `eln-opt-auto--start`），但等它在 GUI 里自己触发，还需要人工确认。
- **崩溃隔离**：没有实现。只能靠失败列表和进行中标记防止反复出错。

## 测试

`tuned-emacs/tests/eln-opt-tests.el` 用 ERT，所有文件都写在临时目录里，`cl-letf` 替换平台判断、编译和睡眠等函数。覆盖：平台门控和金丝雀、trampoline 预测和补齐、锁、账本和失败列表、进行中标记（被打断一次重试、两次失败、不可信路径、写不了标记、符号链接）、候选规则（可信根、opt-out、大小、ratio）、worker 的顺序和首次失败停止、强制编译、报告格式、reset 的确认、`load-history` 映射与 `symbol-file` 一致。

`bash tuned-emacs/install/emacs-setup.sh test` 同时运行 smoke、eln-opt、xl-dired 和 perfetto 的测试。

## 探针和实验脚本

`tuned-emacs/ideas/eln-opt-probes/` 里是最初的实验脚本，运行方法和已知缺口见其 README：排名、trampoline 预测、eln 版本和加载规则、解释执行 / 字节码 / native 的耗时对比。它们是历史记录，不是构建的一部分。

## 未决问题

- **Tier 2 门槛**：`eln-opt-min-ratio` 和分值是经验值，需要离线 profile 校准。
- **持久缓存**：按文件修改时间缓存每个函数的分析结果，把 Pi 上的分析降到 1–2 秒左右。
- **Emacs 自带库的收益**：org 等 `.el.gz` 库的 native 收益还需要在日常使用中测。
- **Linux 的调度**：`ionice` 和 inotify 相关的部分没有测试。
