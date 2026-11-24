# xl-jit：不开全局 JIT 的分层编译（设计稿）

状态：设计稿，未实现。所有数字来自 2026-11-24 在 Emacs 31.1（Emacs.app，`31_1-032f33bc`）上的实验。

## 目标与约束

`early-init.el` 关掉了 `native-comp-jit-compilation`：全局 JIT 在第一次使用时就把所有东西放到后台编译，会拖慢整台机器。xl-jit 想保留 JIT 的好处，去掉"什么都编、马上就编"这两点：

- 只编译真正值得编的文件，并且在后台低优先级地编，一次一个。
- **对用户透明**：运行中的 Emacs 里不调用 `profiler-start`，不加 hook，不加 advice，只有一个 timer（`xl-jit-auto.el`）。所有分析和编译都在后台的子 Emacs 里完成。
- 新的 eln **下次启动才生效**，不在当前 session 里热替换。
- 不出现版本不匹配导致的反复检查、反复重编译，或者编好了却不生效的情况。
- 不加载不安全的 eln，不留 DoS 的口子。
- 只用 Emacs Lisp 和 Emacs 自带的工具。

## 三层

```
Tier 0  解释执行 .el     emacs/lisp/*.el 和 xueliang.el 现在就在这一层（没有 .elc）
Tier 1  字节码 .elc      第三方包，以及 Emacs 自带的非预加载库
Tier 2  native .eln      只给热、并且编成 native 确实有收益的文件
```

另外单独处理 **trampoline**：advice 或重定义 C 原语时需要它。它和 Tier 2 无关，但缓存里没有的话，第一次会在主线程同步编译约 1 秒。

## 实测数据

| 测量 | 结果 |
| --- | --- |
| `xl-git-status--entries`（20000 条 status） | 解释执行 0.244s → 字节码 0.084s（×2.9）→ native 0.076s（×1.1）；重跑一次是 ×2.7 和 ×0.9，native 和字节码的差别在测量噪声范围内 |
| `xl-git-status--insert-diff`（1500 个 hunk） | 解释执行 0.152s → 字节码 0.088s（×1.7）→ native 0.087s（×1.0） |
| Emacs.app 自带的 native 代码 | 只有 138 个预加载文件；`org`、`package`、`thingatpt`、`winner`、`savehist` 等在 JIT 关闭时跑字节码 |
| 子进程跑完 init 并建调用图 | 5792 个函数，213 个根节点，约 2.4 秒 |
| trampoline 静态预测 | 扫 1543 个 .elc 约 10 秒；缓存里的 11 个命中 9 个，另外预测出 6 个缓存里没有的 |

由此得出的优先级：

1. **trampoline 预测**：消除的是约 1 秒的界面冻结，收益最确定。
2. **Tier 1**：自己的 lisp 编成字节码，×1.7 到 ×2.9，成本几乎为零。
3. **Tier 2**：只给少数函数带来可测的收益，门槛要高。

`early-init.el` 和 README 里"Emacs 自带的 Lisp 仍是 native"的说法，在这个构建上只对预加载的文件成立。

## 流水线

```
平台门控（见下一节）：不支持 native 编译的平台，到这里就结束，后面的步骤都不会发生
触发：主 Emacs 开了 10 分钟以上、用户空闲 60 秒、距上次运行超过一天、没在用电池（xl-jit-auto.el，只用 timer）
  └─ taskpolicy -b <同一个 Emacs 二进制> --batch --init-directory <同一个 profile>
       0. 前置检查：拿锁、比较指纹、检查目录权限、确认版本目录一致；任何一项不满足就退出
       1. 复查账本里记录的 eln：哈希、dlopen；有问题的删掉
       2. 加载 init，建调用图，算出热点排名
       3. 预测 trampoline，补齐缓存里没有的
       4. Tier 1：重编 .el 比 .elc 新的文件
       5. Tier 2：在预算内编译排名靠前的文件
       6. 验证：在子进程里加载，并跑 tests/smoke-tests.el；不通过就删掉这一批
       7. 写账本和本次运行的报告
下次启动：load 找到匹配的 .eln，自动用它
```

Linux 上用 `nice -n 19 ionice -c3` 代替 `taskpolicy -b`。

## 平台门控：没有 libgccjit 就整条链都不触发

Windows 上常见的 Emacs 构建没有 native 编译，或者带了 native 编译但找不到 libgccjit 的 DLL。这类平台上 trampoline 不存在，Tier 2 也无从谈起，所以 xl-jit **整条链都不启动**：不建计划任务，不起子进程，不读 savehist，不写账本。

**怎么判断**

- 唯一的依据是 `(native-comp-available-p)`。它在 Emacs 没有编进 native 编译时返回 nil，在 Windows 上找不到 libgccjit DLL 时也返回 nil。不按操作系统名判断：将来 Windows 上装了带 libgccjit 的构建，会自动启用。
- 判断只做一次，结果缓存到 `xl-jit-platform.eld`，key 是 Emacs 二进制的路径、大小和 mtime。二进制不变就直接用缓存结果，连判断用的 batch 进程都不再启动。换了 Emacs，key 变化，重新判断一次。
- 结果为 nil 时，emacs-setup.sh 打印一行说明（例如 `xl-jit: native compilation not available, skipped`），然后退出。不报错，也不影响 setup 的其它步骤。

**不可用时主 Emacs 里连 timer 都不排**

- `xl-jit-auto.el` 每次检查都先看 `native-comp-available-p`。为 nil（例如 Windows 上没有 libgccjit）时，返回 `unavailable`，不再排下一个 timer，这个 session 里不会再有任何动作。
- 金丝雀熔断（见下）在后台子进程里判断。熔断后子进程立刻返回 `unavailable`，主 Emacs 一天最多为此启动一次子进程。

**带了 native 编译但实际编不出来**

`native-comp-available-p` 返回 t，不代表一定能编译：libgccjit 版本不对、gcc 驱动不在、链接器出错，都会让每次编译失败。如果只靠按文件记账，每个文件都会各失败两次，白白消耗 CPU。所以加一个**平台熔断**：

- 每次运行开始时，先编译一个只有一个函数的探针文件（放在临时目录，不进 eln-cache），把它当作金丝雀。
- 金丝雀失败，就把平台标记为不可用，写进 `xl-jit-platform.eld`，key 同上。之后直到二进制变化之前，整条链都不再启动。
- trampoline 的预编译同样受这个熔断控制。

**没有 native 编译的平台还剩什么**

- trampoline：不需要，因为没有 native 代码，advice 原语不需要它（现有的 `cmd_trampolines` 已经这样处理）。
- 应急开关 `load-no-native`：不需要。
- Tier 1（把自己的 lisp 编成 .elc）不依赖 libgccjit，但它不属于 xl-jit 这条链。它是 setup 里一个独立的字节编译步骤（`compile-lisp`），不复用这里的触发器和账本。

## 热点排名（Tier 2 的输入）

### 根节点：都是磁盘上已有的数据

| 根 | 来源 | 权重 |
| --- | --- | --- |
| 用户实际执行过的命令 | savehist 的 `extended-command-history` | 按频次和新旧加权，单个命令封顶 |
| 常用的 major mode | recentf 的路径经 `auto-mode-alist` 映射 | log(文件数) |
| 常用 mode 的 hook 和 keymap | `<mode>-hook`、`<mode>-map` | 跟随 mode；keymap 里的命令权重较低 |
| 每次按键或重绘都会运行的 hook | `post-command-hook`、`after-change-functions` 等 | 最高 |
| 全局和 leader keymap 里的命令 | `global-map`、`xl-leader-map` | 很低 |

**按 mode 剪枝**：某个 mode 的 keymap 和 hook，只有在这个 mode 出现在 recentf 推出的常用集合里时才算根。evil-collection 给几百个 mode 做的设置，大部分会因此被剪掉。

### 边：从字节码里读，再在子进程里读值补上动态调用

- 每个函数常量向量里的函数符号，嵌套的闭包要递归进去。
- 函数里读取了某个变量（`byte-varref`）并且调用了 `funcall`/`apply` 时，如果这个变量的值是函数或函数列表，就连边（比如 `completion-at-point-functions`）。
- 已有 advice 的函数：原函数和 advice 函数双向连边。
- 间接注册表：`completion-styles` → `completion-styles-alist`、`auto-mode-alist`、`major-mode-remap-alist`、`cl-defgeneric` 的方法表、init 结束时的 timer 列表。
- 看不到的部分：命令执行时才创建的 lambda 和 timer、进程的 filter 和 sentinel。对这部分保守处理：离使用过的根只有一跳的文件，给一个最低热度。

### 打分

- 热度 R：`R(被调用者) += α · R(调用者) / sqrt(出边数)`，α 取 0.5，最多 4 层。
- 收益 B：从 LAP 里读。先用向后跳转找出循环体，循环体里的指令按 4^嵌套深度 加权（递归视为多一层）：
  - 加分：内联运算（`car`/`cdr`/算术/`eq`/`aref` 等）、调用其他 Lisp 函数；
  - 扣分：buffer 操作码、C 重活（`insert`、`re-search-*`、`string-match`、`propertize`……），以及**内存分配类调用**（`substring`、`concat`、`format`、`split-string`）。上面 ×1.1 的结果说明，时间大多花在分配和 GC 上。
- 函数按 `symbol-file` 归属到文件（第一版用 `load-history`，`*-autoloads.el` 被误排上榜）。
- 文件分 = Σ R·B；成本 ≈ 字节码总长度。在预算内按"分 ÷ 成本"挑选，并设一个下限。
- 已经是 native 的函数（`native-comp-function-p`）跳过。

第一版排名靠前的大多是每次按键都会运行的 hook（evil-jumps、winum、winner、evil-repeat、delsel）。`package.elc` 排第一，是因为最近的 savehist 里有很多 package menu 命令，所以需要对单个命令的频次封顶。

### 校准

α、各类根的权重、各操作码的分值，由开发者在自己的 session 里离线跑一次 profiler 来校准（算 recall@N）。用户那边不运行 profiler。

## Trampoline 预测

- 把 .elc **当作数据读**，不加载。在 LAP 里匹配"`advice-add` 常量后面紧跟着一个原语常量"。`cl-letf` 改 `symbol-function` 编译成的是 **`byte-fset` 操作码**，不是 `fset` 调用，所以实现时要单独处理：遇到 `byte-fset`，看它前面最近三个常量里的原语（实现时发现的，探针漏掉了这一种）。
  - 顶层代码在 .elc 里会被编译成 `(byte-code "..." [advice-add select-window ...])`，要对它做同样的匹配。
  - LAP 列表里夹着整数偏移量，匹配前要先过滤掉。
  - 已经被 advice 的原语要用 `advice--cd*r` 取出原始的 subr 再判断。
- 再并上 init 之后已经被 advice 的原语（静态扫描漏了 `set-window-buffer`）。
- 对缺失的，在 batch 里调用 `comp-trampoline-compile`。
- **宁可多编**：漏掉一个就是一次冻结，多编一个只花一次 batch 编译。数量设上限，比如 50 个。

现有的 `cmd_trampolines` 靠执行 idle-timer 里的加载来覆盖，覆盖不到在**命令执行过程中**才加的 advice。这次实验找到了三个：

- `magit-files.el:207`：`magit--blob-normal-mode` 里临时 advice `buffer-name`；
- `magit-git.el:3233`：运行时 `advice-add 'message :override`；
- `rect.el`：`rectangle-mark-mode` advice `region-beginning` 和 `region-end`。

## eln 的版本和加载规则（实测）

| 情况 | 会用 eln 吗 |
| --- | --- |
| eln 存在，经真实路径或符号链接加载 | 会 |
| 源码内容改了（文件名里的内容哈希变了） | 不会；下次编译时 comp 会自己删掉同一路径的旧 eln |
| **`.elc` 比 `.eln` 新** | **不会，而且没有任何提示** |
| 只有 `.elc`，没有 `.el` | 不会 |
| 只有 `.el.gz`（Emacs 自带库） | 会 |
| eln 写进了别的 eln-cache（`-Q` 默认写 `~/.emacs.d/eln-cache`） | 永远不会 |
| `load-prefer-newer t` 并且 `.el` 比 `.elc` 新 | 加载 `.el` |
| eln 损坏（随机字节、截断、空文件） | **`require` 报 `native-lisp-load-failed`，不会退回 .elc** |
| 名字对、内容来自别的源码 | **静默加载** |
| 设置了 `load-no-native` | 跳过所有 eln，回到 .elc；预加载的核心代码仍然是 native |

eln 的文件名是 `<名字>-<路径哈希>-<内容哈希>.eln`，放在 `eln-cache/<comp-native-version-dir>/` 下。版本目录随二进制构建变化：Emacs.app 里已经有 `31_1-032f33bc` 和 `31_1-38fe94a6` 两个，版本号同样是 31.1。comp 写 eln 时先写临时文件再 `rename`，会得到新 inode，正在运行的 Emacs 不会崩溃。

## 不会死循环的规则

1. **"完成"由加载结果判断**：编译完后，在子进程里 `require` 这个文件，检查其中一个 defun 是否 `native-comp-function-p`。不靠"eln 文件存在"来判断，因为上表里有三种情况文件存在却不会被使用。
2. **只保存一份状态：失败账本**。key 是 `(comp-native-version-dir, 源码 truename, 内容哈希, 编译参数)`。编译参数指 `native-comp-speed`、`native-comp-compiler-options`、`native-comp-driver-options`、`native-comp-debug`：实测它们**不在 eln 文件名里**，改了以后旧的 eln 仍会被复用，所以要由账本来发现并重编。
   - 第一次失败，隔一天重试；第二次失败，在 key 变化前不再尝试；
   - 源码改了，或者 Emacs 重新构建，key 就变了，自动解除。
   - "编译成功但验证失败"也算失败，并记录原因（比如没有源码、elc 一直比 eln 新）。
3. **Tier 1 一定先于 Tier 2**，并且只在 `.el` 比 `.elc` 新时才重编 elc，所以 elc 的 mtime 保持稳定，不会让 eln 失效。
4. **同一个二进制，同一个缓存目录**：子进程用 `(expand-file-name invocation-name invocation-directory)` 启动，加上同一个 `--init-directory`。开始前断言 `comp-native-version-dir` 和 `native-comp-eln-load-path` 的第一项与 GUI 一致，不一致就整次放弃。
5. **触发有界**：每天最多一次；指纹（版本目录 + 各 load-path 目录的 mtime）没变就立即退出。savehist 每天都会变，不放进指纹，排名每周重算一次。
6. **锁**：用 `make-directory` 建锁目录（原子操作），里面记 pid 和启动时间。pid 不存在或者超过 1 小时就视为过期。抢不到锁就直接退出，不等待也不重试。
7. **写入只走 comp**：只用 `native-compile` 和 `comp-delete-or-replace-file`，从不 `cp` 进 eln-cache，也从不原地覆盖。
8. **清理**：同一路径的旧 eln 由 comp 自己删。源码路径已经不存在（比如包升级换了目录）、并且超过 7 天的 eln，在子进程里删除，只删账本里记录过的。超过 30 天的旧版本目录整体删除。

## 安全

### 信任边界

Emacs 只按文件名信任 eln，不验证内容。所以 xl-jit 必须保证：**不可信的输入永远不会变成会被加载的代码。**

- **只编译可信根目录下的文件**：elpa 目录、Emacs 的 `lisp-directory`、dotfiles 的 `emacs/lisp`，用 `file-truename` 判断，指向根目录之外的符号链接直接拒绝。编译会执行宏；这些文件装包时已经被字节编译过一次，所以 native 编译不会引入新的代码执行。
- **savehist 和 recentf 只当作权重数据**：只用 `read` 读取，并校验结构，从不 `eval`。命令名只用 `intern-soft` 查找，从不调用。recentf 里的路径永远不会成为编译对象。
- **子进程没有网络**：`package-archives` 设为 nil；缺包就拒绝运行，不走 init 的装包路径。
- **目录权限**：`native-comp-eln-load-path` 里的每个目录都必须属于当前用户、group 和 other 不可写、本身不是符号链接，否则整次放弃。

### 完整性

- xl-jit 产出的每个 eln，都把 sha256 和大小记进账本。
- 每次运行先复查：哈希不符，或者在子进程里 dlopen 失败，就删掉并记录原因。
- 只管理自己产出的文件。Emacs 自己生成的 eln（trampoline 等）只做 dlopen 健康检查，有问题就在报告里提示，不删除。

### DoS

| 风险 | 对策 |
| --- | --- |
| 编译炸弹（文件很大、宏展开爆炸） | 单文件大小上限、编译超时（`xl-async` 的 `:timeout`）、每次运行的预算；超时记为失败，不再重试 |
| 磁盘被占满 | xl-jit 产出的总体积上限（比如 200 MB），按账本统计 |
| 进程风暴 | 同一时间只有一个子进程；每天最多触发一次；有锁 |
| 锁被长期占用 | 只是损失优化，功能不受影响 |
| 账本被篡改 | 只用 `read`，限制大小，校验结构；不合格就丢弃重建 |
| **eln 损坏导致 init 中途停下** | 编译后在子进程里加载并跑 smoke tests，不通过就删除；每天复查可以自愈；有应急开关 |

### 应急开关

- `XL_JIT=0 emacs`：early-init 里一行 `(setq load-no-native t)`。已经实测：损坏的 eln 会被跳过，回到 .elc，预加载的核心代码仍然是 native。
- `emacs-setup.sh jit-reset`：只删除账本里记录的 eln。

### 做不到的部分

如果同一个用户的账号已经被攻破、对方能写 eln-cache，那它同样能改 elpa 目录和 init 文件。eln 和 .elc 处在同一个信任级别。xl-jit 能保证的是：流水线本身不扩大攻击面，出问题时能检测、能自愈、能关掉。

## 借鉴其他项目

| 项目 | 它的做法 | xl-jit 借鉴什么 |
| --- | --- | --- |
| Android ART（dex2oat、bg-dexopt、Baseline Profiles） | 运行时收集热点方法；设备空闲并且在充电时，每天在后台编译一次；应用开发者可以随应用附带一份"基线 profile"，第一次启动就有合理的热点 | 调度条件（空闲 + 接电源 + 每天一次）；**基线热点表** |
| HotSpot 分层编译 | 解释 → C1 → C2；code cache 有大小上限；编译后发现假设不成立时可以**去优化**回退 | 三层结构；总体积上限；**崩溃隔离**（回退到 .elc） |
| LuaJIT | trace 多次中止后列入黑名单，不再尝试 | 失败账本的"两次失败就停" |
| ccache / Bazel | 缓存 key 包含编译器版本和全部编译参数；`ccache -s` 给出命中率和体积统计；超过上限按 LRU 淘汰 | key 里加编译参数；**状态统计**；超限时按最近使用淘汰 |
| Python .pyc（PEP 552） | 失效方式可以选：按时间戳，或按源码哈希 | 同样的教训：只看 mtime 不可靠，所以用加载结果来验证 |
| Java CDS / Leyden | 先做一次"训练运行"，把结果存成归档，以后每次启动直接用 | 离线校准排名权重；基线热点表就是训练结果 |
| compile-angel、auto-compile（Emacs 包） | 在加载或保存时自动字节编译和 native 编译；`auto-compile` 推荐 `load-prefer-newer t` | Tier 1 的做法；尊重排除列表。不同点：它们靠 advice 或 hook 在父进程里触发，xl-jit 不进入父进程 |
| Nix | 内容寻址；只有被 GC root 引用的产物才保留 | 账本就是 GC root：只清理账本里记录、并且已经没有对应源码的 eln |

### 由此补充的设计

**1. 基线热点表（冷启动）**
新机器上 savehist 和 recentf 都是空的，排名器没有依据。仓库里附带一份 `emacs/jit-baseline.eld`，列出一般都值得处理的文件和 trampoline：每次按键都会运行的 evil、winum hook，completion 栈，以及上面已知的三个 trampoline。有了用户自己的数据以后，基线的权重逐步降低。这份表只列文件名和原语名，不包含代码。

**2. 尊重已有的退出机制**
- 带 `no-native-compile: t` 或 `no-byte-compile: t` 文件局部变量的文件，不编译。
- 匹配 `native-comp-jit-compilation-deny-list`（在 `comp-run` 里）的文件，不编译。
- `native-comp-speed` 最高为 2。speed 3 会假设同一编译单元里的函数不会被重新定义，这会让 advice 失效。

**3. 崩溃隔离（借鉴去优化）**
主 Emacs 里不加 hook，所以没法在 Emacs 里捕获崩溃。但 macOS 会把崩溃报告写到 `~/Library/Logs/DiagnosticReports/Emacs-*.ips`，子进程可以读：
- 如果某个崩溃报告的时间晚于某批 eln 的生效时间，并且调用栈里出现了 xl-jit 产出的 eln 文件名，就删掉这个 eln，并把它的 key 永久列入账本。
- 有崩溃但找不到对应的 eln 时，只在报告里提示，不做任何操作。
- Linux 上对应的是 `coredumpctl` 的输出，可用时同样处理。

**4. 状态统计（借鉴 `ccache -s`）**
`emacs-setup.sh jit-status` 和 `M-x xl-jit-report` 显示：
- 上次运行的时间、结果，以及跳过的原因（指纹没变、拿不到锁、没接电源……）；
- 各层的文件数和总体积、失败和隔离的数量；
- trampoline 的预测数、已缓存数、本次补齐数。

总体积超过上限时，按"最近一次在排名中出现"的时间淘汰最旧的 eln。

**5. 触发条件（借鉴 bg-dexopt 的"空闲 + 接电源 + 每天一次"）**
- 不用 launchd 这类 Emacs 之外的计划任务。主 Emacs 自己用 timer 判断：开了 `xl-jit-auto-uptime`（10 分钟）以上，说明它会长期开着；之后等用户空闲 `xl-jit-auto-idle`（60 秒）。
- 条件：距上次运行超过 `xl-jit-auto-interval`（一天，看 `last-run.eld` 的 mtime）；没在用电池（`battery-status-function`）；没有正在跑的后台子进程。不满足就隔一段时间再检查。
- 后台子进程在 macOS 上用 `taskpolicy -b`（后台 QoS、效率核心），其它系统用 `nice -n 19`。
- batch 环境（`noninteractive`）里绝不触发：setup、测试，以及后台子进程自己加载 init 时，都不会再派生子进程。
- 还没做：用户恢复操作时暂停子进程（SIGSTOP/SIGCONT）。那需要一个 hook，和"只用 timer"冲突，暂时不做。
- 仍然遵守"每天最多一次"和指纹检查。

**6. 隐私**
`xueliang.el` 把 `kill-ring` 加进了 savehist，所以 savehist 文件里有剪贴板文本。子进程：
- 只从中取出 `extended-command-history`，其它变量一律不解析；
- 报告和账本里只写文件名、函数名和原语名，不写 kill-ring 内容，也不写 recentf 里的路径；
- 不把这些数据发到任何地方。

**7. 用故障注入测试 xl-jit 本身**
把实验时用的情况都写成 ERT 测试，放在 `tests/` 下，每个都在临时目录里跑：
- `.elc` 比 `.eln` 新、没有 `.el`、只有 `.el.gz`、写进了错误的缓存目录；
- eln 损坏（随机字节、截断、空文件）、名字对但内容来自别的源码；
- 锁过期、锁被占用、磁盘写满、编译超时、账本被篡改；
- 改了编译参数、版本目录变了；
- `native-comp-available-p` 返回 nil（断言什么都没启动，也没写任何文件）、金丝雀编译失败（断言熔断以后不再启动）。

每个测试都要断言两件事：最终状态正确，并且**第二次运行什么都不做**（没有重复编译）。

**8. 多个 Emacs 并存**
同一台机器上可能有 Emacs.app 和 Homebrew 的 Emacs，或者同一个版本号的两次构建。账本、锁和指纹都按 `comp-native-version-dir` 分开保存，互不干扰。

## 实现清单

按顺序做，每一步单独提交。每一步都标了适合的实现方式：**可以直接实现**的，规格明确，并且有探针可以照着改；**需要仔细 review** 的，不变量很细，错了不会报错，只会悄悄地反复重编或者永远不生效。

### 通用规则

- 代码放在 `emacs/lisp/xl-jit.el`。**init.el 不 require 它**：它只在子 Emacs 里用 `-l` 加载。运行中的 Emacs 里唯一的改动是第 4 步的应急开关，只有一行 `setq`。
- 状态放在 `user-emacs-directory` 下的 `xl-jit/`：`platform.eld`（平台判断缓存）、`<comp-native-version-dir>/ledger.eld`（账本）、`<comp-native-version-dir>/lock/`（锁目录）、`<comp-native-version-dir>/last-run.eld`（上次运行的报告）。
- eln 只通过 comp 写入（`native-compile`、`comp-trampoline-compile`、`comp-delete-or-replace-file`），从不 `cp` 或原地覆盖。
- 测试放在 `emacs/tests/xl-jit-tests.el`，用 `emacs --batch -Q` 就能跑，所有文件都写在 `make-temp-file` 建的临时目录里。需要模拟 native 编译失败等情况时，用 `cl-letf` 替换函数，并统计调用次数。
- **每个测试都断言两件事**：最终状态正确；第二次运行时 `native-compile` 和 `byte-compile-file` 的调用次数为 0。
- 提交前跑 `bash emacs/install/emacs-setup.sh check`，并更新这份文档和探针 README 里受影响的部分。

### 1. 平台门控和金丝雀熔断（可以直接实现）

- 函数：`xl-jit-platform-available-p`。按二进制的路径、大小、mtime 查 `platform.eld`；没有缓存时先看 `native-comp-available-p`，再在临时目录编译一个只有一个函数的金丝雀文件，把结果写进缓存。
- `emacs-setup.sh` 新增子命令 `jit`，第一步就是这个判断。不可用时打印 `xl-jit: native compilation not available, skipped`，退出码为 0。
- 验收：
  - `native-comp-available-p` 被替换成返回 nil 时，`xl-jit-run` 返回 `unavailable`，状态目录里除了 `platform.eld` 没有其它文件；
  - 金丝雀编译被替换成报错时，`platform.eld` 记为不可用；第二次运行不再编译金丝雀；
  - 改变二进制 key（测试里改 mtime）后，会重新判断一次；
  - 在这台 Mac 上，`bash emacs/install/emacs-setup.sh jit` 报告可用。

### 2. Trampoline 预测和补齐（可以直接实现）

- 从 `xl-jit-probes/xl-jit-probe-trampolines.el` 移植：`xl-jit-trampolines-predict` 返回预测的原语列表（静态扫描并上已经被 advice 的），`xl-jit-trampolines-build` 对缓存里没有的调用 `comp-trampoline-compile`，每次最多 50 个。
- 接进 `cmd_trampolines`：在现有的 idle-timer 加载之后执行，受第 1 步的门控控制。
- 验收：
  - 跑完以后，`buffer-name`、`message`、`region-beginning`、`region-end` 的 trampoline 都在 eln-cache 里；
  - 第二次运行编译 0 个；
  - 测试：一个含 `(advice-add 'buffer-name ...)` 的玩具 .elc，以及一个在函数里用 `cl-letf` 改 `(symbol-function 'message)` 的玩具 .elc，都能被预测到；`(defalias 'x #'car)` 不会被预测；扫描期间这些玩具文件没有被加载（玩具文件里的顶层代码设置一个变量，断言它仍然未绑定）。

### 3. Tier 1：自己的 lisp 编成字节码（可以直接实现）

这一步不依赖 libgccjit，不属于 xl-jit 这条链，Windows 上也要做。

- `emacs-setup.sh` 新增 `compile-lisp`，并在各平台的 setup 里调用：只重编 `.el` 比 `.elc` 新的 `emacs/lisp/*.el` 和 `emacs/xueliang.el`。
- `.gitignore` 加上 `emacs/**/*.elc`。
- `early-init.el` 加上 `(setq load-prefer-newer t)`，这样改了 `.el` 还没重编时，加载的是新的源码，而不是旧的 .elc。
- **`init.el:167` 现在是 `(load ".../emacs/xueliang.el")`，带着扩展名，所以永远不会用 .elc**。要改成不带扩展名的 `emacs/xueliang`。
- 验收：
  - 启动后 `(byte-code-function-p (symbol-function 'xl-git-status--entries))` 为 t，`xueliang.el` 里的函数也是；
  - 第二次运行 `compile-lisp` 编译 0 个文件；
  - 改一个 `.el` 但不重编时，启动后用的是新源码（解释执行）；
  - `bash emacs/install/emacs-setup.sh test` 通过。

### 4. 账本、锁、指纹、验证、应急开关（需要仔细 review）

- 账本 key：`(comp-native-version-dir, 源码 truename, 内容哈希, 编译参数)`。条目里记录状态（done / failed / quarantined）、失败次数、原因、时间，以及产出 eln 的 sha256 和大小。只用 `read` 读取，限制大小，校验结构；不合格就丢弃重建。
- "完成"按加载结果判断：在子进程里 `require` 这个文件，检查其中一个 defun 是否 `native-comp-function-p`。
- 锁：`make-directory` 建锁目录，里面写 pid 和启动时间；pid 不存在或者超过 1 小时视为过期。拿不到锁就退出，不等待。
- 指纹：版本目录 + 各 load-path 目录的 mtime。没变就退出。
- 每次运行开始时复查账本里的 eln：哈希不符或者 dlopen 失败，就删掉并记录原因。
- 应急开关：`early-init.el` 里加 `(when (equal (getenv "XL_JIT") "0") (setq load-no-native t))`；`emacs-setup.sh jit-reset` 只删除账本里记录的 eln。
- 验收：设计文档"用故障注入测试 xl-jit 本身"里列出的每一种情况各写一个 ERT 测试，全部通过，并且每个都满足"第二次运行什么都不做"。

### 5. 排名报告和基线热点表（需要仔细 review）

- 从 `xl-jit-probes/xl-jit-probe-rank.el` 移植，先补上探针 README 里列的缺口。
- 只输出报告，不编译：`emacs-setup.sh jit-report` 和 `M-x xl-jit-report`。
- 新建 `emacs/jit-baseline.eld`，只列文件名和原语名。
- 验收：
  - 报告里没有 `*-autoloads.el`；
  - 单个 savehist 命令的贡献不超过总分的 20%；
  - 前 10 名由你人工看过，觉得合理。

### 6. Tier 2 后台编译（需要仔细 review）

- 在预算内编译排名靠前的文件，要满足"安全"一节的全部要求：可信根目录（truename）、尊重 `no-native-compile` 和 deny list、`native-comp-speed` 不超过 2、单文件大小上限、编译超时、总体积上限。
- 编译完立刻验证：在子进程里加载，并跑 `tests/smoke-tests.el`；不通过就删掉这一批。
- 状态统计（`jit-status`）和崩溃隔离（读 DiagnosticReports）。
- 验收：
  - 测试：指向可信根目录之外的符号链接被拒绝；带 `no-native-compile: t` 的文件不编译；编译超时记为失败，第二次运行不重试；一份伪造的崩溃报告（调用栈里有某个 eln 的文件名）会让这个 eln 被删掉并隔离；
  - 真实运行一次后，再运行一次，编译 0 个文件。

### 7. 主 Emacs 里的触发器（已改，原先是 launchd）

最初实现成了 launchd 计划任务，后来按你的要求换成主 Emacs 里的 timer：一个独立于 Emacs 的计划任务不合适。

- `emacs/lisp/xl-jit-auto.el`，由 init.el `require` 并调用 `xl-jit-auto-arm`。只用 `run-with-timer` 和 `run-with-idle-timer`，不加 hook。
- 条件和优先级见上面"触发条件"。子进程结束后，按 `xl-jit-auto-interval` 重新排下一次检查，Emacs 连开几天也会每天跑一次。
- 结果存在 `xl-jit-auto-last-result`，子进程输出在 ` *xl-jit*` 缓冲区；账本和统计仍然用 `jit-status` 看。
- 验收：各个"不运行"的原因都有测试；batch 下不排 timer；空闲时条件变了（比如拔了电源）就不启动；后台命令带 `taskpolicy -b` 或 `nice -n 19`；用加载了 profile 的 batch 模拟一次完整运行。

## 实现记录（2026-11-24）

实现按上面的清单完成，每一步单独提交（`e85d754` 到 `e62c621`）。与设计不一致的地方如下，以实现为准：

- **验证和审计的子进程加载 profile**，不是裸 `-Q`。`xueliang.el` 的 eln 在顶层引用 `xl-leader-map`，由 `xl-key-bindings.el` 定义，裸子进程会把好的 eln 判为失败，审计还会删掉它。子进程仍然是全新进程，只是先 `-l init.el`。
- **"完成"的判断**：排名里这个文件的任意一个函数在新加载的 eln 里是 native，就通过。宏生成的函数（如 `define-minor-mode`）不一定以顶层 `defun` 出现。
- **编译子进程**同样加载 profile，并把 `load-path` 传进去。eln 加载时会 `require` 依赖（winum 需要 dash）。
- **失败原因**只保存"Error:"开始的至多 300 字符，不保存整段回溯。
- **Tier 1 的 `xueliang.el` 也会被 Tier 2 编译成 eln**（它在排名前列）。之后它的函数类型是 `subr`，不是 `byte-code-function`；冒烟测试据此改为"字节码或 native 都算已编译"。
- **Crafted 的模块**没有 .elc，是解释执行的（报告里数量为 1120）。它们不在可信根里，也就不进入 Tier 2。要不要把 Crafted 纳入可信根，需要你决定。
- **`native-comp-speed` 上限 2**、**预算 3 个文件/次**、**最低排名比 5**（最初是 20；降到 5 后 corfu、evil-common、evil-collection-unimpaired、evil-core、evil-surround 进入候选，但 winner、savehist 等内置库仍因只有 `.el.gz` 而被拒绝）、**单文件源码 500 KB**（最初 200 KB，consult 247 KB 因此被拒绝）、**总量 200 MB**、**编译超时 180 秒**：这些都是起始值，没有校准。
- **总量超限时只停止编译，不做 LRU 淘汰**。设计里的按最近排名时间淘汰还没实现。

## 未验证的部分

- 查询 native 函数：`M-x xl-jit-native-report` 列出 xl-jit 编译过的文件和其中已是 native 的函数，并给出 Emacs 自身预加载的 native 函数总数。
- 主 Emacs 里的 timer 只在 batch 里模拟过（手动调用 `xl-jit-auto--start`），还没有在 GUI 里等它自己在 10 分钟后触发。
- 崩溃隔离只用伪造的崩溃报告测过。真实的 macOS 崩溃报告调用栈里是否会出现 eln 文件名，还没有确认。
- 不再检查热限制（launchd 版本用 `pmset -g therm`，换成 timer 后去掉了）。
- `M-x xl-jit-report` 的交互路径（打开缓冲区）没有测；批处理路径（`jit-report`）测过。
- Linux 和 Windows 上没有测试。

## 未决问题

- Tier 2 的门槛：实测 native 收益普遍不大，需要用离线 profile 确定最低分。
- 跑字节码的 Emacs 自带库（尤其是 org）可能是 Tier 2 最合适的对象，需要单独测一下。
- Linux 上的 inotify、`ionice` 和权限检查还没有测试。

## 实验脚本

探针在 [`xl-jit-probes/`](xl-jit-probes/) 里，运行方法、每个脚本的结果和已知缺口见那里的 README：

- `xl-jit-probe-rank.el`：热点排名；
- `xl-jit-probe-trampolines.el`：trampoline 预测；
- `xl-jit-probe-eln-rules.sh`：eln 版本和加载规则；
- `xl-jit-probe-bench.el`：解释执行、字节码、native 三种状态下的耗时。
