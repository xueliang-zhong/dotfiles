# xl-perfetto-trace：Emacs Lisp 层的 Perfetto 时间线

状态：已实现（2026-11-25）。实现说明见 [`xl-perfetto-trace-prompt.md`](xl-perfetto-trace-prompt.md)。

## 一句话

录制一段时间内 Emacs 在 Lisp 层做了什么：命令、定时器、进程回调、xl-async 的异步任务、GC、字体着色，以及你点名的函数。结果写成 Chrome JSON trace 文件，在 [ui.perfetto.dev](https://ui.perfetto.dev) 打开，就能在时间线上看到时间花在哪里。

**动机**：中文输入法卡顿那次，我们靠猜测加批处理测量，才找到每次提交都有一段约 230ms 的 `ispell-completion-at-point`。有了时间线，打开 trace 就能直接看到它。

## 目标与非目标

**目标**
- 只用 Emacs Lisp，不改 Emacs 的 C 代码，不需要外部程序。
- 不录制时零开销：不挂 hook，不加 advice，不跑定时器。
- 录制期间开销可控：每个事件几微秒，事件数有上限。
- macOS、Linux、Windows 行为一致。
- 输出能被 Perfetto UI 直接打开，不需要转换。

**非目标（第一版不做）**
- C 层内部：redisplay 各阶段、读键、字体渲染。Elisp 看不到这些。
- 合并外部采样器（Linux `perf`、macOS Instruments）。
- Perfetto 的 protobuf 格式。JSON 已经够用。
- 全量函数追踪。只追踪下面列出的边界，以及用户点名的函数。

## 原则

1. **不改 C 代码。**
2. **不对 C 原语加 advice。** 给 C 原语（`subr-primitive-p` 为真）加 advice，会在 native-comp 的 Emacs 上同步编译一个 trampoline，约 1 秒，期间界面冻结。这个仓库在这上面吃过亏（见 `eln-opt.md` 的 "Trampoline 预测"）。所以 `make-process`、`set-process-filter`、`garbage-collect` 等一律不加 advice；`xl-perfetto-trace-function` 也拒绝 C 原语（见"用户点名的函数"）。
3. **默认零开销。** 所有 hook、advice、process function 包装、定时器都只在 `xl-perfetto-trace-start` 和 `xl-perfetto-trace-stop` 之间存在。停止时必须全部移除，出错时也一样（`unwind-protect`）。
4. **有上限。** 事件存在固定大小的环形缓冲区里，满了就覆盖最旧的；录制有最长时间，到点自动停止。
5. **录制期间不做 I/O。** 只在停止时写一次文件。
6. **不记录内容。** 不记录按键序列、buffer 文本、minibuffer 输入、进程输出。只记录名字、时长和尺寸（见"隐私"）。
7. **追踪器不追踪自己。** 用一个动态变量做重入保护：追踪器内部的代码再次触发被追踪的边界（比如在 `post-gc-hook` 里分配内存又触发 GC）时，直接跳过。

## 用户界面

文件：`tuned-emacs/lisp/xl-perfetto-trace.el`，feature `xl-perfetto-trace`，前缀 `xl-perfetto-`（内部用 `xl-perfetto--`）。

### 命令

| 命令 | 作用 |
|---|---|
| `xl-perfetto-trace-start` | 开始录制。带前缀参数时询问最长秒数。已经在录制就提示一下，不重新开始。 |
| `xl-perfetto-trace-stop` | 停止录制，移除全部插桩，写出文件，在回显区显示路径，返回路径。没在录制就提示。 |
| `xl-perfetto-trace-function`（Lisp 函数，不是命令） | 在本次录制中追踪一个函数（`:around` advice）。没在录制时也可以先登记，开始录制时才生效。命令面板里不出现。 |
| `xl-perfetto-untrace-function`（Lisp 函数） | 取消追踪一个函数。 |
| `xl-perfetto-trace-mark` | 询问一段文字，在时间线上插一个瞬时标记，比如"开始打中文"。 |
| `xl-perfetto-trace-open` | 用 `browse-url` 打开 ui.perfetto.dev，并在系统文件管理器里显示最近一次的 trace 文件，方便拖进去。 |

### 给代码用的宏

```elisp
(xl-perfetto-with-span "eln-opt rank" (eln-opt-rank--compute dir))
```

不在录制时只多一次变量检查，然后直接执行 BODY。在录制时记录一个时间片，非局部退出也会结束这个时间片。

### 选项（defcustom，group `xl-perfetto`）

| 变量 | 默认值 | 含义 |
|---|---|---|
| `xl-perfetto-trace-categories` | 全部 | 要录制的类别：`commands`、`timers`、`processes`、`xl-async`、`gc`、`redisplay`、`functions`、`counters`。 |
| `xl-perfetto-trace-ring-size` | 100000 | 环形缓冲区能放的事件数。 |
| `xl-perfetto-trace-max-seconds` | 60 | 最长录制时间，到点自动停止并写文件。 |
| `xl-perfetto-trace-directory` | `traces/` 放在 `user-emacs-directory` 下 | 输出目录，不存在就创建。 |
| `xl-perfetto-trace-record-names` | t | 是否记录 buffer 名和文件名。nil 时只记录 buffer 的 major mode。 |
| `xl-perfetto-trace-process-scan-interval` | 0.5 | 录制期间扫描新进程的间隔秒数（见"进程"）。 |

### 录制状态

录制期间在 mode line 显示 `● REC` 和已录秒数（通过 `global-mode-string`，停止时移除）。不要用 xl-spinner：它是按 buffer 的，而录制是全局的。

## 输出格式

Chrome JSON trace 格式（Trace Event Format），Perfetto UI 原生支持。整体结构：

```json
{"displayTimeUnit": "ms",
 "traceEvents": [ ... ],
 "metadata": {"emacs-version": "31.1", "system-type": "darwin",
              "started": "2026-11-25T12:00:00", "dropped-events": 0}}
```

**时间**：`ts` 和 `dur` 的单位是微秒，取自 `(car (time-convert nil 1000000))`（整数微秒，精确，不经过浮点），减去录制开始的时刻，所以从 0 开始。

**进程和轨道**：`pid` 固定为 `(emacs-pid)`。每个类别一条"线程"轨道，开头用 `M` 元数据事件命名：

| tid | 轨道名 | 内容 |
|---|---|---|
| 1 | commands | 命令 |
| 2 | timers | 定时器（普通和空闲） |
| 3 | processes | 进程的 filter 和 sentinel |
| 4 | gc | 垃圾回收 |
| 5 | redisplay | 重绘开始标记和字体着色 |
| 6 | functions | 用户点名的函数和 `xl-perfetto-with-span` |
| 7 | marks | 用户标记 |

xl-async 的任务用异步事件（`b`/`e`），在 Perfetto 里会显示成单独的异步轨道；计数器用 `C` 事件，各自一条计数器轨道。

**事件类型示例**

```json
{"name":"thread_name","ph":"M","pid":123,"tid":1,"args":{"name":"commands"}}
{"name":"self-insert-command","cat":"commands","ph":"X","ts":1200,"dur":85,"pid":123,"tid":1,
 "args":{"buffer":"notes.org","mode":"org-mode"}}
{"name":"git status","cat":"xl-async","ph":"b","id":"xl-async-7","ts":3000,"pid":123,"tid":3}
{"name":"git status","cat":"xl-async","ph":"e","id":"xl-async-7","ts":41000,"pid":123,"tid":3,
 "args":{"exit":0,"bytes":5120}}
{"name":"memory","ph":"C","ts":50000,"pid":123,"args":{"conses":123456,"strings":2345}}
{"name":"开始打中文","cat":"marks","ph":"i","s":"t","ts":60000,"pid":123,"tid":7}
```

## 事件来源

每一项写明：用什么机制拿到、生成什么事件、记录哪些参数、要注意什么。

### 1. 命令（`commands`）
- **机制**：`pre-command-hook` 记下开始时刻和 `this-command`；`post-command-hook` 生成一个 `X` 事件。两个 hook 都挂在全局值上，而且排在最前和最后（`add-hook` 的 DEPTH 参数用 -100 和 100），这样测到的时长覆盖其它 hook。
- **名字**：`this-command` 是符号就用符号名，否则用 `"(anonymous command)"`。
- **参数**：`buffer`（受 `xl-perfetto-trace-record-names` 控制）、`mode`。**不记录** `this-command-keys`，因为里面可能是你正在打的文字。
- **注意**：命令出错时 `post-command-hook` 仍然会跑。如果出现两次 `pre` 之间没有 `post`（比如递归编辑），丢弃前一个开始时刻，不生成错误的时间片。

### 2. 定时器（`timers`）
- **机制**：给 `timer-event-handler`（Lisp 函数）加 `:around` advice。
- **名字**：`(timer--function timer)` 是符号就用符号名，否则用 `"(anonymous timer)"`。
- **参数**：`idle`（`(timer--idle-delay timer)` 非 nil 时为 t）。
- **注意**：空闲定时器是打字间隙里偷跑的主要来源，所以一定要区分开。

### 3. 进程回调（`processes`）
- **机制**：用 `add-function :around` 包装 `(process-filter proc)` 和 `(process-sentinel proc)`。这是 nadvice 的正式用法，`remove-function` 能精确恢复，而且不对 C 原语加 advice。
- **哪些进程**：
  1. 开始录制时已经存在的进程：遍历 `(process-list)` 逐个包装。
  2. 录制期间新建的进程：由于不能给 `make-process` 加 advice（原则 2），用一个定时器每隔 `xl-perfetto-trace-process-scan-interval` 秒扫描一次 `process-list`，包装还没包装过的进程。缺点：新进程的前 0.5 秒输出可能漏记。xl-async 启动的进程另有精确记录（见第 4 项）。
- **名字**：`filter <进程名>` 或 `sentinel <进程名>`。
- **参数**：filter 记录这次输出的字节数（只记长度，不记内容）；sentinel 记录事件字符串（如 `"finished\n"`）。
- **停止时**：对所有包装过的进程（包括已经结束的）调用 `remove-function`。用一个列表记住包装过的进程。

### 4. xl-async 任务（`xl-async`）
- **机制**：给 `xl-async-run`（Lisp 函数，见 `tuned-emacs/lisp/xl-async.el`）加 `:around` advice。复制参数 plist（不要修改调用方的列表），把 `:on-done` 包一层：调用原来的回调前，先记一个 `e` 事件。启动时记一个 `b` 事件。
- **id**：`xl-async-<递增计数>`，`b` 和 `e` 用同一个。
- **名字**：`:name` 参数，没有就用命令的程序名（`:command` 的第一个元素的文件名部分）。
- **参数**：`e` 事件记 `exit`（退出码）和 `bytes`（输出长度）。
- **注意**：被取消的任务（`xl-async-cancel`）不会调用 `:on-done`，所以只有 `b` 没有 `e`。停止录制时，给每个还没结束的 id 补一个 `e`，参数 `cancelled-or-running` 为 t，避免 Perfetto 里出现没有结尾的轨道。

### 5. 垃圾回收（`gc`）
- **机制**：`post-gc-hook`。开始录制时记下 `gc-elapsed` 和 `gcs-done`；每次 hook 触发时，用 `gc-elapsed` 的增量得到这次 GC 的时长，生成一个以"当前时刻减时长"为起点的 `X` 事件。
- **参数**：`gcs-done`、这次 GC 的序号。
- **注意**：`post-gc-hook` 里不要做会触发 GC 的大量分配。事件写入环形缓冲区里预先分配好的位置。

### 6. 重绘和字体着色（`redisplay`）
- **机制**：
  - `pre-redisplay-functions`：每次重绘开始生成一个瞬时事件 `redisplay`。频率可能很高，同一毫秒内只记一次。
  - 给 `jit-lock-function`（Lisp 函数）加 `:around` advice：每次着色一个 `X` 事件。
- **参数**：着色的 buffer（受选项控制）、major mode、字符数。
- **注意**：拿不到重绘的结束时刻，所以重绘本身只有开始标记；字体着色的时长是精确的。

### 7. 用户点名的函数（`functions`）
- **机制**：`:around` advice，advice 名为 `xl-perfetto--function-advice`，这样能精确移除。
- **拒绝 C 原语**：`(subr-primitive-p (indirect-function fn))` 为真时，默认拒绝，并说明原因（trampoline）。例外：如果 `eln-opt--trampoline-cached-p` 存在并且说这个 trampoline 已经缓存，或者这个 Emacs 没有 native 编译（`native-comp-available-p` 为 nil），就允许。
- **拒绝追踪器自己的函数**（前缀 `xl-perfetto-`）。
- **非局部退出**：advice 体用 `unwind-protect`，出错时也结束时间片，然后让错误照常传播。

### 8. 标记（`marks`）
- `xl-perfetto-trace-mark` 生成一个 `i` 瞬时事件，名字就是用户输入的文字。

### 9. 计数器（`counters`）
- 每次 GC 之后、以及每秒一次（复用进程扫描的定时器），记一个 `C` 事件：`memory-use-counts` 的各项、`gcs-done`、活着的进程数、buffer 数。

## 时间、缓冲区和重入保护

- **环形缓冲区**：长度为 `xl-perfetto-trace-ring-size` 的向量，加一个写指针和"已覆盖数"。满了就覆盖最旧的，并把覆盖数写进 metadata 的 `dropped-events`。
- **事件的内存表示**：一个小向量 `[PH NAME CAT TS DUR TID ARGS ID]`，`ARGS` 是 plist 或 nil。录制期间不构造 JSON，只在停止时转换。
- **重入保护**：一个动态变量 `xl-perfetto--inside`。所有插桩代码在它非 nil 时直接调用原函数、不记录；记录期间把它绑定为 t。
- **开始时刻**：`xl-perfetto--t0`，整数微秒。

## 生命周期

**开始**（在一个 `condition-case` 里做，任何一步失败都回滚已经完成的步骤，然后报错）：
1. 分配环形缓冲区，记下 `t0`、`gc-elapsed`、`gcs-done`。
2. 写入轨道命名的 `M` 事件。
3. 按 `xl-perfetto-trace-categories` 依次安装插桩：hooks、advice、包装现有进程、启动扫描定时器。每装一个就记进"已安装"列表。
4. 启动自动停止的定时器。
5. 打开 mode line 指示。

**停止**（必须是幂等的，放在 `unwind-protect` 里，保证插桩一定被移除）：
1. 按"已安装"列表逆序移除全部插桩：hooks、advice、process function 包装、定时器。
2. 给没有结束的 xl-async id 补 `e` 事件。
3. 关闭 mode line 指示。
4. 把缓冲区转换成 JSON（`json-serialize`），写入文件。
5. 在回显区显示路径和事件数，返回路径。

**其它**
- Emacs 退出时如果还在录制：什么都不做，缓冲区丢弃。不挂 `kill-emacs-hook`，录制本来就是临时的。
- 录制期间再次调用 start：只提示，不重新开始。

## 写文件

- 文件名：`emacs-trace-<YYYYMMDD-HHMMSS>.json`，放在 `xl-perfetto-trace-directory` 下。
- 写法：先写临时文件再 rename（和 `eln-opt--write-eld` 一样），避免写到一半的文件。
- 大小：10 万个事件大约 15–25 MB，Perfetto UI 能轻松打开。
- 打开方式：ui.perfetto.dev 在浏览器本地解析，文件不会上传；没有网络时也可以用 Perfetto 的离线版本。

## 跟随 eln-opt worker

主 Emacs 开始录制时，写一个标记文件 `traces/recording.eld`（内容：录制目录、开始时间、时限），停止时删除。eln-opt 的后台 worker 是另一个 Emacs 进程，它在两个文件之间（原有的 5 秒睡眠之后）检查这个标记：标记出现且没超过时限，就在同一个目录里开始自己的录制，文件名以 `eln-opt-trace-` 开头；标记消失或超时，就停止并写出文件，并在 worker 日志里打印 `ELN-OPT trace <文件>`。这样主 Emacs 不需要知道 worker 的存在，也不需要 hook，只做直接调用。

- 标记是录制的一部分：`xl-perfetto--start` 写入，`xl-perfetto--stop` 删除。写入和删除都不会让录制失败。
- 超时的标记视为过期（主 Emacs 崩溃时不会留下一个永久生效的标记）。
- 正在编译一个大文件时，worker 不会检查标记，要等这个文件结束；录制开始得晚，前面的部分就没有。
- 测试用的录制和 worker 测试都把 `xl-perfetto-marker-file` 绑到临时文件，不碰真实的标记。

worker 里的分段（只在录制时有开销）：

| 名字 | 含义 |
|---|---|
| `eln-opt worker` | 整次运行 |
| `reach: roots` / `lazy roots` / `graph` / `propagate` / `file scores` | 可达性分析的各步 |
| `reach analysis` | 整个分析（只在计划需要 native 候选时） |
| `byte candidates` | 字节编译候选 |
| `byte compiling <file>` / `native compiling <file>` | 每个文件的编译 |
| `byte <defun>` | 字节编译的每个顶层表单，按它定义的函数命名 |
| `warm new eln: <file>` | 刚编出的 eln 的预热（加载一次，让系统做首次检查） |
| `warm all eln` | 整个 eln 目录的预热扫描 |

native 编译没有按函数的入口：`native-compile` 整个文件一次完成，所以 native 只能到文件这一级。字节编译的顶层表单（即每个 `defun`）可以按函数记录，因为 `byte-compile-top-level` 是原生编译的 Lisp 函数，不是 C 原语，worker 里对它加 advice 不需要 trampoline。

实测（批处理主进程录制，子进程是真实的 worker 命令，计划限制为 1 个文件）：worker 的 trace 里有 116 个 span，其中图的构建 2.6 秒，整个分析 6.5 秒，warm 1.0 秒。主进程和 worker 的 pid 不同，所以两个文件在 ui.perfetto.dev 里是两条独立的时间线。

## 性能预算

- 目标：每个事件的记录开销不超过 5 微秒（Apple Silicon）。打字时感觉不到差别。
- 测量方法：对一个空函数加追踪，调用 10 万次，比较有无追踪的时间差。命令 `xl-perfetto-trace-benchmark` 已删除（只为一次性测量而存在，不值得常驻）；测试里不断言耗时（避免在慢机器上不稳定）。
- 实测（2026-11-25，Emacs 31.1 `aarch64-apple-darwin23.6.0`，`emacs --batch`，10 万次调用，三次运行）：**字节编译后每次约 3.2 微秒**，达到 5 微秒的目标；直接加载 `.el` 源码时每次约 7.1 微秒，因为记录路径本身在解释执行。正常安装会经过 `compile-lisp` 生成 `.elc`，所以运行时按 3.2 微秒算。命令和定时器事件每次只记一次，不受这个数字放大，打字时察觉不到。测试只打印这个数字，不断言它。

## 隐私与安全

- 不记录：按键序列、buffer 文本、minibuffer 输入、进程输出、环境变量。
- 记录：函数名、命令名、定时器函数名、进程名、退出码、字节数、buffer 名和文件名（可关闭）。
- trace 文件只写在本地。分享 trace 前应该知道里面有 buffer 名和文件名；需要时把 `xl-perfetto-trace-record-names` 设为 nil。
- 停止时一定移除全部插桩，避免留下"永久追踪"的 advice。

## 文件和集成

- 新文件：`tuned-emacs/lisp/xl-perfetto-trace.el`。头部格式参考 `tuned-emacs/lisp/xl-dired.el`（Author、Version、`Package-Requires: ((emacs "30.1") (xl-async "0.1.0"))`、Commentary）。对 xl-async 只是可选依赖：没加载时跳过第 4 项。
- 新测试：`tuned-emacs/tests/xl-perfetto-trace-tests.el`，用 `emacs --batch -Q -L tuned-emacs/lisp` 单独运行，风格参考 `tuned-emacs/tests/xl-dired-tests.el`。
- `tuned-emacs/install/emacs-setup.sh` 的 `cmd_test` 加一段运行这个测试文件。
- `tuned-emacs/tests/smoke-tests.el` 里 `xl-personal-file-is-framework-neutral` 的文件列表加上 `tuned-emacs/lisp/xl-perfetto-trace.el`。
- `tuned-emacs/init.el`：加一行 `(require 'xl-perfetto-trace)`。它在开始录制之前什么都不装，所以加载代价很小。
- 按键绑定：不在第一版里决定，交给用户。

## 测试计划

用 ERT，全部在临时目录里运行，测试结束时确认没有残留插桩。

1. **开始再停止后不留痕迹**：`pre-command-hook`、`post-command-hook`、`post-gc-hook`、`pre-redisplay-functions` 恢复原样；`timer-event-handler`、`jit-lock-function`、`xl-async-run` 上没有追踪器的 advice；没有追踪器的定时器；进程的 filter 和 sentinel 和开始前 `eq`。
2. **命令**：手动 `run-hooks` 模拟 pre 和 post，`this-command` 绑定为一个符号，得到名字正确、`dur` 非负的 `X` 事件；参数里没有按键。
3. **定时器**：`run-at-time 0` 一个具名函数，等它运行，得到对应的事件，`idle` 为 nil；空闲定时器得到 `idle` 为 t。
4. **GC**：调用 `garbage-collect`，得到一个 `gc` 事件，`dur` 非负。
5. **进程**：开始录制前启动一个带 filter 的进程（比如 `printf` 或 Emacs 自己的 batch），录制时收到输出，得到 `filter` 事件，参数里只有字节数；停止后 filter 和原来 `eq`。录制期间新建的进程在一次扫描后也被包装。
6. **xl-async**：录制时 `xl-async-run` 一个短命令，得到 id 相同的 `b` 和 `e`；被 `xl-async-cancel` 的任务在停止时补上 `e`。
7. **用户函数**：追踪一个会抛错的函数，时间片仍然结束，错误照常传播；追踪 C 原语时被拒绝（在没有缓存 trampoline 的 native 编译 Emacs 上）；追踪 `xl-perfetto-` 前缀的函数被拒绝。
8. **重入保护**：在被追踪的函数里调用另一个被追踪的边界，不会无限递归，事件数合理。
9. **环形缓冲区**：缓冲区大小设为 10，记 25 个事件，保留最后 10 个，`dropped-events` 为 15。
10. **自动停止**：最长时间设为很短，等待后录制已停止，文件已写出，插桩已移除。
11. **输出文件**：用 `json-parse-string` 解析成功；有 `traceEvents`；每个事件都有 `ph`、`ts`、`pid`；`X` 事件的 `dur` 非负；轨道命名的 `M` 事件存在；metadata 有 `emacs-version` 和 `dropped-events`。
12. **开始失败时回滚**：模拟安装到一半出错，已经装上的插桩被移除，状态为未录制。
13. **记录名字的开关**：`xl-perfetto-trace-record-names` 为 nil 时，事件里没有 buffer 名和文件名。

## 验收标准

- `bash tuned-emacs/install/emacs-setup.sh test` 全部通过，包括新测试。
- 在真实的 GUI Emacs 里录制 10 秒，期间打字、切换 buffer、打开 xl-dired 预览，生成的文件能在 ui.perfetto.dev 打开，看得到命令、定时器、GC、xl-async 和字体着色的轨道。
- 不录制时：`pre-command-hook` 等全局 hook 里没有追踪器的函数，`advice-member-p` 都为 nil。

## 以后可以做的

- 合并外部采样器：Linux 上把 `perf script` 的输出和 Elisp 事件放进同一个 trace（需要对齐时钟）。
- 给 eln-opt 和 xl-dired 的关键路径加 `xl-perfetto-with-span`。
- 从 Emacs 自带 `profiler` 的采样结果生成火焰图（它没有时间戳，只能做调用树，不能进时间线）。
- 录制 N 秒的"按需快照"：一个命令录 10 秒后自动停止并打开。

## 已知限制

- 看不到 C 层内部：重绘只有开始标记，没有各阶段的时长；读键延迟只能从命令之间的间隔间接看出。
- 录制期间新建的非 xl-async 进程，前 `xl-perfetto-trace-process-scan-interval` 秒的输出可能漏记。
- 递归编辑（比如在 minibuffer 里再执行命令）期间，外层命令的时间片会被丢弃，而不是嵌套显示。
