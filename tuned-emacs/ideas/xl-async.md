# xl-async：可组合的异步原语

状态：设计稿，尚未实现。现有的 `xl-async.el` 保持不变，按"迁移步骤"一步步替换。

## 背景

`xl-async.el` 现在有两层：进程层（`xl-async-run`、`xl-async-enqueue`）和 loader（`xl-spinner-*`）。进程层把起进程、收输出、归属、slot 替换、超时、流式输出、队列都放在 `xl-async-run` 一个函数的一个大 plist 里：

```elisp
(xl-async-run :command … :directory … :owner … :slot … :stdin …
              :name … :timeout … :on-output … :on-done …)
```

它好用，但这些机制是捆在一起的：想要"超时但不要归属"、"归属但不要 slot"、"同一个进程上既流式又在结束时解析"，都只能通过这个函数，靠参数的组合来表达。

另外新写的 `xl-async-evaluate`（在一个干净的 `emacs -Q --batch` 里求值一个表单）又自己调用了一遍 `make-process`，和进程层重复。

现有调用方（不含 `xl-async.el` 本身）：

| 函数 | 调用方 |
|---|---|
| `xl-async-run` | xl-dired、xl-git-status、xl-git-commands、xl-org-babel-async、xl-perfetto-trace，以及测试 |
| `xl-async-enqueue`、`xl-async-queue-length`、`xl-async-queue-busy-p` | xl-git-status，以及测试 |
| `xl-async-cancel` | xl-dired、xl-git-commands、xl-org-babel-async、perfetto 测试 |
| `xl-async-busy-p` | 测试 |
| `xl-spinner-*` | xl-git-status、xl-git-commands、xl-dired、xl-org-babel-async、xl-consult-spinner |

`xl-async-run` 的关键字在调用方里的使用次数：`:command` 17、`:on-done` 15、`:name` 14、`:slot` 13、`:directory` 10、`:timeout` 7、`:owner` 5、`:stdin` 4、`:on-output` 4。

## 第一性原理

1. **一个异步任务就是一个进程对象。** 不另造"job"结构：状态、输出、处理函数都放在进程的属性里（`process-put`）。
2. **每个原语只做一件事，形状都一样：接收进程，返回进程。** 组合用 `thread-first`，不用大 plist。
3. **只有一个地方碰 `make-process`。** 也只有它安装 filter 和 sentinel。其他原语只往进程上登记处理函数，所以互相不会覆盖。
4. **难的保证只写一次。** "结束时输出已经完整"、"回调恰好一次"、"取消的任务不回调"、"丢失的 sentinel 由 reaper 补上"，这些都只在核心里实现，任何组合都绕不过。
5. **组合与顺序无关。** 进程结束后才登记的处理函数照样执行，所以 `(xl-async-on-exit p f)` 写在哪里都对。

## 非目标

- 不做线程、不做 future/promise 库。回调就是回调。
- 不做远程（ssh/tramp）进程。命令本身可以是 `ssh …`，但这里不管连接。
- 不替代 `compile`、`comint`。交互式 shell 不是这里的场景。

## 结构

```
第 2 层  应用      xl-async-evaluate   xl-async-worker   xl-async-native-compile
                   兼容：xl-async-run  xl-async-enqueue  xl-async-cancel …
第 1 层  行为      xl-async-timeout  xl-async-own  xl-async-slot  xl-async-queue-push
第 0 层  核心      xl-async-start  xl-async-on-output  xl-async-on-exit  xl-async-finally
                   xl-async-abort  xl-async-send  xl-async-close-input  查询函数
```

每一层只用下一层，第 1 层的四个行为彼此独立。

文件：

| 文件 | 内容 |
|---|---|
| `lisp/xl-async.el` | 第 0、1 层，以及兼容层（`xl-async-run` 等改写成组合） |
| `lisp/xl-spinner.el` | loader，从 `xl-async.el` 拆出；`xl-async.el` 仍 `require` 它，旧的 `(require 'xl-async)` 照样拿到 loader |
| `lisp/xl-async-evaluate.el` | 第 2 层：evaluate、worker、native-compile |

命名：所有名字都用 `xl-async-` 前缀。`async-` 是 emacs-async 包（helm、dired-async 会拉进来）的前缀，它也有 `async-start`。第 2 层的私有函数用 `xl-async-evaluate--`、`xl-async-worker--`，不和 `xl-async--` 冲突。

## 第 0 层：核心

### 任务的属性

| 属性 | 含义 |
|---|---|
| `xl-async-status` | `running`、`done`、`timeout`、`cancelled` 之一 |
| `xl-async-output` | 结束时从输出 buffer 复制出来的全部输出（字符串） |
| `xl-async-on-output` | 处理函数列表，按登记顺序 |
| `xl-async-on-exit` | 同上 |
| `xl-async-finally` | 同上 |
| `xl-async-owner` | 归属的 buffer，或 nil（第 1 层的 own 设置） |
| `xl-async-finished` | 结束流程已经跑过（保证只跑一次） |
| `xl-async-keep-buffer` | 输出 buffer 是否在结束后保留 |

### `(xl-async-start COMMAND &rest ARGS)` → 进程

起进程，输出写进一个 buffer，返回进程。ARGS：

| 关键字 | 含义 | 默认 |
|---|---|---|
| `:directory` | 工作目录 | `default-directory` |
| `:stdin` | 写入标准输入的文本 | 无 |
| `:keep-input` | 非 nil 时不关闭标准输入（worker 要用） | nil：写完 `:stdin` 就关闭，这样 `git apply` 这类读 stdin 的命令能结束 |
| `:name` | 进程名，也是 perfetto 和错误信息里的名字 | `"xl-async"` |
| `:buffer-name` | 输出 buffer 的名字，可含 `%d`（替换成 pid）。给了就在结束后**保留** buffer | 隐藏 buffer `" *xl-async*"`，结束后删除 |
| `:coding` | 进程的编码 | `utf-8-unix` |

做的事：`make-process`（`:connection-type 'pipe`、`:noquery t`）、安装唯一的 filter 和 sentinel、状态设为 `running`、启动 reaper（每 0.25 秒检查一次，直到结束）。

### `(xl-async-on-output PROC FN)` → PROC

每块输出到达时调用 `(FN PROC CHUNK)`。输出先追加到 buffer，再调用处理函数，所以处理函数里看到的 buffer 已经包含这块输出。有归属时在 owner buffer 里调用。

### `(xl-async-on-exit PROC FN)` → PROC

任务**正常结束**（`done` 或 `timeout`）时调用 `(FN PROC)` 一次。被取消（`cancelled`）的任务不调用。处理函数用查询函数取输出和退出码。

### `(xl-async-finally PROC FN)` → PROC

任务**无论怎么结束**都恰好调用 `(FN PROC)` 一次。用于清理：取消定时器、从 slot 表里删除、推进队列。

### `(xl-async-abort PROC)` → PROC

取消：状态设为 `cancelled`，杀掉进程。on-exit 不调用，finally 调用。对已经结束的任务什么都不做。

名字用 abort 而不是 cancel，因为兼容层里的 `xl-async-cancel (&optional slot buffer)` 已经有另一种签名。

### `(xl-async-send PROC STRING)`、`(xl-async-close-input PROC)` → PROC

写入标准输入、关闭标准输入。只对 `:keep-input` 的任务有意义。

### 查询

| 函数 | 返回 |
|---|---|
| `(xl-async-status PROC)` | 状态符号 |
| `(xl-async-live-p PROC)` | 状态是否为 `running` |
| `(xl-async-output PROC)` | 运行中：buffer 里到目前为止的输出；结束后：复制出来的字符串（buffer 删除后也能取） |
| `(xl-async-exit-code PROC)` | 退出码；`timeout` 时为 124（coreutils `timeout` 的退出码，调用方可以一视同仁）；运行中为 nil |
| `(xl-async-buffer PROC)` | 输出 buffer，或 nil |

## 结束流程（只在核心里，恰好一次）

sentinel 和 reaper 都调用同一个结束函数，用 `xl-async-finished` 保证只跑一次。顺序固定：

1. **读完输出。** 进程已退出但管道里可能还有数据：循环 `accept-process-output` 直到读空。现在的 reaper 就是这么做的（Emacs 30.1 上，一个进程在另一个 sentinel 运行期间退出，它的 sentinel 可能不被调用；不先读空就回调，会交出被截断的输出）。
2. **定状态。** 已经是 `cancelled` 或 `timeout` 就保持，否则设为 `done`。
3. **保存输出。** 把 buffer 内容复制到 `xl-async-output` 属性。
4. **调用 on-exit**（只在 `done`、`timeout` 时），按登记顺序。有归属时在 owner buffer 里调用；owner 已经被杀，就把状态改成 `cancelled`，跳过 on-exit。
5. **调用 finally**，按登记顺序。
6. **处置 buffer。** 没有 `:buffer-name` 的隐藏 buffer 删除；有名字的保留。

每个处理函数都包在 `condition-case` 里：一个处理函数出错，错误写进 `*Messages*`（带上任务名），其他处理函数照常执行。`xl-async-debug-on-error` 为非 nil 时改为直接抛出，测试里用。

**晚登记**：任务结束后才调用 `xl-async-on-exit` 或 `xl-async-finally`，处理函数在下一轮事件循环里执行（`run-at-time 0`），规则和上面一样（取消的任务不调用 on-exit）。所以组合的顺序无关，即使命令快到 `xl-async-start` 返回前就退出了也没问题。

## 状态机

```
            exit                       ┌─→ done
running ─────────────────────────────┤
            timeout 定时器触发          ├─→ timeout   （退出码 124，on-exit 照常调用）
            abort / owner 被杀 / slot 替换
                                       └─→ cancelled （on-exit 不调用）
```

终态不再变化。finally 在所有终态都调用一次。

## 第 1 层：行为

四个行为都只用第 0 层实现，彼此独立。

### `(xl-async-timeout PROC SECONDS)` → PROC

启动一个定时器：到时如果还在运行，状态设为 `timeout`，杀掉进程。用 finally 取消定时器。

### `(xl-async-own PROC BUFFER)` → PROC

归属，三件事：

- 设置 `xl-async-owner`：on-output、on-exit 在 BUFFER 里调用；BUFFER 被杀时 on-exit 不调用。
- 把 PROC 加进 BUFFER 的局部列表 `xl-async--owned`（`permanent-local`，换 major mode 不丢）。
- 给 BUFFER 加一个**局部** `kill-buffer-hook`：BUFFER 被杀时 abort 它拥有的全部任务。这是现在已有的行为，只作用于那个 buffer。

用 finally 把 PROC 从列表里删除。

### `(xl-async-slot PROC SLOT)` → PROC

同一个 owner（没有 owner 时是全局）、同一个 SLOT，新任务 abort 旧任务。表的键是 `(OWNER . SLOT)`。用 finally 删除表项（只在表项仍是自己时删）。

查询：`(xl-async-slot-process SLOT &optional BUFFER)`。

### 队列：`(xl-async-queue-push KEY JOB &optional ON-DRAIN)`

队列的单位是 **job**，不是进程：`(lambda (DONE) …)`，job 完成时调用 `(funcall DONE)`。这样队列不关心 job 里面起了几个进程、是 git 还是 evaluate。

- 同一个 KEY 的 job 严格按顺序执行，前一个调用 DONE 后才开始下一个。
- 队列空时调用 ON-DRAIN（给了新的就替换旧的），并删除队列。
- KEY 通常是 `(BUFFER . SLOT)`；BUFFER 被杀时清空它的队列。

两个辅助函数：

| 函数 | 作用 |
|---|---|
| `(xl-async-job THUNK)` | THUNK 返回一个进程；进程的 finally 调用 DONE |
| `(xl-async-attempts THUNKS ON-DONE)` | 依次尝试：前一个退出码非零才试下一个；最后一个结束时调用 ON-DONE 和 DONE。这就是现在 `xl-async-enqueue` 的 `:attempts` |

查询：`(xl-async-queue-size KEY)`、`(xl-async-queue-busy-p KEY)`；清空：`(xl-async-queue-clear KEY)`（abort 正在运行的那个）。

## 组合示例

```elisp
;; Git 视图：归属、替换旧请求、超时、结束时渲染
(thread-first
  (xl-async-start '("git" "status" "--porcelain=v2") :name "git status")
  (xl-async-own (current-buffer))
  (xl-async-slot 'status)
  (xl-async-timeout 10)
  (xl-async-on-exit (lambda (p) (my-render (xl-async-output p)))))

;; 只要超时，不要归属：后台检查一个外部命令
(thread-first
  (xl-async-start '("rg" "--version"))
  (xl-async-timeout 5)
  (xl-async-on-exit (lambda (p) (message "rg: %s" (xl-async-exit-code p)))))

;; 流式显示，结束时再解析
(thread-first
  (xl-async-start '("git" "push") :name "git push")
  (xl-async-own log-buffer)
  (xl-async-on-output (lambda (_p chunk) (my-append chunk)))
  (xl-async-on-exit #'my-parse-push))

;; 队列：s s s 按顺序暂存
(xl-async-queue-push (cons (current-buffer) 'stage)
                     (xl-async-job (lambda () (xl-async-start (list "git" "add" "--" file)))))
```

## 第 2 层：在子 Emacs 里求值

### 子进程协议

evaluate 和 worker 用**同一个**子进程循环，只是 evaluate 发一个请求就关闭输入：

- 启动命令：`emacs -Q --batch --eval <一小段引导代码>`。表单**不**放在命令行里，而是通过标准输入发送。这样没有命令行转义问题，也不受 Windows 命令行 32767 字符的限制。
- 请求：一行 `(ID . FORM)`，用 `prin1` 打印，`print-escape-newlines`、`print-escape-multibyte`、`print-escape-nonascii` 都打开，`print-length`、`print-level` 为 nil。整行只有 ASCII，所以不依赖子进程的 locale 编码（Windows 上尤其重要）。
- 子进程：`read-from-minibuffer` 读一行（batch 模式下从 stdin 读），`read` 出请求，在 `condition-case` 里 `(eval FORM t)`，然后打印一个换行、标记、ID 和结果：`XL-ASYNC-RESULT <ID> (:ok VALUE)` 或 `XL-ASYNC-RESULT <ID> (:error MESSAGE)`。前面先打一个换行，保证标记在行首，即使表单自己打印了半行文字。读到 EOF 就退出。
- 父进程：用 on-output 按行切分，只认行首是标记的行，按 ID 交给对应的回调；其他输出只留在 buffer 里给人看。读回结果时 `read-eval` 为 nil，单行超过上限（比如 8 MB）就报错，不去读。

### `(xl-async-evaluate FORM &optional CALLBACK &rest ARGS)` → 进程

一个干净的子 Emacs，求值一个表单，CALLBACK 收到 `(:ok VALUE)` 或 `(:error MESSAGE)`。输出在 `*async-buffer <pid>*`，结束后保留。

组合起来就是：

```elisp
(thread-first
  (xl-async-start (xl-async-evaluate--command) :keep-input t
                  :buffer-name "*async-buffer %d*" :name "xl-async-evaluate")
  (xl-async-evaluate--request 0 form)      ; 发出唯一的请求
  (xl-async-close-input)
  (xl-async-on-exit (lambda (p) (funcall callback (xl-async-evaluate--result p 0)))))
```

子进程没有输出结果就结束时（崩溃、被杀、命令不是 Emacs），CALLBACK 收到 `(:error "no result; …")`，并带上原因。

### worker：常驻子 Emacs

```elisp
(let ((w (xl-async-worker-start)))
  (xl-async-worker-eval w '(require 'org) #'ignore)
  (xl-async-worker-eval w '(org-version) (lambda (r) (message "%S" r)))
  (xl-async-worker-stop w))
```

- `xl-async-worker-start` 起一个 `:keep-input` 的子进程，返回进程本身，所以第 1 层的行为都能用在 worker 上（own、timeout）。
- `xl-async-worker-eval` 分配一个 ID，登记回调，发送请求，返回 ID。
- 请求按顺序处理（子进程是单线程的），结果按 ID 交付。要并行就起多个 worker。
- 子进程的状态在请求之间保留：这正是 worker 和 evaluate 的区别。evaluate 每次都是干净的环境；worker 只付一次启动开销（测试里每次约 0.13–0.18 秒），适合多次交互。
- worker 结束时，所有还没回来的请求都收到 `(:error "worker exited: …")`。
- `xl-async-worker-stop` 关闭输入，让子进程自己退出；`xl-async-abort` 直接杀掉。

### `(xl-async-native-compile FILE &optional CALLBACK &rest ARGS)` → 进程

`(xl-async-evaluate '(native-compile FILE) …)` 的薄封装：

- 平台不支持原生编译（`native-comp-available-p` 为 nil，比如某些 Windows 构建）时，直接 `user-error`，不起子进程，也不调用回调。回调只会收到进程，形状和 evaluate 一致；调用方应先用 `native-comp-available-p` 检查。
- 成功时 VALUE 是 eln 文件的路径。
- 编译一个依赖别的包的文件时，调用方用 `:load-path` 明确给出目录（见"决定"）。

## 兼容层

现有调用方一行都不改。旧函数改写成组合：

| 旧接口 | 组合 |
|---|---|
| `xl-async-run` | `xl-async-start`（`:directory` `:stdin` `:name`）→ owner 为 nil 时不 own，否则 `xl-async-own`（默认当前 buffer）→ 有 `:slot` 时 `xl-async-slot` → 有 `:timeout` 时 `xl-async-timeout` → 有 `:on-output` 时 `xl-async-on-output`（包一层，调用 `(ON-OUTPUT CHUNK)`）→ 有 `:on-done` 时 `xl-async-on-exit`（包一层，调用 `(ON-DONE OUTPUT EXIT-CODE)`） |
| `xl-async-cancel (&optional SLOT BUFFER)` | abort BUFFER 在 SLOT（或全部）的任务 |
| `xl-async-busy-p` | 查 slot 表或 owner 列表 |
| `xl-async-enqueue` | `xl-async-queue-push` 加 `xl-async-attempts`，`:on-start` 在 job 开始时调用 |
| `xl-async-queue-length`、`xl-async-queue-busy-p`、`xl-async-clear-queue` | 对应的队列查询和清空 |
| `xl-async-timeout-code` | 保留，值仍是 124 |

要保持的行为细节（现有测试覆盖）：ON-DONE 在 OWNER 里运行；OWNER 被杀或被取消时 ON-DONE 不运行；unowned 的任务 ON-DONE 总是运行，且不能用 `:slot`；`:on-output` 的块在 ON-DONE 的 OUTPUT 里仍完整出现。

## loader 拆分

`xl-spinner-*` 和进程无关，拆到 `lisp/xl-spinner.el`（`provide 'xl-spinner`）。`xl-async.el` 继续 `(require 'xl-spinner)`，旧的 `(require 'xl-async)` 照样拿到 loader；以后调用方可以直接 `require 'xl-spinner`。loader 的代码不动。

## 观测（perfetto）

perfetto 现在在录制期间 advice `xl-async-run`。新设计里所有进程都从 `xl-async-start` 起，所以改为 advice `xl-async-start`：开始时记 `b`，在 finally 里记 `e`，名字用 `:name`。这样 evaluate、worker、native-compile 也会出现在时间线上，兼容层的调用照样被记录。

## 平台

| 平台 | 注意点 |
|---|---|
| macOS | `taskpolicy -b` 这类调度包装属于命令本身，不在核心里 |
| Linux | 无特别处理 |
| Windows | 表单走标准输入，不走命令行，避开转义和长度限制；请求和结果只含 ASCII，不依赖代码页；`:coding` 默认 `utf-8-unix`；没有原生编译时 native-compile 直接报错，不起进程 |

## 安全

- evaluate 和 worker 在子进程里执行任意表单，信任边界和在当前 Emacs 里 `eval` 一样：只执行调用方自己构造的表单，不执行来自文件或网络的文本。
- 父进程读回结果时 `read-eval` 为 nil，只读带标记的行，单行有大小上限。
- 子进程用 `-Q` 启动，不加载用户配置，不会因为配置里的代码产生副作用。

## 性能

- 每个子 Emacs 启动约 0.13–0.18 秒（`-Q`，测试实测）。频繁的小任务用 worker 摊薄。
- 每块输出的分发开销是遍历处理函数列表，通常 0 到 2 个。
- reaper 每个运行中的任务每 0.25 秒检查一次，结束即停，和现在一样。
- 结束时输出复制成字符串，短时间内 buffer 和字符串各一份；对 git 的输出量没有影响。

## 测试计划

每条契约都有对应的测试，测试在 `tests/xl-async-tests.el`（新），兼容层继续由 `smoke-tests.el` 覆盖：

| 契约 | 测试 |
|---|---|
| on-exit 恰好一次 | sentinel 和 reaper 都触发时，处理函数只运行一次 |
| 取消不回调 | abort 后 on-exit 不运行，finally 运行一次 |
| 输出完整 | 大输出（> 1 MB）在 on-exit 里长度正确 |
| 晚登记 | 进程结束后登记的 on-exit 和 finally 仍然运行 |
| 处理函数隔离 | 第一个处理函数出错，第二个照常运行 |
| 超时 | 状态 `timeout`、退出码 124、on-exit 运行、定时器被取消 |
| 归属 | 处理函数在 owner 里运行；杀掉 owner 会 abort，on-exit 不运行 |
| slot | 同 slot 新任务 abort 旧任务；旧任务的 finally 不删新表项 |
| 队列 | 顺序执行；attempts 失败后试下一个；队列空时调用 ON-DRAIN 一次 |
| evaluate | 值、错误、无法读回的值、子进程无输出、非 ASCII 字符串往返 |
| worker | 多个请求按 ID 交付；请求之间状态保留；worker 死掉时未完成的请求收到错误 |
| 兼容 | 现有 168 个 smoke 测试和 perfetto 测试不改、全部通过 |

## 迁移步骤

每一步一个提交，四个测试套件都要全部通过：

1. 第 0 层写进 `xl-async.el`，和旧代码并存（新名字不冲突），加 `tests/xl-async-tests.el`。
2. 第 1 层，加测试。
3. 兼容层：`xl-async-run`、`xl-async-enqueue` 等改写成组合，删除旧实现；现有测试不改。
4. perfetto 改为 advice `xl-async-start`。
5. evaluate 改建在第 0 层上，换成标准输入协议；加 worker 和 native-compile。
6. loader 拆到 `xl-spinner.el`。

## 决定

这些问题在第一版之前定下，原因都是为了简洁和稳定。

1. **第 2 层的文件名：`xl-async-evaluate.el`。** evaluate 是这一层的核心；worker 和 native-compile 都建立在它上面。
2. **`*async-buffer <pid>*` 的清理：只保留最近 10 个。** 每次启动新的 evaluate 或 worker，删掉更老的 buffer。输出能看，但数量有上限，不需要用户手动清理。
3. **处理函数的参数：`(FN PROC)`。** 进程是唯一的对象，状态、输出、退出码都从它查询，形状统一，以后扩展也不用改签名。兼容层只需要把 `(FN PROC)` 包成旧的 `(ON-DONE OUTPUT EXIT-CODE)`，每个包装只有几行。
4. **子进程的 `load-path`：由调用方通过 `:load-path` 明确传入。** 子进程只把这些目录加到自己的 `load-path` 前面，不继承父进程的整个 `load-path`。这样子进程的行为可复现，也不会因为父进程的配置而变化。
5. **worker 不支持并发请求。** 请求按顺序处理，要并行就起多个 worker。这样只有一种执行顺序，状态和错误都更容易推理。

## 已知问题

- **特征缓存的结果和冷启动不一致。** 同一个环境里，缓存已经建好的一次运行，和不用缓存的运行，有些函数的 `:benefit` 不同（例如 `cl--parse-loop-clause` 差 2）；第一次就用缓存写入的运行也会出现。原因还没有定位。所以 `xl-async-jit-rank` 的特征缓存**默认关闭**，只有设置 `XL_ASYNC_JIT_FEATURE_CACHE=1` 才打开。关闭时的结果是参考基准：与旧的 eln-opt 分析逐行一致（完整 profile、有基线）。代价是图的构建约 4 秒，而不是缓存下的 0.65 秒。
