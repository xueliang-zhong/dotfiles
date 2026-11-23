# XL Emacs（`emacs/`）

**A thin Emacs layer whose heavy work runs async. 重活异步，界面不等。**

一层很薄的个人适配层，架在锁定版本的 Crafted Emacs 之上。Git 的重活——status、diff、log、blame、commit 视图、暂存与提交——都用异步进程重写，界面在数据到达前就显示 spinner，不会冻住。`xl-` 是这个项目的命名空间（`xl-key-bindings`、`xl-git-status`、`xl-spinner`），同时是作者 Xueliang 的缩写；配置本身放在 dotfiles 的 `emacs/` 目录里。

## 两条可验证的承诺

| 承诺 | 怎么验证 |
| --- | --- |
| 个人层是薄的 | `init.el`、`early-init.el`、`lisp/xl-key-bindings.el`、`xueliang.el` 合计约 1400 行 |
| Git 不阻塞 UI | 读（status/diff/log、blame、commit 视图）与写（stage/unstage/discard/commit）全部走 `make-process`；测试在命令返回的瞬间断言 loader 已在、内容未到 |

还有一处异步不是我们做的：`list-packages` 的归档下载在 Emacs 31 里本来就是异步的（`package-menu-async` 默认为 t）。我们只把它 mode line 里静态的 `:Loading` 换成动画，见 `lisp/xl-package-menu.el`。重写它只会比上游更差。

还没异步化的部分，如实写在这里，不假装已经完成：`list-packages` 里的安装/删除（package transaction）仍然同步，归档刷新除外。那是上游的限制，装上后还要在同一个 Emacs 里激活包，拆到子进程反而更糟。

不写形容词：不叫自己 “lite”，也不叫自己 “fast”。数字和测试能证明的东西，才写进 README。

## 目录

```
emacs/
├── early-init.el          启动期设置（GC、界面、包源）
├── init.el                选 Crafted 模块、装包、按顺序加载
├── install/emacs-setup.sh 不依赖 just 的安装、检查和启动脚本
├── lisp/                  用 require 加载的模块
│   ├── xl-key-bindings.el     Spacemacs 键位习惯（SPC 树等）
│   ├── xl-async.el            异步进程：归属、槽位取消、串行队列
│   ├── xl-spinner.el          共用 loader：帧、mode line 槽位、不抖屏
│   ├── xl-git-status.el       异步 Git status 摘要
│   ├── xl-git-commands.el     Gstatus/Gblame/Gwrite/Gcommit
│   ├── xl-package-menu.el     Package Menu 刷新时的 loader
│   └── xl-package-upgrades.el 可升级包数量，在子 Emacs 里算
├── xueliang.el            私有口味，不发布
├── tests/smoke-tests.el   ERT 测试
└── frameworks.lock        Crafted 的仓库地址与提交号
```

## 架构

Emacs 31.1 启动时，先读 `~/.emacs-profiles/crafted` 里的两个存根，存根再把控制权交给这里的文件。

```
just emacs-try
  └─ emacs --init-directory ~/.emacs-profiles/crafted   (Emacs 31.1)
       ├─ ~/.emacs-profiles/crafted/early-init.el   存根：设 xl-dotfiles、xl-crafted-home
       │    └─ emacs/early-init.el               启动期设置，加载 Crafted 包源配置
       └─ ~/.emacs-profiles/crafted/init.el         存根
            └─ emacs/init.el                     选 Crafted 模块、装包、按顺序加载
                 ├─ Crafted 模块（~/.emacs-profiles/src/crafted-emacs，锁定提交）
                 ├─ emacs/lisp/xl-key-bindings.el Spacemacs 键位习惯（SPC 树等）
                 ├─ emacs/lisp/xl-git-status.el  异步 Git status 摘要
                 ├─ emacs/lisp/xl-git-commands.el Gstatus/Gblame/Gwrite/Gcommit
                 └─ emacs/xueliang.el       我自己的设置、键位、命令
```

四层的分工：

| 层 | 文件 | 谁负责 |
| --- | --- | --- |
| 运行时 | Emacs 31.1、elpa 包目录 | 系统与包管理 |
| 框架 | Crafted 模块 | 上游项目，版本由 `frameworks.lock` 锁定 |
| 适配 | `early-init.el`、`init.el`、`lisp/xl-key-bindings.el` | 把 Crafted 接到我的习惯上 |
| 包 | `lisp/xl-async.el`、`lisp/xl-spinner.el`、`lisp/xl-git-status.el`、`lisp/xl-git-commands.el`、`lisp/xl-package-menu.el` | 异步 Git、共用 loader 与 Package Menu 动画，可脱离本配置单独安装 |
| 个人 | `xueliang.el` | 我的全部口味，只用原生 Emacs 和包的接口，不发布 |

## 文件说明

### `frameworks.lock`

记录 Crafted Emacs 的仓库地址和锁定的提交号。`just emacs-crafted-install` 读这个文件，把 Crafted 克隆到 `~/.emacs-profiles/src/crafted-emacs` 并检出该提交。升级 Crafted 时改这里的提交号，然后重新执行安装命令。

### `early-init.el`（适配层）

Emacs 在创建界面之前读它，所以只放启动期的设置：

- 启动时关掉 GC，启动完成后恢复到 128 MB。
- 关掉菜单栏、工具栏、滚动条。
- 包只用 bytecode：关闭 JIT 原生编译和装包时的原生编译。Emacs 自带的核心 Lisp 仍是 native 的；advice C 原语需要的 trampoline 会在缓存为空时在主线程同步编译一次（第一次启动可能多几秒白屏）。`just emacs-trampolines-cache-setup` 在 batch 里预先把它们建好，`just emacs-mac-setup` 会自动执行它。
- 包目录按 Emacs 版本分开（`elpa-31.1`），编译产物不会跨版本混用。
- 关闭 Crafted 每天启动时的包列表刷新检查，改为手动刷新。
- 抑制 Emacs 31 对没有 `lexical-binding` 声明的第三方文件的警告。
- evil 系列固定从 MELPA 安装（NonGNU 的 1.15.0 在 Emacs 30 以上会报错）。
- 最后加载 `crafted-early-init-config`，设置包源和优先级。

### `init.el`（适配层）

启动的主流程，按顺序做这些事（详见下面的“加载顺序”）：

- 检查 Emacs 版本，低于 30 直接报错停止。
- 加载 `custom.el`（如果存在），它是 Customize 写入的地方。
- 加载 `crafted-init-config`，把 Crafted 模块目录加入加载路径。
- 汇总要安装的包：Crafted 的 completion、evil、org 三组包，加上 `xl-packages`（Crafted 不提供、而 `lisp/xl-key-bindings.el`、`lisp/xl-git-status.el`、`lisp/xl-git-commands.el` 和 `xueliang.el` 要用的包）。缺包时先 `package-refresh-contents` 再装：这个配置关掉了启动时的自动刷新，而 `package-install-selected-packages` 只在缓存为空时才刷新；旧缓存会让新依赖（例如 2026 年的 transient 要求 compat 31.1）被误报为 unavailable。刷新只发生在确实缺包的那次启动，装完就恢复零网络。如果刷新后还缺包（典型是 GNU ELPA 的签名校验因 `package.el` 的 gnupg home 过期而失败，Denote 就属于 GNU ELPA），会重建 keyring 再刷新一次；仍失败才报错并列出哪些包根本不在任何归档里；gpg 完全不可用时，最后会临时关闭签名校验再试一次（只影响那次启动）。
- 加载 Crafted 的 defaults、completion、evil、org、startup 五个模块。其中 completion 负责整个补全栈：vertico、consult、marginalia、orderless、embark、corfu、cape。
- 修正 Crafted 默认值中与我的习惯冲突的部分：插入态 `C-h` 保持为帮助前缀，关闭全局拼写检查，并去掉 Crafted 挂在 `embark-collect-mode-hook` 上的 `consult-preview-at-point-mode`（Consult 3.4 已删除该函数，不去掉会让 Embark Collect 报错）。
- 把 `emacs/lisp/` 加入加载路径，用 `require` 依次载入 `xl-key-bindings.el`、`xl-git-status.el`、`xl-git-commands.el`、`xl-package-menu.el`，最后加载 `xueliang.el`。用 `require` 而不是 `load`，是因为其中可以单独安装的包走的是同一条路。
- 最后把 `xl-packages` 补回安装名单，并在每次切换主题后再补一次：加载主题会恢复 `custom.el` 里保存的旧名单，不补回的话 `package-autoremove` 可能误删正在用的包。

### `lisp/xl-key-bindings.el`（适配层）

复刻 Spacemacs 的键位习惯，Crafted 本身没有这些：

- SPC 键树（`b f g j p q r s t w` 等分组）和 `M-m`。
- `SPC i s` 列出当前模式的 snippet，`SPC i S` 新建 snippet，位置和 Spacemacs 的 auto-completion 层一致。
- `,` 与 `SPC m` 主模式 leader，目前对 Org 和 Emacs Lisp 生效。
- `fd` 退出插入态，evil-surround，窗口编号 `SPC 1`–`9`。
- 首页（`SPC b h`），列出最近文件，按数字打开。
- `SPC q r` 重启 Emacs：重启后回到当前文件和光标位置（dired 回到同一个目录）；从临时 buffer（没有文件）重启则回到首页。状态写在 `user-emacs-directory` 的 `xl-restart-place.eld` 里，只用一次；取消重启会把它删掉。
- 创建 `xl-leader-map`，供 `xueliang.el` 继续往里加绑定。

它在 `xueliang.el` 之前加载，所以 `xueliang.el` 里的绑定会覆盖这里的默认值（例如 `SPC SPC`、`SPC x`）。它和 `xueliang.el` 一起构成适配层，因此虽然放在 `lisp/` 里，用 `require` 加载，但不是可以单独安装的包：它要用 `xl-dotfiles` 和 `xueliang.el` 里的命令。

### `lisp/xl-git-status.el`（异步 Git 页面）

status 窗口可见时，工作区变化会自动刷新，不用按 `g`。用 `file-notify` 监听：根目录、`.git`，以及每个含受跟踪或未跟踪文件的目录（`git ls-files` 列出，异步完成）。file-notify 在 macOS 和 Linux 上都不递归（FSEvents 只报告子目录本身的变化），所以要按目录监听。锁文件、编辑器备份和 `.git` 里与状态无关的文件（objects、logs）不触发刷新；连续的变化合并为一次刷新，最后一次变化 0.5 秒后执行（`xl-git-status-refresh-delay`）；窗口不可见或 git 队列忙时不刷新。`xl-git-status-auto-refresh` 可以关掉。macOS 已在 GUI Emacs 里测试（子目录修改、新目录、终端里的提交）；Linux 的 inotify 分支尚未测试。

一个不调用 Magit 的异步 status 摘要：`Head:`、`Untracked files`、`Unstaged changes`、`Staged changes`（文件下面带 diff hunk）和最近 10 条 commit，顺序与 `magit-status` 一致。首次加载（buffer 还是空的）才在 buffer 里显示 `[|]`/`[/]` 这种 ASCII spinner（`Loading...`），git 返回后替换成内容；之后 `s`、`u`、`g`、`Gwrite` 等触发的刷新不清空 buffer，旧内容一直留在屏幕上，新内容准备好后一次性替换，所以看不到空白和 `Loading...` 闪烁。如果操作慢到 loader 已经出现，队列排空后 mode line 会把标签切成 `[|] Refreshing`，渲染完成时和内容一起消失。buffer 只读，`g` 刷新，`q` 关闭，`SPC g g` 打开。折叠方式和 Magit 一样：文件 diff 默认收起，`TAB` 折叠/展开光标所在的文件或 hunk，`S-TAB` 一次折叠/展开所有文件 diff。操作以 hunk 为单位：`s` 暂存光标处的 hunk、文件或整个 section，`u` 取消暂存，`X` 丢弃（`y/n` 确认，删除未跟踪文件也用它），`S`/`U` 暂存/取消暂存全部；`p` 立即打开 Magit 那样的推送菜单（`set-transient-map` + which-key 直接绘制，不等 which-key 的 idle delay，标签里写着目标 ref）：`p` 推到 push remote、`u` 推到 upstream、`e` 推到别处并在 minibuffer 里选 remote 和分支。选中后 push 照样进后台队列。写操作排成一条队列（`xl-git-status--queue`）：一次只跑一个 git 进程，连按 `s s s` 会按顺序执行而不是互相抢索引；操作超过 `xl-git-status-queue-delay`（默认 0.1 秒）才出现 loader，本地的 stage 通常还没来得及显示就结束了；loader 放在 mode line 而不是 header line，因为它只在运行时占位，header line 的出现/消失会让整个 buffer 上下挪一行，mode line 一直都在，所以在慢操作（比如 push）期间窗口也不会抖动；loader 又放在 mode line 的最左边（`mode-line-format` 前面加一个 `:eval`），因为 `mode-line-process` 的位置在 mode name 之后，窗口 split 变窄时正好被截掉，看不见；显示时是 `[|] Staged file-1.txt +2`，push 时是 `[|] Pushing master to origin`，没有帧数上限，再慢的 push 也一直转；队列排空后只刷新一次 status，而且是静默刷新。`s`/`u` 之后光标跟着同一个文件或 hunk 走（hunk 按内容匹配，不被重新编号带跑；目标文件 diff 是折叠的就展开），不会被扔回 buffer 顶部；丢弃或整个 section 消失时落在最近的标题上。没有 HEAD 的仓库里，`u` 会自动退回 `git rm --cached`；失败的输出用 `message` 报出来，不会吞掉；没有 upstream 的 push 也照此把 git 自己的提示报出来。`RET` 在文件行打开文件，在 hunk 上直接跳到该 hunk 行在文件中的位置，在 commit 行查看 commit（commit 窗口沿用同一套标题和 diff 高亮）。commit 视图也是异步的：buffer 立刻打开，显示 hash 和 spinner，`git show` 返回后填入 diff；重复请求同一个 buffer 时，旧结果会被丢弃。标题、状态、文件名、commit hash、分支/标签和 diff 行都有沿用主题 `font-lock` / `diff-mode` 配色的高亮。

### `lisp/xl-git-commands.el`（异步 G 命令）

vim-fugitive 风格的命令，都定义在这个文件里：

- `Gstatus`（`:Gstatus`）：打开 `lisp/xl-git-status.el` 的 status buffer。
- `Gblame`（`SPC g b`，或 `:Gblame`）：在当前文件左侧打开 blame 栏，每个源码行对应一行 commit、作者和日期，不同 commit 的 hash 用不同颜色区分（未提交的行保持暗色），跟随源码窗口滚动并高亮光标所在行；它是普通窗口，`C-w C-w` 可以切换过去，`:only`、`C-w c`、`:x` 也能关掉。`git blame` 在后台跑：窗口立刻打开，先铺好与源码行数相同的占位行（滚动和光标依然对齐），mode line 最左边转 spinner（窗口很窄，header line 会占行，放在 mode line 左边才不会被截掉）；第一次打开、窗口还是空白时，窗口正中间也显示同一个 spinner（零宽 overlay 的 `before-string`，不插入行，占位行仍与源码对齐，两处由同一个时钟绘制）；数据到了就替换，中间的 spinner 立即消失；失败时错误也写在那里，不会挤掉占位行；保存后的刷新也保留旧内容，不会闪。`RET` 查看该行 commit，`g` 刷新，`q` 关闭。
- `Gwrite`（`:Gwrite`）：暂存当前文件。
- `Gcommit`（`:Gcommit`）：用 `Update <文件名>` 提交当前文件。

`Gwrite` 和 `Gcommit` 也不再经过 evil 的 `!`：它们用 `make-process` 在文件所在仓库里跑，成功后刷新同一个仓库的 status buffer，失败时把 git 的输出 `message` 出来。所以 pre-commit hook 跑几秒也不会卡住 Emacs，而且没有 evil 也能用。

它依赖 `lisp/xl-git-status.el` 提供的 status buffer、faces 和 commit 窗口，所以在其后加载。

### `lisp/xl-package-menu.el`（Package Menu loader）

`list-packages` 在 Emacs 31 里本来就是异步的：`package-menu-async` 默认开着，`package-refresh-contents` 把下载记进 `package--downloads-in-progress`，菜单先显示缓存，下载完再由 `package--post-download-archives-hook` 刷新。所以这里没有"重写"任何东西——重写只会比上游更差。

它只补上游没有的一件事：把 mode line 里静态的 `:Loading` 换成和 Git buffer 一样的动画 loader。

打开 `list-packages` 时，原来的整个列表要在主线程上连续算完，约 0.7 秒界面完全不动。`lisp/xl-package-menu.el` 把它分块做：先分块读取 archive（`list-packages` 期间跳过 `package-initialize` 里那次同步读取），再分块计算每个包的状态，排序之后分块追加行，块与块之间让出主线程。刷新时新行接在旧行后面，全部完成后才删掉旧行，并按行的 id 恢复窗口顶部和光标所在的行，所以屏幕不会跟着翻动；中途开始新的列表时，已追加的半成品会被撤掉。同一份数据得到相同的行、文字和光标位置（测试比较过）。实测最长一次阻塞从约 0.72 秒降到约 0.07 秒，总耗时约多 0.17 秒。只有"显示全部包、不带关键字过滤"这一种情况走分块路径，其它情况仍是 package.el 的函数。

刷新结束后的收尾也改了：package.el 的 `tabulated-list-revert-hook` 会同步重算全部包的状态（约 0.2 秒），`package-menu--post-refresh` 整表重打印（约 0.3 秒），mode line 逐行查询状态（约 0.5 秒）。现在收尾改为分块列表，mode line 一次遍历统计，"可升级"数量由子进程给出（回来之前显示 `?`）。下载完成后 package.el 自己的 archive 读取（约 0.15 到 0.25 秒）仍在主线程上。

刷新结束后，package.el 会在回显区报告"可升级 N 个包"。这个搜索在主线程上第一次运行要约 0.25 秒，现在由 `lisp/xl-package-upgrades.el` 放到子 Emacs 里做：子进程自己合并 archive，只接收已安装包的版本，回答是包名列表，消息内容不变，只是晚一点出现。`U` 标记升级等用户主动的命令仍然是同步搜索，因为它们马上要用结果。子进程失败时回退到 package.el 原来的搜索。

- `package-menu-mode-hook` 装上 `xl-spinner` 的 mode line 槽位（见下），`mode-line-process` 只留 transaction status
- `package-refresh-contents-hook` 在刷新开始时启动 loader，标签是 `Loading archives`
- `package--post-download-archives-hook` 在最后一个下载完成时停掉它
- 一分钟还没收到完成信号就自行放弃，坏掉的 archive 不会让 loader 永远转

异步是 Emacs 的，loader 是我们的。

### `lisp/xl-async.el`（共用异步进程层）

status、GBlame、commit 窗口和 G 命令原本各写一份 `make-process`、sentinel、输出 buffer 和"这个回答是否已过期"的判断。现在都走 `xl-async-run`：

```elisp
(xl-async-run
 :command '("git" "blame" "--line-porcelain" "--" "init.el")
 :directory root
 :slot 'blame                          ; 同一 buffer 同一槽位，新请求取消旧请求
 :stdin patch                          ; 可选：写入标准输入（git apply 不再需要临时文件）
 :on-done (lambda (output code) ...))  ; 在 owner buffer 里回调；被取消或 buffer 已关则不回调
```

关掉 owner buffer 会杀掉它的进程，切换 major mode 也不会丢失这份归属；`:owner nil` 用于不该随 buffer 结束的命令（`Gcommit` 的 hook）。`xl-async-enqueue` 是串行队列：一次一个 job，失败时尝试下一个命令，清空时回调一次，status 的 `s`/`u`/`X`/push 都用它。

为什么不用社区的库：`async.el` 解决的是"在子 Emacs 里跑 Elisp"，不是管理外部命令；`deferred`、`promise`、`aio` 是通用的 promise 风格，会多一层依赖和抽象，而这里需要的只有归属、槽位取消和队列，`make-process` 之上一百多行就够了。数据仍是 git 的文本格式（`status -z`、`--line-porcelain`），解析留给调用方。

### `lisp/xl-org-babel-async.el`（后台执行 shell 代码块）

源码块加上 `:async yes` 就在后台运行，`C-c C-c` 立即返回。没有这个参数，块照常同步执行，结果和顺序都不变。`:session` 的块不能异步，会同步执行并给出提示。

输出分两层，长度不影响编辑：

- 运行中：每一块输出都进入 `*Org Babel Output: 文件:行号*`；块下面只显示最后 `xl-org-babel-live-lines`（默认 20）行。
- 结束后：写入文件的是前 `xl-org-babel-result-lines`（默认 200）行，后面附一行说明，完整输出仍在输出 buffer 里。

结果位置用 marker 记住，运行时在上方增删文字不会放错；块被删掉时，结果只留在输出 buffer。`C-c C-k` 取消光标所在的块（保留已经输出的内容）；同一个块再次运行时，取消仍在运行的那次。`:timeout 秒数` 超时后停止进程。

运行中的块在 `#+RESULTS:` 行上显示 spinner（overlay，不改文件）；buffer 的 mode line 最左边同时显示 `●·· 2 blocks running`，块不在屏幕上时也能一眼看出这个 buffer 有后台任务。两处由同一个 spinner 时钟绘制。

块之间是并行的，不排队：彼此依赖的块需要自己按顺序执行。

`lisp/xl-async.el` 为此新增了 `:on-output`（每次有输出就回调一次）和 `:timeout`（超时后杀掉进程，退出码为 124，和 coreutils `timeout` 一致）。

### `lisp/xl-spinner.el`（共用 loader）

目标是既好看、又醒目，但不让屏幕动。分两层。

**内核**只管时钟和帧序号，画在哪里交给调用方。status 首次加载和 commit 视图用它在空 buffer 里画 overlay：buffer 里本来没有内容，所以没有东西会被推开。

```elisp
(xl-spinner-start
 (lambda (text)                       ; text 为 nil 时表示停止，负责擦掉
   (overlay-put my-overlay 'display text))
 :limit 600)                          ; 可选：最多走多少帧（0.1 秒一帧，600 帧一分钟）
```

**Mode line 槽位**给已经有内容的 buffer 用：status 的刷新和写操作队列、GBlame、Package Menu。它在 mode line 最左边，窄的分屏也截不掉；在 buffer 里临时插一行会把内容整体下移，看起来像刷新，所以不放在 buffer 里。

```elisp
(xl-spinner-mode-line-install)              ; major mode 里装一次
(xl-spinner-mode-line-start "Pushing main") ; 0.1 秒后才出现
(xl-spinner-mode-line-label "Refreshing")   ; 换标签
(xl-spinner-mode-line-stop)                 ; 或 (xl-spinner-mode-line-stop "git blame failed: ...")
```

不抖屏靠下面几条，每条都有测试：

- **帧只用 mode line 自己字体里的字形。** 字体缺的字形会从 fallback 字体借，GUI 下 mode line 会因此变高：实测 JetBrains Mono 下 `⠋`（来自 Apple Braille）和 `▰` 让 mode line 从 23px 变成 24px，loader 一出一收，整个窗口就跟着跳一下。所以默认的 `xl-spinner-style` 为 `auto`：GUI 里只有当 `char-displayable-p` 报告圆点（`●··` `·●·` `··●`）由 mode line 的字体自己画时才用它，否则退回 ASCII `[|] [/] [-] [\]`。`●` 和 `·` 在 JetBrains Mono、Menlo、Consolas 里都有，所以 macOS、Windows 和大多数 Linux 看到的是同一个动画。终端里（ssh 到 Linux）字符排在固定网格里，高度不会变，但看不到终端用的字体，所以只在 UTF-8 终端、且字符为单宽时用圆点；`LANG=C` 之类会退回 ASCII。终端字体缺圆点时，可以把 `xl-spinner-style` 设为 `ascii`。
- **出现前等 0.1 秒，出现后至少留 0.3 秒**（`xl-spinner-show-delay`、`xl-spinner-min-duration`）。本地 stage 在出现前就结束，mode line 完全不动；刚过 0.1 秒就结束的操作也不会一闪而过。
- **忙碌期间槽位只会变宽不会变窄。** 标签从 `Pushing main to origin` 换成 `Refreshing` 时用空格补齐，后面的 buffer 名不会左右来回移动。留存期间来了新操作，接着用同一个 loader，只换标签。
- **醒目靠颜色，不靠尺寸。** 圆点用 `xl-spinner` face（继承主题的 keyword 色）；忙碌时 mode line 的背景向这个颜色混 20%（`xl-spinner-tint`，设为 nil 关闭），只改背景色，不改 box 和高度，少于 256 色的终端不着色。
- **0.1 秒一帧**（`xl-spinner-interval`）：看得出在动，又不闪，ssh 上重绘的数据量也少一半。

实测（GUI Emacs、JetBrains Mono 18pt）：从空闲、出现并着色、连续三帧、换标签到收起，mode line 一直是 23px，正文高度和 `window-start` 不变。

### `xueliang.el`（个人层，不发布）

我自己的全部设置，按段落组织：

- 平台、路径、字体（所有文字 face 统一使用选中的字体）、主题；可选的 `~/.emacs-profiles/local.el`（只放机器相关的路径，不进仓库）。
- macOS UK 键盘：Option-3 本应输入 `#`，但 Emacs 把 Option 当作 Meta，收到的是 `M-3`（数字前缀）。现在插入态、替换态、Emacs 态、minibuffer（含 evil 的 `:` 和搜索命令行）和 isearch 里都输入 `#`，普通态保留 `M-3` 的数字前缀。
- 全局设置：备份、recentf、which-key、缩进、长行不换行、保存时自动建目录。
- evil 习惯：`vs`/`sp`、`C-u` 向上翻页、`*` 按整个符号搜索、IME 切换。
- 补全：vertico、corfu 等由 Crafted 打开，这里只放我的改动。候选列表占 frame 高度的 38.2%，`SPC r l` 重复上一次列表，候选列表里 `C-.` 调出 embark 动作、`C-q` 导出、`M-w` 也导出（和 `C-q` 一样，导出结果在 buffer 里可见）；corfu 输入一个字符就立即弹出（Windows 上改为停顿 0.2 秒、输入第二个字符后才查询，避免打字卡顿），eshell 等不自动弹出，弹窗背景、当前候选项和边框跟随主题（切换主题时自动更新）；cape 的补全来源加到全局（Crafted 只加到了 `*scratch*`），dabbrev 只扫描当前 buffer。
- Snippet：`SPC i s` 用 yasnippet 列出当前模式的 snippet（`SPC i S` 新建），首次使用时才加载 yasnippet。
- Org：`org-modules` 保持为空，但显式 require `org-tempo`，所以插入态里 `<s` + TAB 也会展开成 `#+begin_src` 代码块。
- 持久 scratch：`SPC b s` 打开 `*scratch*`，persistent-scratch 在启动时恢复内容、空闲 30 秒和退出时保存到 `~/.emacs-profiles/crafted/persistent-scratch.eld`，`C-x C-s` 立即保存；再按一次 `SPC b s` 跳到已有的 scratch 窗口，不重复分屏。batch（测试、安装脚本）不会读写这份笔记。
- dired、eshell、Org、magit 的设置和小函数。
- 功能键 F2–F12，和 `SPC` 下我自己的几条命令。
- F7（`just all`）和 `SPC c c` 通过 `compile` 异步跑 `just`：输出流进 `*compilation*` 并保留，失败时 `next-error` 能跳到出错的 `文件:行`；之前的 `!just all &` 把输出丢在临时 buffer 里，失败也看不见。

这个文件不应该出现 `spacemacs/`、`doom`、`crafted-` 开头的符号，这样换掉框架时它不用改。

## 加载顺序

启动时按这个顺序执行（括号里是对应的文件）：

1. Emacs 31.1 按 `--init-directory` 读 `~/.emacs-profiles/crafted/early-init.el`（存根）。
2. 存根设置 `xl-dotfiles` 和 `xl-crafted-home`，加载 `emacs/early-init.el`。
3. `emacs/early-init.el` 设置 GC、界面、JIT、包目录，最后加载 `crafted-early-init-config`。
4. Emacs 进入初始化，运行 `before-init-hook`：Crafted 检查包列表是否为空，为空则刷新。
5. Emacs 读 `~/.emacs-profiles/crafted/init.el`（存根），加载 `emacs/init.el`。
6. `emacs/init.el` 检查版本，加载 `custom.el`，加载 `crafted-init-config`。
7. 加载 `crafted-completion-packages`、`crafted-evil-packages`、`crafted-org-packages`，加入 `xl-packages`，再调用 `package-install-selected-packages` 装包。
8. 加载 `crafted-defaults-config`、`crafted-completion-config`、`crafted-evil-config`、`crafted-org-config`、`crafted-startup-config`。
9. 修正 `evil-want-C-h-delete` 和 ispell 钩子。
10. 把 `emacs/lisp/` 加入加载路径，`require` `xl-key-bindings.el`，建立 `xl-leader-map` 和 SPC 键树。
11. 加载 `emacs/lisp/xl-git-status.el`，定义不依赖 Magit 的异步 status buffer 和 commit 窗口。
12. 加载 `emacs/lisp/xl-git-commands.el`，定义 `Gstatus`、`Gblame`、`Gwrite`、`Gcommit` 及对应的 Ex 命令。
13. 加载 `emacs/lisp/xl-package-menu.el`，给 Package Menu 装上刷新用的动画 loader。
14. 加载 `emacs/xueliang.el`：先读可选的 `local.el`，再依次设置外观、全局选项、evil、vertico、corfu、dired、eshell、Org、magit、函数、功能键，最后绑定 `SPC b s`、`SPC c c`、`SPC f p`、`SPC g g`。
15. `emacs/init.el` 把 `xl-packages` 补回安装名单，并挂到切换主题的钩子上。
16. 启动完成，显示 Crafted 首页（底部显示启动耗时，`SPC b h` 随时可以回到首页）。如果这次启动来自 `SPC q r`，`initial-buffer-choice` 改为打开上次的文件/dired 并恢复 point 和 window-start，这时不显示首页。

Org、dired、magit 等包的设置放在 `with-eval-after-load` 里，包被第一次用到时才执行，所以它们不拖慢启动。

## 为什么这样排

- **装包在加载任何模块和个人配置之前**：它们一加载就会用到这些包。
- **Crafted 模块先于 `xl-key-bindings.el`**：键位需要 evil 已经加载。
- **`xl-key-bindings.el` 先于 `xueliang.el`**：`xl-leader-map` 由前者建立，后者往里加个人绑定；同名键以后加载的为准。
- **`evil-collection-key-blacklist` 在加载 Crafted 模块之前设置**：这样 evil-collection 一开始就不会抢走 SPC 和 `,`。

## 常见修改

| 想做的事 | 改哪里 |
| --- | --- |
| 增加或删除一个包 | `init.el` 里的 `xl-packages`（Crafted 模块已提供的包不用再列） |
| 调整补全（vertico、corfu 等） | `xueliang.el` 的补全段落；整套补全栈的开关在 `init.el` 的 `crafted-completion` |
| 改一个个人键位或命令 | `xueliang.el` |
| 改 Spacemacs 风格的默认键位 | `lisp/xl-key-bindings.el` |
| 增加或去掉一个 Crafted 模块 | `init.el` 里的 `require` |
| 升级 Crafted | 改 `frameworks.lock` 的提交号，再执行 `just emacs-crafted-install` |
| 升级 Emacs、重建 profile 或新增包之后 | 执行 `just emacs-trampolines-cache-setup`，避免第一次启动白屏 |
| 改启动期设置（GC、JIT、包目录） | `early-init.el` |
| 改 Package Menu 的 loader | `lisp/xl-package-menu.el` |

修改后用下面这几条命令检查：

```bash
just emacs-smoke   # 完整加载一遍，只证明没有初始化错误
just emacs-test    # ERT 测试：键位、钩子、变量、文件结构、真实操作
just emacs-check   # 上面两条一起跑

# 没装 just 的机器上，这三条等价：
bash emacs/install/emacs-setup.sh smoke
bash emacs/install/emacs-setup.sh test
bash emacs/install/emacs-setup.sh check
```

### `just emacs-test`

`emacs/tests/smoke-tests.el` 里的 109 个 ERT 测试在配置完整加载后运行，覆盖：

| 范围 | 例子 |
| --- | --- |
| 文件结构 | `xl-packages` 定义在 `init.el`、`xueliang.el` 里没有；`lisp/xl-key-bindings.el` 先于个人配置加载；个人文件不出现 `crafted-`、`spacemacs/` 等框架符号 |
| 启动设置 | 包目录按 Emacs 版本分开；菜单栏、工具栏关闭；evil 系列锁定 MELPA |
| 装包 | 缺包时先刷新归档再安装（旧缓存会让 compat 31.1 之类的新依赖被误报 unavailable）；刷新后仍缺包会带着包名报错 |
| 键位 | `SPC`/`M-m` 树、`SPC f f`、`SPC g g`、`SPC b s`、`SPC i s`/`SPC i S`、`SPC *`/`SPC /` 搜索、F2–F12、`SPC t r`、`SPC T n`、macOS UK 键盘 Option-3 输入 `#` |
| 主模式 leader | `,` 和 `SPC m` 在 Org、Emacs Lisp、其它模式中的解析 |
| Org | 变量、TODO 关键字、链接缩写、表头 face；`org-tempo` 的 `<s` + TAB 展开；打开文件后 `org-indent`、`org-superstar`、`evil-org` 和截断长行 |
| Magit | `h/j/k/l` 在 Magit 里保持 vim 方向；在临时 git 仓库里真的打开一次 status |
| GBlame | 每个源码行一行 blame；未提交行、光标高亮和滚动同步；`git blame` 返回前已有对齐的占位行，mode line 最左边的槽位在等 loader；失败时错误也留在 mode line，占位行不动；`:Gblame`、`Gwrite`、`Gcommit` 命令 |
| Package Menu | 刷新时 mode line 最左边出现动画 loader、帧会变；完成钩子把它停掉；静态 `:Loading` 去掉，transaction status 保留 |
| 静默刷新 | 已有内容时刷新（`g`）不清空 buffer、不出现 `Loading...`；本地的 `s` 不显示 loader（超过延迟才出现）；一个慢任务结束后 mode line 最左边变成 `Refreshing` 且旧内容还在，渲染完成才替换 |
| Loader | `xl-spinner` 首帧立即绘制、逐帧推进、`:limit` 到点自停并清除；字体或终端画不了圆点时退回 ASCII；mode line 槽位在延迟内结束就完全不出现、出现后留够最短时间、换标签不变窄、错误文本立即替换并转义 `%`；着色按比例混色 |
| Commit 视图 | 打开时已有 spinner 和「Loading...」、还没有 diff；`git show` 返回后 spinner 消失、diff 就位 |
| 远端同步 | `f` 在队列里 `git fetch` 上游的 remote，mode line 显示 `Fetching from origin`，完成后 status 重新渲染，远端分支的变化可见；`F` 执行 `git pull --ff-only`，没有 upstream 时直接报错，不分叉、不弹合并编辑器；分叉时 git 的错误显示在 echo area |
| 写操作队列 | 连按两次 `s` 时命令立即返回、loader 在跑、动作写在 mode line 最左边；push 菜单由 `set-transient-map` 立即弹出、标签带目标 ref，选 push remote / upstream / elsewhere 后立即返回并一直转 spinner；排空后两个文件都进了索引；没有 HEAD 的仓库能 `u` |
| 光标保持 | 暂存文件后光标留在同一文件（已到 Staged changes），取消暂存再跟回来；暂存 hunk 后光标落在 staged 的同一行，并展开目标文件 diff；丢弃后落在最近的标题 |
| Dired / Eshell | `-alh`、`TAB` 预览、`h/l/RET`；eshell 的插入态键位和 `eshell/e` 等别名 |
| 补全 | vertico/consult/corfu 状态；Corfu 弹窗的背景、当前候选项和边框跟随主题；补全栈的包由 Crafted 选中、已安装，切换主题后也不会被 `package-autoremove` 当作可清理 |
| 首页 | 底部显示两位小数的启动耗时；最近文件最多 9 个 |
| 重启 | 文件和 dired 保存位置并在下次启动恢复；临时 buffer 不保存、回到首页；取消重启会删掉保存的位置；`SPC q r` 绑定到 `xl-sk-restart-emacs` |
| 其它 | 主题、`xl-idle-require`、保存时建目录、按输入顺序选择候选 |

测试不启动图形界面，全部在 `emacs --batch` 里完成。

## 不装 just 也能用

`justfile` 里的 `emacs-*` 配方现在只是 `emacs/install/emacs-setup.sh` 的薄包装：检查（平台、Emacs 30+、profile）、克隆 Crafted、生成存根、trampoline 缓存全部在脚本里。`setup-macos` / `setup-linux` / `setup-windows` 按步骤打印 `[1/5] ...` 这样的标题；`Install Emacs packages` 逐条显示 `package i/N`，`Prebuild advice trampolines` 逐条显示 `trampolines i/N`，结束时显示总耗时。`NO_COLOR=1` 关闭颜色；步骤内部的其它输出默认隐藏，失败时才打印完整日志；`XL_VERBOSE=1` 可以看到全部输出。所以没有 `just` 的机器照样能装：

```bash
bash emacs/install/emacs-setup.sh setup-macos     # 或 setup-linux / setup-windows
bash emacs/install/emacs-setup.sh crafted-install # 只重建 profile 存根
bash emacs/install/emacs-setup.sh check           # smoke + ERT
bash emacs/install/emacs-setup.sh trampolines     # 预编译 advice trampoline
bash emacs/install/emacs-setup.sh make-default    # ~/.emacs.d -> profile
bash emacs/install/emacs-setup.sh restore-spacemacs
bash emacs/install/emacs-setup.sh try             # 启动，参数原样传给 Emacs
```

环境变量和 `just` 版含义一致：`EMACS`、`PROFILE`、`PROFILES`、`PROFILE_DIR`、`DOTFILES`。另外两个：

| 变量 | 作用 |
| --- | --- |
| `XL_SKIP_DOTFILES=1` | `setup-*` 时跳过 `bash env.sh`（只装 Emacs 部分） |
| `XL_FORCE_WINDOWS=1` | 在非 Windows 上测试 `setup-windows` 的流程 |

如果第一次运行报 `Package ‘compat’ (version 31.1) is unavailable`，说明那份 profile 的归档缓存比依赖还旧：脚本现在会在缺包时自动刷新，重跑一次 `setup-*` 即可；在 Emacs 里也可以先 `M-x package-refresh-contents` 再启动。

`just emacs-mac-setup` 与 `bash emacs/install/emacs-setup.sh setup-macos` 等价，前者只是把 `EMACS`、`PROFILE*`、`DOTFILES` 放进环境再调用脚本。

## 在 macOS 上安装

在终端里一条命令完成：

```bash
just emacs-mac-setup                          # 装了 just
bash emacs/install/emacs-setup.sh setup-macos    # 没装 just
```

它依次做五件事：复制 vim/nvim/tmux 配置和 `~/bin` 脚本；按 `frameworks.lock` 克隆 Crafted 并生成 `~/.emacs-profiles/crafted` 存根；在 batch 里完整加载一遍配置，顺便装好所有包（有初始化错误会直接失败）；再加载一遍并预先编译 advice trampoline 缓存，第一次 GUI 启动就不会白屏；最后把 `~/.emacs.d` 指向 Crafted。最后一步要求 Emacs 已经退出；如果它正在运行，前面的步骤仍会完成，退出 Emacs 后再执行 `just emacs-make-default` 即可。

之后用 `just emacs-try`（或 `bash emacs/install/emacs-setup.sh try`）启动 Emacs，或者直接打开 Dock 里的 Emacs。

## 在 Windows 上安装

在 Git Bash 里一条命令完成：

```bash
just emacs-windows-setup                       # 装了 just
bash emacs/install/emacs-setup.sh setup-windows   # 没装 just
```

### 准备

1. **下载能运行的 Emacs**：GNU 官方的 Windows 版放在 `https://ftp.gnu.org/gnu/emacs/windows/emacs-31/`。要下载 `emacs-31.1.zip`（或安装程序），不是 `emacs-31-deps-mingw-w64-src.zip`：后者只是依赖库的源码，不能运行。
2. **解压并加入 PATH**：比如解压到 `C:\Emacs`，再把 `C:\Emacs\bin` 加入用户的 `PATH`。用资源管理器或 7-Zip 解压都可以，要保留文件的修改时间，否则 Emacs 会改为解压加载源码，明显变慢。
3. **Git Bash 和 just**：两者都要在 `PATH` 里。在 Git Bash 里能运行 `emacs --version` 就说明准备好了。

### `just emacs-windows-setup` 做了什么

1. 检查 `emacs` 存在且版本不低于 30。
2. 如果 Windows 用户变量 `HOME` 没有设置，把它设为你的用户目录。从开始菜单启动的 Emacs 只看这个变量；没有它，`~` 会指向 `%APPDATA%`，配置和 `~/workspace/...` 的路径都会找不到。
3. 把已有的 `~/.emacs`、`~/.emacs.el`、`~/.emacs.d` 移到 `~/.emacs-profiles/backup-日期时间/`，不删除。`~/.emacs` 和 `~/.emacs.el` 存在时，Emacs 会优先读它们而忽略 `~/.emacs.d`。
4. 按 `frameworks.lock` 克隆 Crafted，把两个存根直接写进 `~/.emacs.d`。和 macOS 不同，这里不用符号链接：Git Bash 的 `ln -s` 默认会复制而不是链接。
5. 以批处理方式加载一遍配置，顺便装好所有包，最后打印 `smoke ok`。

可以重复运行：`~/.emacs.d` 已经是它生成的就保留，只更新存根和缺少的包。

### 平台差异

| 项目 | Windows 上的情况 |
| --- | --- |
| 原生编译 | 官方依赖包里没有 libgccjit，所以没有原生编译；配置本来就关闭了 JIT，没有影响 |
| dired | Windows 没有 `ls`，Emacs 用内置的 `ls-lisp` 模拟，`-alh` 参数可以用 |
| 字体与字号 | 依次找 JetBrains Mono、Consolas 等已安装字体，`default`、`fixed-pitch`、`fixed-pitch-serif`、`variable-pitch` 统一使用它；Windows 上字号单独设为 120 |
| 补全性能 | corfu 在 Windows 上停顿 0.2 秒、输入第二个字符后才查询补全后端；`cape-dabbrev` 只扫描当前 buffer，避免逐字符扫描所有同模式 buffer |
| macOS 专属 | macism 输入法切换、`s-x`、UK 键盘 Option-3 输入 `#` 等只在 macOS 生效，Windows 上自动跳过 |
| `SPC *`、`SPC s p`、`SPC p s` | 需要 `rg`（ripgrep）；eshell 的 `fzf` 改用 `consult-fd`（有 `fd` 时）或 `consult-find`，没有 `fzf` 也能用。可以用 `winget install BurntSushi.ripgrep.MSVC` 安装 rg |
| Org 的 shell 代码块 | 用 `cmd.exe` 执行，为 bash 写的代码块可能无法运行 |
| 机器相关路径 | 链接文件、笔记目录等写进 `~/.emacs-profiles/local.el`，不进仓库 |

### 换行符

仓库根目录的 `.gitattributes` 让所有文本文件在 Windows 上也保持 LF 换行。Git for Windows 默认会把它们转成 CRLF，这会让 `justfile` 里的 bash 脚本报 `\r: command not found`，也会让读取 `frameworks.lock` 的提交号多出一个 `\r`。

在 `.gitattributes` 加入之前就克隆过的仓库，在工作区没有未提交修改的前提下，运行一次下面的命令，按新规则重新检出：

```bash
git rm -r -q --cached . && git reset --hard
```

### 还没在真实 Windows 上验证的

上面的安装流程是在 macOS 上模拟 Windows 跑通的：用临时的 HOME，验证了移走旧配置、生成存根、装包和重复运行。下面几项只有在 Windows 上才能确认：设置 `HOME` 用户变量（`powershell.exe` 和 `setx`）、`cygpath` 路径转换、官方安装包是否自带 gnutls 等 DLL（装包需要 HTTPS）。第一次在 Windows 上运行时请留意这几步的输出。
