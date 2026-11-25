# Prompt：实现 xl-perfetto-trace.el

把下面这段完整交给实现的 agent。

---

你在 `~/workspace/dotfiles` 这个仓库里工作，任务是实现一个新的 Emacs Lisp 模块 `emacs/lisp/xl-perfetto-trace.el`：录制一段时间内 Emacs 在 Lisp 层做了什么，写成 Chrome JSON trace 文件，在 ui.perfetto.dev 里打开看时间线。

## 先读这些

1. `AGENTS.md`：仓库的代码风格和规则。
2. `emacs/ideas/xl-perfetto-trace.md`：**完整设计，按它实现**。命令名、变量名、事件格式、轨道编号、生命周期、测试计划、验收标准都在里面。设计没有覆盖的细节，你自己选一个合理的做法，并在最后的报告里列出来。
3. `emacs/lisp/xl-async.el`：`xl-async-run` 的参数（`:command`、`:name`、`:on-done`、`:slot`、`:owner`、`:timeout`），以及 `xl-async-cancel`。你要给 `xl-async-run` 加 advice。
4. `emacs/lisp/xl-dired.el` 和 `emacs/tests/xl-dired-tests.el`：一个 xl-* 模块和它的测试应该长什么样（文件头、defgroup、defcustom、测试的临时目录和等待写法）。
5. `emacs/install/emacs-setup.sh` 里的 `cmd_test`：测试是怎么跑的。

## 必须遵守

- **不改 Emacs 的 C 代码，不依赖外部程序。**
- **不对任何 C 原语加 advice**（`subr-primitive-p` 为真的函数，比如 `make-process`、`set-process-filter`、`garbage-collect`）。原因是 native-comp 的 trampoline 会同步编译、冻结界面。进程用 `add-function :around (process-filter proc)` 包装，新进程靠录制期间的定时扫描发现，设计文档里写了具体做法。
- **不录制时零开销**：hook、advice、定时器、process function 包装只在 start 和 stop 之间存在。stop 必须幂等，并且一定移除全部插桩（`unwind-protect`）。start 中途失败要回滚。
- **不记录内容**：不记录按键序列、buffer 文本、minibuffer 输入、进程输出。
- 代码风格：`lexical-binding: t`，2 空格缩进，kebab-case，公开名字用 `xl-perfetto-` 前缀，内部用 `xl-perfetto--`。只用普通 Emacs API，不出现 `crafted-`、`spacemacs/` 这类框架专有名字（有 smoke 测试会检查）。
- 只改这些文件：
  - 新建 `emacs/lisp/xl-perfetto-trace.el`
  - 新建 `emacs/tests/xl-perfetto-trace-tests.el`
  - `emacs/install/emacs-setup.sh`：在 `cmd_test` 里加一段运行新测试
  - `emacs/tests/smoke-tests.el`：在 `xl-personal-file-is-framework-neutral` 的文件列表里加上新文件
  - `emacs/init.el`：加一行 `(require 'xl-perfetto-trace)`
  - `emacs/ideas/xl-perfetto-trace.md`：只在"性能预算"一节写入实测结果，以及把"状态"改成已实现
- 不要改其它文件。工作区里如果有别人未提交的改动，不要碰，也不要提交它们。
- **不要提交**，除非用户明确要求。如果用户要求提交，提交信息最后一行是 `Signed-off-by: Xueliang Zhong <xueliang.zhong@gmail.com>`。

## 怎么验证

1. 字节编译没有警告（来自可选依赖的"函数未定义"警告除外）：
   ```bash
   emacs --batch -Q -L emacs/lisp -f batch-byte-compile emacs/lisp/xl-perfetto-trace.el
   ```
   编译完删掉生成的 `.elc`。
2. 新测试单独跑：
   ```bash
   emacs --batch -Q -L emacs/lisp -l emacs/tests/xl-perfetto-trace-tests.el -f ert-run-tests-batch-and-exit
   ```
3. 全部测试：
   ```bash
   bash emacs/install/emacs-setup.sh test
   ```
   必须全部通过。如果有和你的改动无关的失败，报告出来，不要去改别人的测试。
4. 在 batch 里真实录制一次：开始录制，跑几个命令、一个定时器、一次 `garbage-collect`、一个 `xl-async-run`，停止，用 `json-parse-string` 读回生成的文件，确认每个轨道都有事件。

GUI 里的验收（在 ui.perfetto.dev 打开真实录制的文件）你做不了，在报告里说明，留给用户。

## 完成后报告

用中文，先说结论，然后：
- 做了什么，每个文件改了什么。
- 测试结果（三个套件各多少个、是否全部通过）。
- 设计没有覆盖、由你决定的地方，每条一句话说明选择和理由。
- 没做或没验证的部分（比如 GUI 验收），以及原因。
