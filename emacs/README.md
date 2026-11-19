# Emacs 配置说明

这个目录是 Emacs 配置的全部内容。Emacs 31.1 启动时，先读 `~/.emacs-profiles/crafted` 里的两个存根，存根再把控制权交给这里的文件。

## 架构

```
just emacs-try
  └─ emacs --init-directory ~/.emacs-profiles/crafted   (Emacs 31.1)
       ├─ ~/.emacs-profiles/crafted/early-init.el   存根：设 xl-dotfiles、xl-crafted-home
       │    └─ emacs/early-init.el                  启动期设置，加载 Crafted 包源配置
       └─ ~/.emacs-profiles/crafted/init.el         存根
            └─ emacs/init.el                        选 Crafted 模块、装包、按顺序加载
                 ├─ Crafted 模块（~/.emacs-profiles/src/crafted-emacs，锁定提交）
                 ├─ emacs/spacemacs-keys.el         Spacemacs 键位习惯（SPC 树等）
                 └─ emacs/xueliang.el               我自己的设置、键位、命令
```

四层的分工：

| 层 | 文件 | 谁负责 |
| --- | --- | --- |
| 运行时 | Emacs 31.1、elpa 包目录 | 系统与包管理 |
| 框架 | Crafted 模块 | 上游项目，版本由 `frameworks.lock` 锁定 |
| 适配 | `early-init.el`、`init.el`、`spacemacs-keys.el` | 把 Crafted 接到我的习惯上 |
| 个人 | `xueliang.el` | 我的全部习惯，只用原生 Emacs 和包的接口 |

## 文件说明

### `frameworks.lock`

记录 Crafted Emacs 的仓库地址和锁定的提交号。`just emacs-crafted-install` 读这个文件，把 Crafted 克隆到 `~/.emacs-profiles/src/crafted-emacs` 并检出该提交。升级 Crafted 时改这里的提交号，然后重新执行安装命令。

### `early-init.el`（适配层）

Emacs 在创建界面之前读它，所以只放启动期的设置：

- 启动时关掉 GC，启动完成后恢复到 128 MB。
- 关掉菜单栏、工具栏、滚动条。
- 关闭 JIT 原生编译和自动原生编译。
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
- 汇总要安装的包：Crafted 的 completion、evil、org 三组包，加上 `xl-packages`（Crafted 不提供、而 `spacemacs-keys.el` 和 `xueliang.el` 要用的包），然后一次装好缺少的。
- 加载 Crafted 的 defaults、completion、evil、org、startup 五个模块。其中 completion 负责整个补全栈：vertico、consult、marginalia、orderless、embark、corfu、cape。
- 修正 Crafted 默认值中与我的习惯冲突的部分：插入态 `C-h` 保持为帮助前缀，关闭全局拼写检查，并去掉 Crafted 挂在 `embark-collect-mode-hook` 上的 `consult-preview-at-point-mode`（Consult 3.4 已删除该函数，不去掉会让 Embark Collect 报错）。
- 加载 `spacemacs-keys.el`，然后加载 `xueliang.el`。
- 最后把 `xl-packages` 补回安装名单，并在每次切换主题后再补一次：加载主题会恢复 `custom.el` 里保存的旧名单，不补回的话 `package-autoremove` 可能误删正在用的包。

### `spacemacs-keys.el`（适配层）

复刻 Spacemacs 的键位习惯，Crafted 本身没有这些：

- SPC 键树（`b f g j p q r s t w` 等分组）和 `M-m`。
- `SPC i s` 列出当前模式的 snippet，`SPC i S` 新建 snippet，位置和 Spacemacs 的 auto-completion 层一致。
- `,` 与 `SPC m` 主模式 leader，目前对 Org 和 Emacs Lisp 生效。
- `fd` 退出插入态，evil-surround，窗口编号 `SPC 1`–`9`。
- 首页（`SPC b h`），列出最近文件，按数字打开。
- 创建 `xl-leader-map`，供 `xueliang.el` 继续往里加绑定。

它在 `xueliang.el` 之前加载，所以 `xueliang.el` 里的绑定会覆盖这里的默认值（例如 `SPC SPC`、`SPC x`）。

### `xueliang.el`（个人层）

我自己的全部设置，按段落组织：

- 平台、路径、字体（所有文字 face 统一使用选中的字体）、主题；可选的 `~/.emacs-profiles/local.el`（只放机器相关的路径，不进仓库）。
- macOS UK 键盘：Option-3 本应输入 `#`，但 Emacs 把 Option 当作 Meta，收到的是 `M-3`（数字前缀）。现在插入态、替换态、Emacs 态、minibuffer（含 evil 的 `:` 和搜索命令行）和 isearch 里都输入 `#`，普通态保留 `M-3` 的数字前缀。
- 全局设置：备份、recentf、which-key、缩进、长行不换行、保存时自动建目录。
- evil 习惯：`vs`/`sp`、`C-u` 向上翻页、`*` 按整个符号搜索、IME 切换。
- 补全：vertico、corfu 等由 Crafted 打开，这里只放我的改动。候选列表占 frame 高度的 38.2%，`SPC r l` 重复上一次列表，候选列表里 `C-.` 调出 embark 动作、`C-q` 导出、`M-w` 也导出（和 `C-q` 一样，导出结果在 buffer 里可见）；corfu 输入一个字符就立即弹出（Windows 上改为停顿 0.2 秒、输入第二个字符后才查询，避免打字卡顿），eshell 等不自动弹出；cape 的补全来源加到全局（Crafted 只加到了 `*scratch*`），dabbrev 只扫描当前 buffer。
- Snippet：`SPC i s` 用 yasnippet 列出当前模式的 snippet（`SPC i S` 新建），首次使用时才加载 yasnippet。
- Org：`org-modules` 保持为空，但显式 require `org-tempo`，所以插入态里 `<s` + TAB 也会展开成 `#+begin_src` 代码块。
- dired、eshell、Org、magit 的设置和小函数。
- 功能键 F2–F12，和 `SPC` 下我自己的几条命令。

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
10. 加载 `emacs/spacemacs-keys.el`，建立 `xl-leader-map` 和 SPC 键树。
11. 加载 `emacs/xueliang.el`：先读可选的 `local.el`，再依次设置外观、全局选项、evil、vertico、corfu、dired、eshell、Org、magit、函数、功能键，最后绑定 `SPC b s`、`SPC c c`、`SPC f p`、`SPC g g`。
12. `emacs/init.el` 把 `xl-packages` 补回安装名单，并挂到切换主题的钩子上。
13. 启动完成，显示 Crafted 首页（底部显示启动耗时，`SPC b h` 随时可以回到首页）。

Org、dired、magit 等包的设置放在 `with-eval-after-load` 里，包被第一次用到时才执行，所以它们不拖慢启动。

## 为什么这样排

- **装包在加载任何模块和个人配置之前**：它们一加载就会用到这些包。
- **Crafted 模块先于 `spacemacs-keys.el`**：键位需要 evil 已经加载。
- **`spacemacs-keys.el` 先于 `xueliang.el`**：`xl-leader-map` 由前者建立，后者往里加个人绑定；同名键以后加载的为准。
- **`evil-collection-key-blacklist` 在加载 Crafted 模块之前设置**：这样 evil-collection 一开始就不会抢走 SPC 和 `,`。

## 常见修改

| 想做的事 | 改哪里 |
| --- | --- |
| 增加或删除一个包 | `init.el` 里的 `xl-packages`（Crafted 模块已提供的包不用再列） |
| 调整补全（vertico、corfu 等） | `xueliang.el` 的补全段落；整套补全栈的开关在 `init.el` 的 `crafted-completion` |
| 改一个个人键位或命令 | `xueliang.el` |
| 改 Spacemacs 风格的默认键位 | `spacemacs-keys.el` |
| 增加或去掉一个 Crafted 模块 | `init.el` 里的 `require` |
| 升级 Crafted | 改 `frameworks.lock` 的提交号，再执行 `just emacs-crafted-install` |
| 改启动期设置（GC、JIT、包目录） | `early-init.el` |

修改后用下面这几条命令检查：

```bash
just emacs-smoke   # 完整加载一遍，只证明没有初始化错误
just emacs-test    # ERT 测试：键位、钩子、变量、文件结构、真实操作
just emacs-check   # 上面两条一起跑
```

### `just emacs-test`

`emacs/tests/smoke-tests.el` 里的 35 个 ERT 测试在配置完整加载后运行，覆盖：

| 范围 | 例子 |
| --- | --- |
| 文件结构 | `xl-packages` 定义在 `init.el`、`xueliang.el` 里没有；`spacemacs-keys.el` 先于个人配置加载；个人文件不出现 `crafted-`、`spacemacs/` 等框架符号 |
| 启动设置 | 包目录按 Emacs 版本分开；菜单栏、工具栏关闭；evil 系列锁定 MELPA |
| 键位 | `SPC`/`M-m` 树、`SPC f f`、`SPC g g`、`SPC b s`、`SPC i s`/`SPC i S`、`SPC *`/`SPC /` 搜索、F2–F12、`SPC t r`、`SPC T n`、macOS UK 键盘 Option-3 输入 `#` |
| 主模式 leader | `,` 和 `SPC m` 在 Org、Emacs Lisp、其它模式中的解析 |
| Org | 变量、TODO 关键字、链接缩写、表头 face；`org-tempo` 的 `<s` + TAB 展开；打开文件后 `org-indent`、`org-superstar`、`evil-org` 和截断长行 |
| Magit | 状态页删掉的分区和保留的分区；`h/j/k/l`；在临时 git 仓库里真的打开一次 status |
| Dired / Eshell | `-alh`、`TAB` 预览、`h/l/RET`；eshell 的插入态键位和 `eshell/e` 等别名 |
| 补全 | vertico/consult/corfu 状态；补全栈的包由 Crafted 选中、已安装，切换主题后也不会被 `package-autoremove` 当作可清理 |
| 首页 | 底部显示两位小数的启动耗时；最近文件最多 9 个 |
| 其它 | 主题、`xl-idle-require`、保存时建目录、按输入顺序选择候选 |

测试不启动图形界面，全部在 `emacs --batch` 里完成。

## 在 macOS 上安装

在终端里一条命令完成：

```bash
just emacs-mac-setup
```

它依次做四件事：复制 vim/nvim/tmux 配置和 `~/bin` 脚本；按 `frameworks.lock` 克隆 Crafted 并生成 `~/.emacs-profiles/crafted` 存根；完整加载配置并运行 ERT 测试（第一次会顺便装好所有包）；最后把 `~/.emacs.d` 指向 Crafted。最后一步要求 Emacs 已经退出；如果它正在运行，前面的步骤仍会完成，退出 Emacs 后再执行 `just emacs-make-default` 即可。

之后用 `just emacs-try` 启动 Emacs，或者直接打开 Dock 里的 Emacs。

## 在 Windows 上安装

在 Git Bash 里一条命令完成：

```bash
just emacs-windows-setup
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
