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

- 启动时关掉 GC，启动完成后恢复到 64 MB。
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
- 从 `xueliang.el` 读出 `xl-packages`，加入需要安装的包列表，再装上 Spacemacs 习惯需要的几个包。
- 加载 Crafted 的 defaults、evil、org、startup 四个模块。
- 修正 Crafted 默认值中与我的习惯冲突的部分：插入态 `C-h` 保持为帮助前缀，关闭全局拼写检查。
- 加载 `spacemacs-keys.el`，然后加载 `xueliang.el`。

### `spacemacs-keys.el`（适配层）

复刻 Spacemacs 的键位习惯，Crafted 本身没有这些：

- SPC 键树（`b f g j p q r s t w` 等分组）和 `M-m`。
- `,` 与 `SPC m` 主模式 leader，目前对 Org 和 Emacs Lisp 生效。
- `fd` 退出插入态，evil-surround，窗口编号 `SPC 1`–`9`。
- 首页（`SPC b h`），列出最近文件，按数字打开。
- 创建 `xl-leader-map`，供 `xueliang.el` 继续往里加绑定。

它在 `xueliang.el` 之前加载，所以 `xueliang.el` 里的绑定会覆盖这里的默认值（例如 `SPC SPC`、`SPC x`）。

### `xueliang.el`（个人层）

我自己的全部设置，按段落组织：

- 文件开头的 `xl-packages` 是第一个表单，`init.el` 只读这一段，用来决定装哪些包。
- 平台、路径、字体、主题；可选的 `~/.emacs-profiles/local.el`（只放机器相关的路径，不进仓库）。
- 全局设置：备份、recentf、which-key、缩进、保存时自动建目录。
- evil 习惯：`vs`/`sp`、`C-u` 向上翻页、`*` 按整个符号搜索、IME 切换。
- ivy/counsel：候选窗口占 frame 高度的 38.2%，prescient 排序。
- company、dired、eshell、Org、magit 的设置和小函数。
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
7. 加载 `crafted-evil-packages` 和 `crafted-org-packages`，读取 `xueliang.el` 的 `xl-packages`，调用 `package-install-selected-packages` 装包。
8. 加载 `crafted-defaults-config`、`crafted-evil-config`、`crafted-org-config`、`crafted-startup-config`。
9. 修正 `evil-want-C-h-delete` 和 ispell 钩子。
10. 加载 `emacs/spacemacs-keys.el`，建立 `xl-leader-map` 和 SPC 键树。
11. 加载 `emacs/xueliang.el`：先读可选的 `local.el`，再依次设置外观、全局选项、evil、ivy、company、dired、eshell、Org、magit、函数、功能键，最后绑定 `SPC b s`、`SPC c c`、`SPC f p`、`SPC g g`。
12. 启动完成，显示 Crafted 首页（`SPC b h` 随时可以回到首页）。

Org、dired、magit 等包的设置放在 `with-eval-after-load` 里，包被第一次用到时才执行，所以它们不拖慢启动。

## 为什么这样排

- **`xl-packages` 必须是 `xueliang.el` 的第一个表单**：`init.el` 只读第一个表单，这样装包时不需要加载整个个人配置。
- **Crafted 模块先于 `spacemacs-keys.el`**：键位需要 evil 已经加载。
- **`spacemacs-keys.el` 先于 `xueliang.el`**：`xl-leader-map` 由前者建立，后者往里加个人绑定；同名键以后加载的为准。
- **`evil-collection-key-blacklist` 在加载 Crafted 模块之前设置**：这样 evil-collection 一开始就不会抢走 SPC 和 `,`。

## 常见修改

| 想做的事 | 改哪里 |
| --- | --- |
| 增加或删除一个包 | `xueliang.el` 开头的 `xl-packages` |
| 改一个个人键位或命令 | `xueliang.el` |
| 改 Spacemacs 风格的默认键位 | `spacemacs-keys.el` |
| 增加或去掉一个 Crafted 模块 | `init.el` 里的 `require` |
| 升级 Crafted | 改 `frameworks.lock` 的提交号，再执行 `just emacs-crafted-install` |
| 改启动期设置（GC、JIT、包目录） | `early-init.el` |

修改后用下面这条命令检查：

```bash
just emacs-smoke
```

它以批处理方式完整加载配置，任何初始化错误都会返回非零状态。
