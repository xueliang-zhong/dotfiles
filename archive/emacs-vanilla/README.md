# Vanilla init.el 测试

用一份独立的 `init.el` 复刻 `emacs/xueliang.el` 和 `emacs/spacemacs-keys.el`
的习惯，不加载 Crafted Emacs。用来回答一个问题：

> 不用 Crafted，只靠一份 init.el，能不能把这些习惯跑起来？

结论：**可以**。键位、evil、补全、Org、Magit、dired、eshell、首页和 F2–F12
都已复刻；仍然需要第三方包（evil、vertico、corfu、magit 等）， 但不再需要
Crafted 的模块和存根。

## 目录隔离

测试配置放在单独的 profile 里，和正在运行的 Crafted 配置完全分开：

```
~/.emacs-profiles/vanilla/init.el  ->  dotfiles/emacs/vanilla/init.el
~/.emacs-profiles/vanilla/elpa/       首次启动安装的包
~/.emacs-profiles/vanilla/history     保存的历史
~/.emacs-profiles/vanilla/recentf.eld 最近文件
~/.emacs-profiles/vanilla/backups/    备份
```

`--init-directory` 会在读 init 之前改变 `user-emacs-directory`，所以
`elpa/`、`custom.el`、历史、最近文件、备份、`eln-cache` 都写在这个目录里，
不会碰 `~/.emacs.d` 或当前正在运行的 Emacs。

## 启动

```bash
# 已经建好软链接，直接启动：
just emacs-try vanilla

# 等价于：
emacs --init-directory ~/.emacs-profiles/vanilla
```

如果换机器，先建软链接：

```bash
mkdir -p ~/.emacs-profiles/vanilla
ln -sfn ~/workspace/dotfiles/emacs/vanilla/init.el \
        ~/.emacs-profiles/vanilla/init.el
```

第一次启动会从 MELPA 安装 `xl-packages` 里的包，需要网络，可能要一两分钟。
之后再启动不会重复安装。

## 验证

批量加载（不启 GUI，任何 init 错误都会让命令失败）：

```bash
emacs --batch --init-directory ~/.emacs-profiles/vanilla \
  -l ~/.emacs-profiles/vanilla/init.el \
  -l ~/workspace/dotfiles/emacs/vanilla/check.el \
  -f ert-run-tests-batch-and-exit
```

`check.el` 里的 29 个 ERT 测试覆盖：

- profile 隔离、包已安装、主题、evil / vertico / corfu / which-key 已打开
- `SPC`/`M-m` 键树、`SPC f f`、`SPC g g`、`SPC b h`、`SPC i s`、窗口键
- `,` / `SPC m` 主模式 leader（Org、Emacs Lisp、无 leader 的模式）
- macOS UK 键盘 Option-3 输入 `#`
- Org：变量、`org-indent`、`org-superstar`、`evil-org`、`<s` + TAB 展开
- Magit：状态页分区裁剪、`h/j/k/l`、真实 git 仓库里打开 status
- Dired：`h/l/RET`；Eshell：插入态 RET、C-a
- 首页：最近文件数字键、启动时间
- diff-hl、evil-escape、evil-surround、winum

另外在 macOS 图形界面里实际启动过，确认：

```
display=t fullscreen=maximized theme=spacemacs-dark evil=t
font="JetBrains Mono" home=t corfu=t vertico=t
```

并用 `F9` 实际打开/关闭过 dired sidebar，用 `F5` 打开过 eshell popup。

## 和 Crafted 版的差异

这些是有意为之，或者没有复刻：

- **不再是零包**：evil、evil-collection、evil-escape、evil-surround、
  evil-org、evil-nerd-commenter、winum、avy、yasnippet、magit、
  org-superstar、golden-ratio、主题、diff-hl、vertico/consult/marginalia/
  orderless/embark、corfu/cape 仍然从 MELPA 安装。Crafted 只是把这些包的
  默认配置打包好了，去掉 Crafted 不等于去掉这些包。
- **首页**：没有 Crafted 的 logo 和 `crafted-startup`，用 `*Home*` 实现了
  最近文件 1–9、打开文件/家目录/dotfiles、启动耗时；`SPC b h` 和 F10 能用。
- **补全默认值**：Crafted 的 completion 模块做了 vertico-cycle、C-s
  consult-line、minibuffer C-r、corfu-popupinfo、cape 的 pcomplete 静默等；
  这里逐条补上了。Crafted 里 Consult 3.4 已删除的
  `consult-preview-at-point-mode` 钩子没有再加。
- **Crafted defaults 里没有搬过来的**：`font-use-system-font`（这里显式用
  JetBrains Mono）、C-c w windmove 前缀（这里用 evil 的窗口键）、
  dictionary、Man/Ediff 的显示设置、flyspell（现有配置本来就去掉了）。
- **eglot / tree-sitter**：没有自动加 `eglot-ensure` 或
  `major-mode-remap-alist`，因为 `xueliang.el` 目前没有这些习惯。需要的话
  可以按示例里的几行加上。
- **`org-capture`**：没有定义 capture template（现有配置也没有；如果
  `local.el` 或 YI 文件提供模板则不受影响）。
- **`package-check-signature nil`**：只为这个一次性测试 profile 避免 GPG
  导入提示。若打算长期使用，删掉这一行。
- **没有 early-init.el**：菜单栏/工具栏和 GC 设置在 init 里，GUI 启动时
  可能有一瞬间的闪烁；长期使用建议把这两段挪到 `early-init.el`。

## 删除测试

```bash
rm -rf ~/.emacs-profiles/vanilla
```

只删除测试 profile 的软链接、包和运行状态；仓库里的
`emacs/vanilla/init.el` 和 `check.el` 不受影响，当前 Crafted 配置也不会被改动。
