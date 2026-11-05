# Grant's Emacs

A tidy and small Emacs settings of Grant.

> I won't use Emacs to code anymore! (except tex, you know)

## 配置结构

`init.el` 按模块加载,下面按同样的顺序介绍日常使用频率较高的 package,不是完整依赖清单。

1. `init-const` / `init-startup` / `init-elpa`:系统检测、启动默认值、ELPA 与 `use-package`。
2. `init-ui`:界面、字体、输入法、补全和视觉增强。
3. `init-package`:搜索、Git、窗口和日常编辑工具。
4. `init-ai` / `init-mail` / `init-shell`:AI、邮件和终端。
5. `init-lsp` / `init-tex`:语言模式、LSP 和 LaTeX。
6. `init-org*`:Org 核心、预览、UI、Org-roam 和发布。
7. `init-fun` / `init-kbd`:辅助命令和快捷键。

包默认由 `use-package :ensure` 从 ELPA/MELPA 安装;`org-fc`、`maimemo` 等是 `site-lisp` 下的本地包。

## 界面与输入(`init-ui`)

- **dashboard**:启动页和常用入口。
- **doom-modeline**:简洁的 modeline,显示 Git、时间、LSP 等信息。
- **vertico**:minibuffer 垂直补全,`M-x`、`C-x b` 等命令的统一候选列表。
- **orderless**:把补全候选按空格分词匹配,不必按顺序输入。
- **marginalia**:在 minibuffer 候选旁显示命令、变量、文件的简要说明。
- **dirvish**:替代 Dired 的文件管理界面,`C-x d` 打开,支持预览和侧边栏。
- **ef-themes**:当前使用 `ef-reverie` 主题。
- **rime**:中文输入法,默认输入法为 `rime`。
- **jinx**:英文拼写检查,`M-$` 修正;WSL 需要 `enchant-2`。
- **pangu-spacing**:自动在中英文之间插入间距。

## 日常操作(`init-package`)

- **which-key**:按键前缀菜单,忘记组合键时非常有用。
- **restart-emacs**:`C-c r` 重启 Emacs。
- **drag-stuff**:`M-p` / `M-n` 上下移动当前行或选区。
- **embark** / **embark-consult**:对 minibuffer 候选项或当前对象执行上下文操作,`C-.` 打开 Embark。
- **consult**:增强版搜索和跳转。当前绑定:
  - `C-s` `consult-line`(当前 buffer)
  - `C-x b` `consult-buffer`
  - `C-c f` `consult-ripgrep`(全项目搜索)
- **wgrep**:把 `grep`/`consult-ripgrep` 的结果变成可编辑 buffer。
- **consult-dir**:在 minibuffer 中快速切换目录。
- **crux**:常用编辑命令集合,如复制行、关闭其他 buffer。
- **yasnippet** / **yasnippet-snippets**:代码模板和常用片段。
- **avy**:`C-;` 跳转到屏幕上可见的字符位置。
- **saveplace**:重新打开文件时恢复上次光标位置。
- **magit**:Git 前端,`C-x g` 打开 status。
- **atomic-chrome**:把浏览器中的 Overleaf 编辑内容同步到 Emacs。
- **ace-window**:`M-o` 快速跳转到指定窗口。
- **expand-region**:`M-=` 逐层扩大选区。
- **diff-hl**:在 fringe 中显示 Git diff 标记。
- **ws-butler**:保存时清理行尾空格。
- **helpful**:增强 `describe-function` / `describe-variable`。
- **calibredb** / **nov**:Calibre 书库浏览和 EPUB 阅读。
- **gt**:`C-c g` 中英翻译。

## AI、邮件与终端

- **agent-shell**:通过 ACP 在 Emacs 内使用 Kimi Code 等 coding agent,`C-c k` 启动。
- **agent-shell-math-renderer**:把 agent 回复中的 LaTeX 渲染为 SVG 公式。
- **mu4e**:Emacs 邮件客户端(使用 Debian/系统安装的 mu4e),配合 `mbsync` 同步清华邮箱;支持线程折叠和引用隐藏。
- **vterm** / **vterm-toggle**:Emacs 内终端,`C-c s` 打开/隐藏。
- **exec-path-from-shell**:在 macOS 上从登录 shell 同步 PATH。

## 语言与 LSP(`init-lsp`)

- **format-all**:统一调用各语言的格式化工具,`C-c C-<return>`。
- **cape** / **corfu**:buffer 内补全前端和 CAPF 扩展。
- **eglot**:Emacs 内置 LSP 客户端,Python 使用 pyright,C/C++ 使用 clangd。
- **treesit**:Emacs 内置 Tree-sitter 支持;Emacs 31 由 `treesit-enabled-modes` 自动把经典模式映射到对应的 `*-ts-mode`。
- **python**:Python 解释器、缩进和 `python-ts-mode` 配置。
- **markdown-ts-mode** / **markdown-ts-appear**:Markdown Tree-sitter 语法高亮;31.1+ 支持光标处显示源码。
- **csv-mode**:CSV 文件模式。

## LaTeX(`init-tex`)

- **auctex**:LaTeX 主模式,配合 `latexmk`。
- **auctex-latexmk**:让 AUCTeX 使用 `latexmk` 编译。
- **cdlatex**:数学公式和环境的快速输入。
- **pdf-tools**:PDF 查看、搜索和同步;导出 PDF 默认在 Emacs 内打开。

## Org(`init-org*`)

- **org**:核心编辑、agenda、capture、Babel;支持 Python、C、shell、LaTeX、PlantUML 等代码块。
- **ob-mermaid**:Org Babel 的 Mermaid 图支持。
- **org-latex-preview**:Org main 自带的异步 LaTeX 预览,`C-/` 预览公式。
- **org-modern**:现代 Org 外观:标题、TODO、标签、列表符号等。
- **org-appear**:光标离开时隐藏 emphasis 标记,移入时显示源码。
- **valign**:按列宽对齐 Org 表格。
- **org-inline-pdf**:在 Org 中内嵌 PDF 预览。
- **org-roam**:双链笔记,`C-c n f` 查找、`C-c n i` 插入链接、`C-c n c` capture。
- **citar** / **citar-org-roam**:BibTeX 文献选择、引用和文献笔记。
- **org-fc**:间隔重复记忆卡片,`C-c v f` 开始复习。
- **easy-hugo** / **ox-hugo**:Hugo 博客管理和 Org 导出。

## 辅助命令与快捷键

`init-fun` 和 `init-kbd` 主要是自定义函数和零散键位,不引入额外 package,例如打开 journal、博客目录、切换全屏、段落跳转等。
