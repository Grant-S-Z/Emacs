;;; init-org.el --- for org
;;; Commentary:
;;; Code:
;;; org LaTeX 模板
;; article and report
(setq org-latex-classes
      '(("article" "\\documentclass{article}"
         ("\\section{%s}" . "\\section*{%s}")
         ("\\subsection{%s}" . "\\subsection*{%s}")
         ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
	 ("\\usepackage{fontspec}")
	 ("\\setmainfont{Times New Roman}")
	 ("\\setmonofont{Inconsolata}"))
        ("report" "\\documentclass{report}"
         ("\\chapter{%s}" . "\\chapter*{%s}")
         ("\\section{%s}" . "\\section*{%s}")
         ("\\subsection{%s}" . "\\subsection*{%s}")
         ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))))

;; ctexart
(add-to-list 'org-latex-classes '("ctexart" "
\\documentclass[UTF8, 11pt]{ctexart}

\% fonts
\\usepackage{fontspec}
\\setmainfont{Times New Roman}
\\setmonofont{Inconsolata}
\\setCJKmainfont{宋体-简}

\\usepackage{amssymb}
\\usepackage{amsfonts}
\\usepackage{amsthm}
\\usepackage{bm}
\\usepackage{siunitx}
\\usepackage{xcolor}

\\usepackage{fancyvrb}
\\RecustomVerbatimEnvironment{verbatim}{Verbatim}{}

\% 设置全局的 Verbatim 样式
\\fvset{
  frame=lines,
  fontsize=\\small,
  numbers=left,
  comment=\\color{grey}, % 注释颜色
  keyword=\\color{red}, % 关键字颜色
  % 设置背景色
  fillcolor=\\color{gray}
}
"

("\\section{%s}" . "\\section*{%s}")
("\\subsection{%s}" . "\\subsection*{%s}")
("\\subsubsection{%s}" . "\\subsubsection*{%s}")
("\\paragraph{%s}" . "\\paragraph*{%s}")
("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

;; beamer
(add-to-list 'org-latex-classes '("beamer" "
\\documentclass[10pt]{beamer}

\% fonts
\\usepackage{ctex}
\\usepackage{fontspec}
\\setmainfont{Times New Roman}
\\setmonofont{Inconsolata}
\\setsansfont{Times New Roman}
\\setCJKmainfont{宋体-简}
\\setCJKsansfont{宋体-简}

\\usepackage{amsfonts}
\\usepackage{amsthm}
\\usepackage{bm}
\\usepackage{siunitx}
\\usepackage{xcolor}
"

("\\section{%s}" . "\\section*{%s}")
("\\subsection{%s}" . "\\subsection*{%s}")
("\\subsubsection{%s}" . "\\subsubsection*{%s}")
("\\paragraph{%s}" . "\\paragraph*{%s}")
("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

;;; org
(use-package org
  :defer nil
  :config
  ;;; 设置光标颜色
  (set-cursor-color "#8A2BE2");; BlueViolet

  ;;; 待办事项关键词
  (setq org-todo-keyword-faces '(("TODO" . "red") ("WAITING" . "orange") "|" ("DONE" . "blue") ("CANCELED" . "black")))

  ;;; 日程文件位置
  (setq org-agenda-files '("~/org/task.org" "~/org/event.org" "~/org/plan.org"))

  ;; 任务 capture-templetes
  (setq org-capture-templates nil)

  (add-to-list 'org-capture-templates '("t" "Tasks")) ;; 任务模版
  (add-to-list 'org-capture-templates
	       '("tr" "Reading Task" entry
		 (file+headline "~/org/task.org" "Reading")
		 "* TODO %^{Contentsname}\n%u\n"))
  (add-to-list 'org-capture-templates
	       '("tw" "Work Task" entry
		 (file+headline "~/org/task.org" "Work")
		 "* TODO %^{Workname}\n%u\n"))
  (add-to-list 'org-capture-templates
	       '("th" "Homework Task" entry
		 (file+headline "~/org/task.org" "Homework")
		 "* TODO %^{Homeworkname}\n%u\n")) ;; 没必要放文件位置，实际很难对应
  (add-to-list 'org-capture-templates
	       '("tl" "Long Task" entry
		 (file+headline "~/org/task.org" "Long Task")
		 "* TODO %^{Longtaskname}\n%u\n"))
  (add-to-list 'org-capture-templates
	       '("tq" "Questions" entry
		 (file+headline "~/org/task.org" "Questions")
		 "* TODO %^{Questionname}\n%u\n"))

  (add-to-list 'org-capture-templates ;; 日志模版
               '("j" "Journal" entry (file "~/org/journal.org")
		 "* %U - %^{heading}\n  %?"))

  (add-to-list 'org-capture-templates ;; 事例模版
               '("e" "Event" entry (file "~/org/event.org")
		 "* %U - %^{heading}\n  %?"))

  (add-to-list 'org-capture-templates ;; 备忘录模版
               '("i" "Inbox" entry (file "~/org/inbox.org")
		 "* %U - %^{heading} %^g\n %?\n"))

  (add-to-list 'org-capture-templates ;; 计划模板
	       '("p" "Plan tomorrow" entry (file "~/org/plan.org")
		 "* TODO %U - %^{heading} %^g\n %?\n"))

  (add-to-list 'org-capture-templates ;; 密码模板
             '("k" "Passwords" entry (file "~/passwords.org")
               "* %U - %^{title} %^G\n\n  - 用户名: %^{用户名}\n  - 密码: %(get-or-create-password)"
               :empty-lines 1 :kill-buffer t))

  ;; 设置内联图片显示
  (auto-image-file-mode t)
  (setq org-image-actual-width 300)

  ;; 导出 pdf 用 emacs 查看
  (setq org-file-apps '("\\.pdf\\'" . emacs))

  ;; babel 配置
  (setq org-confirm-babel-evaluate nil)
  (setq org-plantuml-jar-path "~/.emacs.d/repos/plantuml-1.2023.10.jar")

  (require 'ob-C)
  (require 'ob-latex)
  (require 'ob-shell)
  (require 'ob-org)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((python . t)
     (R . t)
     (emacs-lisp . t)
     (plantuml . t)
     (C . t)
     (shell . t)
     (org . t)
     (latex . t)))

  ;;; python
  (setq python-shell-completion-native-enable t)
  (setq python-shell-interpreter "python3")
  (setq org-babel-python-command "python3")

  ;;; latex
  (setq org-latex-default-class "ctexart") ;; 默认 latex class
  (setq org-latex-compiler "xelatex") ;; 默认 latex compiler

  (add-hook 'org-mode-hook (lambda () ;; cdlatex
			   (setq truncate-lines nil)
			   (org-cdlatex-mode)))

  :custom
  (org-pretty-entities t) ;; pretty entities in org, which is not nice for writing latex
  (org-startup-indented nil) ;; 大纲缩进，开启后代码块不美观
  (org-highlight-latex-and-related '(latex entities)) ;; latex 高亮设置

  ;;; Agenda styling
  (org-agenda-use-time-grid t)
  (org-agenda-include-diary t)

  (org-agenda-tags-column 0)
  (org-agenda-block-separator ?─)
  (org-agenda-current-time-string
  "⭠ now ─────────────────────────────────────────────────")
  ;;---------------------------------------------
  ;;org-agenda-time-grid
  ;;--------------------------------------------
  (org-agenda-time-grid (quote ((daily today require-timed)
                                      (300
                                       600
                                       900
                                       1200
                                       1500
                                       1800
                                       2100
                                       2400)
                                      "......"
                                      "-----------------------------------------------------"
                                      )))
  )

;; org-appear, 方便编辑 latex 公式等
(use-package org-appear
  :after org
  :hook (org-mode . org-appear-mode)
  :custom
  (org-appear-autoemphasis t)
  (org-appear-autolinks t)
  (org-appear-autoentities t)
  (org-appear-autosubmarkers t) ;; 下标
  (org-appear-inside-latex t) ;; latex 符号
  (org-appear-autokeywords t))

;;; 主题
(use-package org-modern
  :after org
  :hook (org-mode . org-modern-mode)
  :custom
  (org-modern-todo nil) ;; 不太好看
  (org-modern-table nil) ;; org-modern 表格十分难用
  (org-modern-timestamp nil) ;; 和 TODO 一样不好看
  (org-modern-tag nil) ;; 不想看见 org-modern 的 archive
  (org-modern-priority nil)
  :config
  ;; Add frame borders and window dividers
  (modify-all-frames-parameters
   '((right-divider-width . 5)
     (internal-border-width . 5)))
  (dolist (face '(window-divider
                  window-divider-first-pixel
                  window-divider-last-pixel))
    (face-spec-reset-face face)
    (set-face-foreground face (face-attribute 'default :background)))
  (set-face-background 'fringe (face-attribute 'default :background))
  (setq
   ;; Edit settings
   org-auto-align-tags t
   org-tags-column 0
   org-fold-catch-invisible-edits 'show-and-error
   org-special-ctrl-a/e t
   org-insert-heading-respect-content t
   ;; Org styling, hide markup etc.
   org-hide-emphasis-markers t
   org-ellipsis "…"
))

;;; 图片
;; 复制
(use-package org-download
  :after org
  :config
  (add-hook 'dired-mode-hook 'org-download-enable)
  (setq-default org-download-image-dir "./img"))

;; graphviz
(use-package graphviz-dot-mode)

;;; 表格
(use-package valign
  :after org
  :hook (org-mode . valign-mode)
  :custom
  (valign-fancy-bar nil)) ;; 确保性能

;;; 笔记配置
;; Zotero 位置
(setq zot_bib '("~/zotero/bibex/我的文库.bib") ;; Zotero 用 Better BibTeX 导出的 BibTeX 文件
      zot_pdf "~/library/CloudStorage/坚果云-zhuyt20@mails.tsinghua.edu.cn/zotero" ;; Zotero 的 ZotFile 同步文件夹
      org_refs "~/library/CloudStorage/坚果云-zhuyt20@mails.tsinghua.edu.cn/org/roam-notes/ref" ;; org-roam 文献笔记目录
      )

;; org-roam
(use-package org-roam
  :after org
  :defer nil
  :custom
  (org-roam-directory "~/org/roam-notes/") ;; 默认笔记目录
  (org-roam-db-gc-threshole most-positive-fixnum) ;; 提高性能
  :bind
  (("C-c n f" . org-roam-node-find)
   ("C-c n i" . org-roam-node-insert)
   ("C-c n c" . org-roam-capture)
   ("C-c n l" . org-roam-buffer-toggle) ;; 显示后链窗口
   ("C-c n u" . org-roam-ui-mode)) ;; 浏览器中可视化
  :config
  (org-roam-db-autosync-mode) ;; 启动时自动同步数据库
  ;; one module to combine org-roam and org-noter
  (setq my/ref-template
	(concat "#+FILETAGS: reading research\n"
		"- tags :: %^{keywords}\n"
		"* %^{title}\n"
		":PROPERTIES:\n"
		":Custom_ID: %^{citekey}\n"
		":URL: %^{url}\n"
		":AUTHOR: %^{author-or-editor}\n"
		":NOTER_DOCUMENT: ~/library/CloudStorage/坚果云-zhuyt20@mails.tsinghua.edu.cn/zotero/%^{citekey}.pdf\n"
		":END:"))
  (add-to-list 'org-roam-capture-templates
	       `("r" "Zotero 文献模版" plain
		 ,my/ref-template
		 :target
		 (file+head "~/library/CloudStorage/坚果云-zhuyt20@mails.tsinghua.edu.cn/org/roam-notes/ref/${citekey}.org" "#+title: ${title}\n"))))

;; 用 helm-bibtex 读取 Zotero 信息
(use-package helm-bibtex
  :after org-roam
  :defer nil
  :custom
  (bibtex-completion-notes-path org_refs)
  (bibtex-completion-bibliography zot_bib)
  (bibtex-completion-library-path zot_pdf))

;; org-roam-ui
(use-package org-roam-ui
  :after org-roam
  :custom
  (org-roam-ui-sync-theme t) ;; 同步 Emacs 主题
  (org-roam-ui-follow t) ;; 笔记节点跟随
  (org-roam-ui-update-on-save t))

;; org-roam-bibtex 绑定 helm-bibtex
(use-package org-roam-bibtex
  :after org-roam
  :defer nil
  :hook (org-roam-mode . org-roam-bibtex-mode)
  :bind (("C-c n k" . orb-insert-link)
	 ("C-c n a" . orb-note-action))
  :custom
  (orb-insert-interface 'helm-bibtex)
  (orb-insert-link-description 'citekey)
  (orb-preformat-keywords
   '("citekey" "title" "url" "author-or-editor" "keywords" "file"))
  (orb-process-file-keyword t)
  (orb-attached-file-extensions '("pdf")))

;; org-ref 引用设置
(use-package org-ref
  :defer nil
  :after org-roam
  :bind (("C-c (" . org-ref-insert-link)))

;; org-noter
(use-package org-noter
  :defer t
  :bind (("C-c n n" . org-noter))
  :custom
  (org-noter-always-create-frame nil) ;; Please stop opening frames, which will change your fonts and make it hard to choose the window you want.
  (org-noter-highlight-selected-text t)
  (org-noter-max-short-selected-text-length 15) ;; tab 高亮最小字符长度，大于该长度变为 quote
  (org-noter-auto-save-last-location t) ;; 自动保存上次位置
  (org-noter-notes-search-path '("~/org/roam-notes/"))
  :config
  (define-key pdf-view-mode-map
	      "d" 'pdf-view-next-page-command) ;; 向后翻页
  (define-key pdf-view-mode-map
	      "a" 'pdf-view-previous-page-command) ;; 向前翻页
  (define-key pdf-view-mode-map
	      "s" 'pdf-view-scroll-up-or-next-page) ;; 向下滑动
  (define-key pdf-view-mode-map
	      "w" 'pdf-view-scroll-down-or-previous-page) ;; 向上滑动
)
(use-package djvu)
(use-package nov)

(use-package olivetti
  :after org
  :defer nil
  :config
  (olivetti-set-width 80))

(provide 'init-org)
;;; init-org.el ends here
