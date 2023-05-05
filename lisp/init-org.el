;;; init-org.el --- for org
;;; Commentary:
;;; Code:

;; 待办事项关键词
(setq org-todo-keyword-faces '(("TODO" . "red") ("SHELVED" . "orange") ("DONE" . "blue") ("CANCELED" . "green")))

;; 日程文件位置
(setq org-agenda-files (list "~/org/task.org"))

;; 任务 capture-templetes
(setq org-capture-templates nil)

(add-to-list 'org-capture-templates '("t" "Tasks")) ;; 任务模版集
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
	       "* TODO %^{Homeworkname}\n%u\n[[%F]]\n"))
(add-to-list 'org-capture-templates
	     '("tl" "Long Task" entry
	       (file+headline "~/org/task.org" "Long Task")
	       "* TODO %^{Longtaskname}\n%u\n"))
(add-to-list 'org-capture-templates
	     '("tq" "Questions" entry
	       (file+headline "~/org/task.org" "Questions")
	       "* TODO %^{Questionname}\n%u\n"))
(add-to-list 'org-capture-templates
	     '("tt" "Thesis Doubts" entry
	       (file+headline "~/org/task.org" "Thesis Doubts")
	       "* TODO %^{Doubtsname}\n%u\n"))

(add-to-list 'org-capture-templates ;; 日志模版
             '("j" "Journal" entry (file "~/org/journal.org")
               "* %U - %^{heading}\n  %?"))

(add-to-list 'org-capture-templates ;; 备忘录模版
             '("i" "Inbox" entry (file "~/org/inbox.org")
               "* %U - %^{heading} %^g\n %?\n"))

;; latex
(setq org-latex-classes
      '(("article" "\\documentclass{article}"
         ("\\section{%s}" . "\\section*{%s}")
         ("\\subsection{%s}" . "\\subsection*{%s}")
         ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))
        ("report" "\\documentclass{report}"
         ("\\chapter{%s}" . "\\chapter*{%s}")
         ("\\section{%s}" . "\\section*{%s}")
         ("\\subsection{%s}" . "\\subsection*{%s}")
         ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))))

(add-to-list 'org-latex-classes '("ctexart" "
\\documentclass[UTF8, 11pt]{ctexart}

\% fonts
\\usepackage{fontspec}
\\setmainfont{Times New Roman}
\\setmonofont{Inconsolata}
\\setCJKmainfont{宋体-简}

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

(add-to-list 'org-latex-classes '("beamer" "
\\documentclass[presentation]{beamer}

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

(setq org-latex-default-class "ctexart")
(setq org-latex-compiler "xelatex")
;; (eval-after-load 'org
;;   '(progn
;;      (add-to-list 'org-file-apps '("\\.pdf\\'" . (lambda (file link) (pdf-tools-install) (find-file file))))))


;; 主题
(use-package org-superstar
  :after org
  :hook (org-mode . org-superstar-mode))

(use-package org-modern
  :after org
  :hook
  (org-mode . org-modern-mode)
  (org-agenda-finalize-hook . org-modern-agenda)

  :custom
  (org-modern-todo nil)
  (org-modern-table nil)

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
   org-auto-align-tags nil
   org-tags-column 0
   org-catch-invisible-edits 'show-and-error
   org-special-ctrl-a/e t
   org-insert-heading-respect-content t

   ;; Org styling, hide markup etc.
   org-hide-emphasis-markers t
   org-pretty-entities t
   org-ellipsis "…"

   ;; Agenda styling
   org-agenda-tags-column 0
   org-agenda-block-separator ?─
   org-agenda-time-grid
   '((daily today require-timed)
     (800 1000 1200 1400 1600 1800 2000)
     " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄")
   org-agenda-current-time-string
   "⭠ now ─────────────────────────────────────────────────")
  )

;; ;; pandoc 导出
;; (use-package ox-pandoc
;;   :after org
;;   :config
;;   (setq org-pandoc-options-for-beamer-pdf '((pdf-engine . "xelatex"))
;;        	org-latex-pdf-process '("xelatex -shell-excape -interaction nonstopmode -output-directory %o %f")))

;; ;; org-mode 居中
;; (add-hook 'org-mode #'(lambda ()
;; 		     (setq visual-fill-column-width 100) ;; 宽度
;; 		     (setq visual-fill-column-center-text t) ;; 居中
;; 		     (setq adaptive-fill-mode t)
;; 		     (visual-fill-column-mode 1)))

;; (use-package nov)
;; (use-package djvu)

;; ;; 公式预览
;; (use-package org-fragtog)

;; 图片
(use-package org-download
  :after org
  :config
  (add-hook 'dired-mode-hook 'org-download-enable)
  (setq-default org-download-image-dir "./img")
  (setq org-download-image-org-width 200))

;; 表格
;; (use-package valign
;;   :after org
;;   :hook (org-mode . valign-mode)
;;   :custom
;;   (valign-fancy-bar t))

;; (use-package ftable)

;; graphviz
(use-package graphviz-dot-mode)

;; else
(use-package org
  :defer nil
  :config
  ;;; 设置光标颜色
  ;; Purple #800080
  ;; BlueViolet #8A2BE2
  ;; DarkViolet #9400D3
  (set-cursor-color "BlueViolet")

  ;; 设置内联图片显示
  (auto-image-file-mode t)
  (setq org-image-actual-width 300)

  ;;; babel 配置
  (setq org-confirm-babel-evaluate nil)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((python . t)
     (R . t)
     (jupyter . t)
     (emacs-lisp . t)
     (ditaa . t)
     (plantuml . t)
     (dot . t)
     (plantuml . t)))

  ;; python
  (setq python-shell-completion-native-enable t)
  (setq python-shell-interpreter "python3")
  (setq org-babel-python-command "python3")

  (add-hook 'org-mode-hook (lambda ()
			   (setq truncate-lines nil)
			   (org-cdlatex-mode);; 启用 cdlatex
			   ))

  :custom
  (org-startup-indented nil) ;; 大纲缩进
  (org-highlight-latex-and-related '(native latex entities)) ;; latex 高亮设置
  (org-pretty-entities t) ;; latex prettify for org
  )

;; 快捷键
(global-set-key (kbd "C-c a") #'org-agenda)
(global-set-key (kbd "C-c c") #'org-capture)

;;; 笔记配置
;; Zotero 位置
(setq zot_bib '("~/zotero/bibex/我的文库.bib") ;; Zotero 用 Better BibTeX 导出的 BibTeX 文件
      zot_pdf "~/library/CloudStorage/坚果云-zhuyt20@mails.tsinghua.edu.cn/zotero" ;; Zotero 的 ZotFile 同步文件夹
      org_refs "~/library/CloudStorage/坚果云-zhuyt20@mails.tsinghua.edu.cn/org/roam-notes/ref" ;; org-roam 文献笔记目录
      )

;; org-roam
(use-package org-roam
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

  ;; :bind-keymap
  ;; ("C-c n d" . org-roam-dailies-map) ;; 日记菜单

  :config
  ;; (require 'org-roam-dailies) ;; 启用日记功能
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
  :custom
  (bibtex-completion-notes-path org_refs)
  (bibtex-completion-bibliography zot_bib)
  (bibtex-completion-library-path zot_pdf))

(use-package org-roam-ui
  :after org-roam
  :custom
  (org-roam-ui-sync-theme t) ;; 同步 Emacs 主题
  (org-roam-ui-follow t) ;; 笔记节点跟随
  (org-roam-ui-update-on-save t))

;; org-roam-bibtex 绑定 helm-bibtex
(use-package org-roam-bibtex
  :after org-roam
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

(use-package org-ref
  :after org-roam)

;; org-noter
(use-package org-noter
  :defer nil
  :bind (("C-c n n" . org-noter))
  :custom
  (org-noter-highlight-selected-text t)
  (org-noter-max-short-selected-text-length 15)
  (org-noter-notes-search-path '("~/org/roam-notes/")))

;; jupyter
(use-package jupyter)

(use-package zmq)

(provide 'init-org)
;;; init-org.el ends here
