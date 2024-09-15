;;; init-org.el --- for org
;;; Commentary:
;;; Code:

;;; org
(use-package org
  :defer nil
  :bind ("C-x C-y" . org-insert-image)
  :config
  ;;; 自动折叠
  (setq org-startup-folded 'content) ;; 只显示标题

  ;; 设置内联图片显示
  (auto-image-file-mode t)
  (setq org-image-actual-width 400)

  ;; babel 配置
  (setq org-confirm-babel-evaluate nil)
  (setq org-plantuml-jar-path "~/Code/plantuml/plantuml-1.2024.3.jar")

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

  ;;; latex
  (setq org-latex-default-class "ctexart") ;; 默认 latex class
  (setq org-latex-compiler "xelatex") ;; 默认 latex compiler
  (turn-on-cdlatex)
  (add-hook 'org-mode-hook (lambda () ;; cdlatex
			   (setq truncate-lines nil)
			   (org-cdlatex-mode)))

  ;;; pdf view
  (setq org-file-apps
	(quote
	 ((auto-mode .emacs)
	  ("\\.pdf\\'" . "/Applications/Skim.app/Contents/MacOS/Skim %s"))))

  :custom
  (org-pretty-entities t) ;; pretty entities in org
  (org-startup-indented t) ;; 缩进
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
                                      "-----------------------------------------------------"))))

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
;; org-modern
(use-package org-modern
  :after org
  :hook (org-mode . org-modern-mode)
  :custom
  (org-modern-hide-stars nil)
  (org-modern-todo nil)
  (org-modern-table nil)
  (org-modern-timestamp nil)
  (org-modern-tag nil)
  (org-modern-priority nil)
  (org-modern-star 'replace)
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
   org-ellipsis "…"))

;; Fit org modern indent
(use-package org-modern-indent
  :load-path "~/.emacs.d/site-lisp/org-modern-indent"
  :after org
  :config
  (add-hook 'org-mode-hook #'org-modern-indent-mode 90))

;;; 表格
(use-package valign
  :after org
  :hook (org-mode . valign-mode)
  :custom
  (valign-fancy-bar nil)) ;; 确保性能

;;; 笔记配置
;; Zotero 位置
(setq zot_bib '("~/org/roam-notes/reference.bib") ;; Zotero 用 Better BibTeX 导出的 BibTeX 文件
      zot_pdf "~/Nutstore Files/zotero" ;; Zotero 的 ZotFile 同步文件夹
      org_refs "~/org/roam-notes/ref" ;; org-roam 文献笔记目录
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
		":NOTER_DOCUMENT: ~/Nutstore Files/zotero/%^{citekey}.pdf\n"
		":END:"))
  (add-to-list 'org-roam-capture-templates
	       `("r" "Zotero 文献模版" plain
		 ,my/ref-template
		 :target
		 (file+head "~/org/roam-notes/ref/${citekey}.org" "#+title: ${title}\n"))))

;; 用 helm-bibtex 读取 Zotero 信息
(use-package helm-bibtex
  :after org-roam
  :defer t
  :custom
  (bibtex-completion-notes-path org_refs)
  (bibtex-completion-bibliography zot_bib)
  (bibtex-completion-library-path zot_pdf))

;; org-roam-ui
(use-package org-roam-ui
  :after org-roam
  :defer t
  :custom
  (org-roam-ui-sync-theme t) ;; 同步 Emacs 主题
  (org-roam-ui-follow t) ;; 笔记节点跟随
  (org-roam-ui-update-on-save t))

;; org-roam-bibtex 绑定 helm-bibtex
(use-package org-roam-bibtex
  :after org-roam
  :defer t
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
  :defer t
  :after org-roam
  :bind (("C-c (" . org-ref-insert-link)))

;; org-noter
(use-package org-noter
  :defer t
  :bind (("C-c n n" . org-noter))
  :custom
  (org-noter-always-create-frame nil) ;; Please stop opening frames, which will change your fonts and make it hard to choose the window you want.
  (org-noter-highlight-selected-text t)
  (org-noter-max-short-selected-text-length 50) ;; tab 高亮最小字符长度，大于该长度变为 quote
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

;;; org-reveal
(use-package ox-reveal
  :config
  (setq org-reveal-root "file:///Users/grant/Code/js/reveal.js-5.0.5"))

;;; org-remark
(use-package org-remark
  :bind (("C-c n m" . org-remark-mark)
	 ("C-c n ]" . org-remark-view-next)
	 ("C-c n [" . org-remark-view-prev))
  :hook (nov-mode . org-remark-nov-mode))

(provide 'init-org)
;;; init-org.el ends here
