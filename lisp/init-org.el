;;; init-org.el --- for org
;;; Commentary:
;;; Code:

;; 待办事项关键词
(setq org-todo-keyword-faces '(("TODO" . "red") ("SHELVED" . "orange") ("DONE" . "blue") ("CANCELED" . "green")))

;; 日程文件位置
(setq org-agenda-files (list "~/org/task.org"))

;; org特性
(add-hook 'org-mode-hook (lambda ()
			   (setq truncate-lines nil)
			   (org-cdlatex-mode);; 启用cdlatex
			   ))

;; 任务capture-templetes
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
	       "* TODO %^{Homeworkname}\n%u\n"))
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

;; 主题
(use-package org-superstar
  :after org
  :hook (org-mode . org-superstar-mode))

;; pandoc导出
(use-package ox-pandoc
  :after org
  :config
  (setq org-pandoc-options-for-beamer-pdf '((pdf-engine . "xelatex"))
       	org-latex-pdf-process '("xelatex -shell-excape -interaction nonstopmode -output-directory %o %f")))

;; org-mode居中
(add-hook 'org-mode #'(lambda ()
		     (setq visual-fill-column-width 100) ;; 宽度
		     (setq visual-fill-column-center-text t) ;; 居中
		     (setq adaptive-fill-mode t)
		     (visual-fill-column-mode 1)))

;; 图片
(setq org-image-actual-width 300)

;;; 笔记配置
;; org-roam
(use-package org-roam
  :custom
  (org-roam-directory "~/org/roam-notes/") ;; 默认笔记目录
  (org-roam-dailies-directory "~/org/daily/") ;; 默认日记目录
  (org-roam-db-gc-threshole most-positive-fixnum)

  :bind
  (("C-c n f" . org-roam-node-find)
   ("C-c n i" . org-roam-node-insert)
   ("C-c n c" . org-roam-capture)
   ("C-c n l" . org-roam-buffer-toggle) ;; 显示后链窗口
   ("C-c n u" . org-roam-ui-mode)) ;; 浏览器中可视化

  :bind-keymap
  ("C-c n d" . org-roam-dailies-map) ;; 日记菜单

  :config
  (require 'org-roam-dailies) ;; 启用日记功能
  (org-roam-db-autosync-mode) ;; 启动时自动同步数据库
  )

(use-package org-roam-ui
  :after org-roam
  :custom
  (org-roam-ui-sync-theme t) ;; 同步Emacs主题
  (org-roam-ui-follow t) ;; 笔记节点跟随
  (org-roam-ui-update-on-save t)
  )

;; org-noter
(use-package org-noter)

;;; 图片预览
;; xenops
(use-package xenops
  :after org
  :hook (org-mode . xenops-mode)
  :config
  (setq xenops-xen-mode t))

;;; babel配置
;; python
(setq python-shell-interpreter "~/usr/bin/python3")
(setq org-babel-python-command "python3")
(setq python-indent-offset 2)

(org-babel-do-load-languages
  'org-babel-load-languages
  '((python . t)))

(auto-image-file-mode t)

;; 快捷键
(global-set-key (kbd "C-c a") #'org-agenda)
(global-set-key (kbd "C-c c") #'org-capture)

(provide 'init-org)
;;; init-org.el ends here
