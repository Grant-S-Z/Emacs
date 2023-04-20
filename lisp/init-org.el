;;; init-org.el --- for org
;;; Commentary:
;;; Code:

;; 待办事项关键词
(setq org-todo-keyword-faces '(("TODO" . "red") ("DOING" . "yellow") ("SHELVED" . "orange") ("DONE" . "blue") ("CANCELED" . "green")))

;; 日程文件位置
(setq org-agenda-files (list "~/org/agenda/work.org"))

;; 任务capture-templetes
(setq org-capture-templates nil)
(add-hook 'org-mode-hook (lambda () (setq truncate-lines nil)))

(add-to-list 'org-capture-templates '("t" "Tasks")) ;; 任务模版集
(add-to-list 'org-capture-templates
	     '("tr" "Reading Task" entry
	       (file+headline "~/org/task.org" "Reading")
	       "* TODO %^{Contentsname}\n%u\n%a\n" :clock-in t :clock-resume t))
(add-to-list 'org-capture-templates
	     '("tw" "Work Task" entry
	       (file+headline "~/org/task.org" "Work")
	       "* TODO %^{Workname}\n%u\n%a\n" :clock-in t :clock-resume t))
(add-to-list 'org-capture-templates
	     '("th" "Homework Task" entry
	       (file+headline "~/org/task.org" "Homework")
	       "* TODO %^{Homeworkname}\n%u\n%a\n" :clock-in t :clock-resume t))
(add-to-list 'org-capture-templates
	     '("tl" "Long Task" entry
	       (file+headline "~/org/task.org" "Long Task")
	       "* TODO %^{Longtaskname}\n%u\n%a\n" :clock-in t :clock-resume t))
(add-to-list 'org-capture-templates
	     '("tq" "Questions" entry
	       (file+headline "~/org/task.org" "Questions")
	       "* TODO %^{Questionname}\n%u\n%a\n" :clock-in t :clock-resume t))
(add-to-list 'org-capture-templates
	     '("tt" "Thesis Doubts" entry
	       (file+headline "~/org/task.org" "Thesis Doubts")
	       "* TODO %^{Doubtsname}\n%u\n%a\n" :clock-in t :clock-resume t))

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

;;; babel配置
;; python
(setq python-shell-interpreter "~/code/python/venv3.11/bin/python")
(setq org-babel-python-command "python3")
(setq python-indent-offset 2)

(org-babel-do-load-languages
  'org-babel-load-languages
  '((python . t)))

(auto-image-file-mode t)

;; prettify-symbols-mode配置
;; (add-hook 'org-mode (lambda () (setq prettify-symbols-mode t)))

;; 快捷键
(global-set-key (kbd "C-c a") #'org-agenda)
(global-set-key (kbd "C-c c") #'org-capture)

(provide 'init-org)
;;; init-org.el ends here
