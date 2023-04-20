;;; init-package.el --- for packages
;;; Commentary:
;;; Code:

(use-package restart-emacs ;; 重启emacs
  :bind (("C-c r" . restart-emacs)))

(use-package auto-package-update ;; package自动更新
  :init (setq auto-package-update-delete-old-versions t
	      auto-package-update-hide-results t))

(use-package drag-stuff ;; 范围移动
  :bind (("<M-up>" . drag-stuff-up)
	 ("<M-down>" . drag-stuff-down)))

(use-package ivy ;; 强化minibuffer
  :defer 1
  :demand
  :hook (after-init . ivy-mode)
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t
	ivy-initial-inputs-alist nil
	ivy-count-format "%d/%d "
	enable-recursive-minibuffers t
	ivy-re-builders-alist '((t . ivy--regex-ignore-order))))

(use-package counsel ;; 文件搜索
  :after ivy
  :bind (("M-x" . counsel-M-x)
	 ("C-x C-f" . counsel-find-file)
	 ("C-c f" . counsel-recentf)
	 ("C-c g" . counsel-git)))

;; (ivy-set-actions ;; 查找文件时删除文件
;;  'counsel-find-file
;;  '(("d" delete-file "delete")))

(use-package swiper ;; 文件内容搜索
  :after ivy
  :bind (("C-s" .swiper)
	 ("C-r" . swiper-isearch-backward))
  :config (setq swiper-action-recenter t
		swiper-include-line-number-in-search t))

(use-package crux ;; 一些快捷键
  :bind (("C-a" . crux-move-beginning-of-line)
	 ("C-c ^" . crux-top-join-line)
	 ("C-x ," . crux-find-user-init-file)
	 ("C-S-d" . crux-duplicate-current-line-or-region)
	 ("C-S-k" . crux-smart-kill-line)))

(use-package yasnippet ;; 设置snippets
  :init (yas-global-mode t)
  :hook (prog-mode . yas-minor-mode))

(use-package yasnippet-snippets ;; 提供常用snippets
  :after (yasnippet))

;; (use-package auto-yasnippet
;;   :bind
;;   (("C-c w" . aya-create)
;;    ("C-c y" . aya-expand))
;;   :config
;;   (setq aya-persist-snippets-dir (concat user-emacs-directory "~/.emacs.d/snippets")))

(use-package which-key ;; 快捷键提示
  :defer nil
  :config (which-key-mode))

;; 语法
(use-package company ;; 语法提示补全
  :bind ("C-x c" . company-mode)
  :config
  (setq company-minimum-prefix-length 2
        company-show-quick-access 'left
 	company-tooltip-align-annotations t))

;; (use-package auto-complete
;;   :hook (prog-mode . auto-complete)
;;   :config
;;   (ac-config-default))

(use-package flycheck ;; 语法检查
  :hook (prog-mode . flycheck-mode))

(use-package rainbow-delimiters ;; 括号颜色
  :init (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

(use-package highlight-parentheses ;; 括号高亮
  :init (add-hook 'prog-mode-hook 'highlight-parentheses-mode))

;; Git
(use-package magit)

;; 界面
(use-package ace-window ;; 分屏切换
  :bind (("M-o" . 'ace-window)))

(use-package dimmer ;; 确认分屏
  :hook (prog-mode . dimmer-mode)
  :config
  (dimmer-configure-company-box)
  (dimmer-configure-which-key)
  (dimmer-configure-posframe)
  (dimmer-configure-org))

(use-package dirvish ;; 文件管理
  :bind ("C-c l" . dirvish)
  :config
  (dirvish-override-dired-mode) ;; 启用dirvish覆盖dired
  (setq dirvish-default-layout '(0 0.2 0.8)) ;; 去除父目录
  (setq-default truncate-lines t) ;; 显示不自动换行
  (setq dirvish-attributes '(all-the-icons file-size) ;; 设置icons
	insert-directory-program "gls"))

(use-package shell-pop ;; 终端弹出
  :bind (("C-c s" . shell-pop))
  :config
  (setq shell-pop-term-shell "/bin/zsh"))

(use-package ws-butler ;; 自动清除空格
  :hook (prog-mode . ws-butler-mode))

(use-package super-save ;; 自动保存
  :diminish
  :defer 0.5
  :config
  (super-save-mode 1)
  (setq super-save-auto-save-when-idle t)
  (setq save-silently t))

(provide 'init-package)
;;; init-package.el ends here