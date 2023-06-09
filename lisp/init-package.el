;;; init-package.el --- for packages
;;; Commentary:
;;; Code:
(use-package restart-emacs ;; 重启 emacs
  :bind (("C-c r" . restart-emacs)))

(use-package auto-package-update ;; package 自动更新
  :init (setq auto-package-update-delete-old-versions t
	      auto-package-update-hide-results t))

(use-package drag-stuff ;; 范围移动
  :bind (("<M-up>" . drag-stuff-up)
	 ("<M-down>" . drag-stuff-down)))

(use-package keycast ;; 演示按键
  :init (keycast-header-line-mode)
  :config
  (push '(org-self-insert-command nil nil) keycast-substitute-alist)
  (push '(self-insert-command nil nil) keycast-substitute-alist)
  (push '(mouse-drag-region nil nil) keycast-substitute-alist)
  (push '(mouse-set-point nil nil) keycast-substitute-alist)
  (push '(lsp-ui-doc--handle-mouse-movement nil nil) keycast-substitute-alist)
  (push '(mac-mwheel-scroll nil nil) keycast-substitute-alist))

(use-package smooth-scrolling
  :init
  :config (smooth-scrolling-mode t))

;; (ivy-set-actions ;; 查找文件时删除文件
;;  'counsel-find-file
;;  '(("d" delete-file "delete")))

;; (use-package swiper ;; 文件内容搜索
;;   :after vertico
;;   :bind (("C-s" .swiper)
;; 	 ("C-r" . swiper-isearch-backward)
;; 	 ("C-x M-r" . query-replace-regexp))
;;   :config (setq swiper-action-recenter t
;; 		swiper-include-line-number-in-search t))

(use-package consult
  :bind (("C-s" . consult-line)))

(use-package crux ;; 一些快捷键
  :bind (("C-a" . crux-move-beginning-of-line)
	 ("C-c ^" . crux-top-join-line)
	 ("C-x ," . crux-find-user-init-file)
	 ("C-S-d" . crux-duplicate-current-line-or-region)
	 ("C-S-k" . crux-smart-kill-line)))

(use-package yasnippet ;; 设置 snippets
  :init (yas-global-mode t)
  :hook (prog-mode . yas-minor-mode))

(use-package yasnippet-snippets ;; 提供常用 snippets
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
  ;; :init (global-company-mode t)
  :config
  (setq company-minimum-prefix-length 1
        company-show-quick-access 'left
 	company-tooltip-align-annotations t
	company-idle-delay 0.0 ;; default is 0.2
	))

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

(use-package dimmer ;; 黯淡分屏
  :hook (prog-mode . dimmer-mode)
  :config
  (dimmer-configure-company-box)
  (dimmer-configure-which-key)
  (dimmer-configure-posframe)
  (dimmer-configure-org))

(use-package dirvish ;; 文件管理
  :bind ("C-c C-x l" . dirvish)
  :config
  (dirvish-override-dired-mode) ;; 启用 dirvish 覆盖 dired
  (setq dirvish-default-layout '(0 0.3 0.7)) ;; 去除父目录
  (setq dirvish-attributes '(all-the-icons file-size) ;; 设置 icons
	insert-directory-program "gls"))

(use-package shell-pop ;; 终端弹出
  :bind (("C-c s" . shell-pop)))

(use-package ws-butler ;; 自动清除空格
  :hook (prog-mode . ws-butler-mode))

(use-package super-save ;; 自动保存
  :diminish
  :defer 0.5
  :config
  (super-save-mode 1)
  (setq super-save-auto-save-when-idle t)
  (setq save-silently t))

(use-package pangu-spacing
  :init
  (global-pangu-spacing-mode 1)
  :config
  (setq pangu-spacing-real-insert-separtor t))

;; md-mode
(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

;; hugo
(use-package easy-hugo
  :init
  (setq easy-hugo-basedir "~/Code/Site/GrantSite/") ;; 网站本地文件根目录
  (setq easy-hugo-url "https://Grant-S-Z.github.io/GrantSite") ;; url 路径
  (setq easy-hugo-sshdomain "Grant-S-Z.github.io")
  (setq easy-hugo-previewtime "300")
  (setq easy-hugo-default-ext ".org")
  :bind ("C-c b" . easy-hugo))

(use-package ox-hugo
  :init
  (setq org-hugo-base-dir "~/Code/Site/GrantSite/")
  (setq org-hugo-section "post")
  :pin melpa
  :after ox)

(provide 'init-package)
;;; init-package.el ends here
