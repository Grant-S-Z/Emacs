;;; init-ui.el -- themes of emacs
;;; Commentary:
;;; Code:

;;; themes
;; (use-package doom-themes
;;   :init (load-theme 'doom-one-light t))

;; 主题随时间变化
(add-to-list 'load-path "~/.emacs.d/repos/theme-changer")
(require 'theme-changer)
(change-theme 'doom-one-light 'doom-one)

(use-package all-the-icons ;; all-the-icons
  :if (display-graphic-p))

(use-package dashboard ;; 开始界面
  :init
  (add-hook 'after-init-hook 'dashboard-open)
  
  :config
  (setq initial-buffer-choice (lambda () (get-buffer-create "*dashboard*")))
  (setq dashboard-items '((recents  . 7)
                          (bookmarks . 5)
                          (agenda . 5)))

  :custom
  ;; Set the title
  (dashboard-banner-logo-title "Welcome, Grant!")
  ;; Center contents
  (dashboard-center-content t)
  ;; Logo
  (dashboard-startup-banner "~/.emacs.d/img/madeline-strawberry.gif")
  ;; Footnote
  (dashboard-footer-messages '
  ;("True mastery of any skill takes a lifetime.")
  ("Schedule your tomorrow."))
  
  (dashboard-set-heading-icons t)
  (dashboard-set-file-icons t)
  (dashboard-set-init-info t)
  (dashboard-set-navigator t))

(use-package time ;; 显示时间
  :init
  (setq display-time-24hr-format t ;; 显示时间
	display-time-day-and-date t) ;; 显示日期
  :config
  (display-time-mode t))

;;; Modeline
(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :config
  (setq doom-modeline-buffer-file-name 'auto)
  (setq doom-modeline-icon t))

;;; minibuffer
(use-package vertico
  :init (vertico-mode))

(use-package savehist ;; Persist history over Emacs restarts. Vertico sorts by history position.
  :after vertico
  :init (savehist-mode))

(use-package orderless ;; Optionally use the orderless completion style.
  :after vertico
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-consult-dispatch orderless-affix-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

(use-package marginalia
  :after vertico
  :init (marginalia-mode t))

(use-package embark
  :bind (("C-." . embark-act))
  :config
  (setq prefix-help-command 'embark-prefix-help-command))

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package rime ;; 输入法
  :custom
  (default-input-method "rime")
  (rime-librime-root "~/.emacs.d/librime/dist") ;; librime 位置
  (rime-emacs-module-header-root "/opt/homebrew/opt/emacs-mac/include/") ;; Emacs 头文件位置
  (rime-share-data-dir "~/Library/Rime") ;; 共享目录
  (rime-user-data-dir "~/.emacs.d/rime") ;; Emacs 目录，需要同步
  (rime-cursor ".")
  (rime-show-candidate 'posframe) ;; 使用 posframe 显示输入法
  (rime-commit1-forall t) ;; 在输入位置显示首个备选项
  (rime-posframe-properties
   (list :internal-border-width 1 ;; 调整 posframe 边框
	 :font "LXGW WenKai"
	 :color 'rime-default-face))
  (mode-line-mule-info '((:eval (rime-lighter)))) ;; 在 modeline 显示输入法标志
  ;; 在 minibuffer 使用后自动关闭输入法
  (rime-deactivate-when-exit-minibuffer t))

(use-package emacs
  :init
  ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Emacs 28: Hide commands in M-x which do not work in the current mode.
  ;; Vertico commands are hidden in normal buffers.
  (setq read-extended-command-predicate
        #'command-completion-default-include-p)

  ;; Enable recursive minibuffers
  (setq enable-recursive-minibuffers t)

  (progn
    (set-face-attribute 'default nil    ;:font "Fantasque Sans Mono"
					;:font "Fira Code"
					;:font "DejaVu Sans Mono"
					:font "Inconsolata"
					:height 130)
    (dolist (charset '(kana han symbol cjk-misc bopomofo)) ;; Chinese fonts
      (set-fontset-font (frame-parameter nil 'font)
			charset (font-spec :family "LXGW WenKai")))))

;;; 文件管理
;; dired
(when (string= system-type "darwin")
  (setq dired-use-ls-dired t
        insert-directory-program "/opt/homebrew/bin/gls" ;; 因 ls 不能使用，设置为 gls
        dired-listing-switches "-aBhl --group-directories-first"))
;; dirvish
(use-package dirvish
  :bind
  (("C-c l" . dirvish-side)
   ("C-c f" . dirvish-fd))
  :custom
  (dirvish-quick-access-entries
   '(("h" "~/" "Home")
     ("d" "~/Downloads" "Downloads")
     ("s" "~/Senior" "Senior")))
  (dirvish-default-layout '(0 0.2 0.8))
  (dirvish-attributes '(subtree-state
		        nerd-icons
		        collapse
		        file-size)) ;; 设置显示
  :config
  (dirvish-override-dired-mode) ;; 启用 dirvish 覆盖 dired
  (dirvish-side-follow-mode)
)

(use-package dired-subtree)

(provide 'init-ui)
;;; init-ui.el ends here
