;;; init-ui.el -- themes of emacs
;;; Commentary:
;;; Code:

;;; themes
;; 主题随时间变化
(setq calendar-location-name "Beijing, CN")
(setq calendar-latitude 39.9)
(setq calendar-longitude 116.4)
(require 'theme-changer)
(change-theme 'ef-frost 'doom-shades-of-purple)

;;; Set transparency
(set-frame-parameter (selected-frame) 'alpha '(100 100))
(add-to-list 'default-frame-alist '(alpha 100 100))

;;; all-the-icons
(use-package all-the-icons
  :if (display-graphic-p))

;;; Line number
(setq display-line-numbers-type 'relative)
(defun grant/enable-line-numbers ()
  "Enable line numbers except in specific modes."
  (unless (or (derived-mode-p 'org-mode)
              (derived-mode-p 'latex-mode)
              (derived-mode-p 'pdf-view-mode)
              (derived-mode-p 'doc-view-mode))
    (display-line-numbers-mode 1)))
(add-hook 'prog-mode-hook 'grant/enable-line-numbers)

;;; 开始界面
(use-package dashboard
  :init
  (add-hook 'after-init-hook 'dashboard-open)

  :config
  (setq initial-buffer-choice (lambda () (get-buffer-create "*dashboard*")))
  (setq dashboard-items '((recents  . 9)
                          (bookmarks . 5)
                          (agenda . 6)))

  :custom
  ;; Set the title
  (dashboard-banner-logo-title "Welcome, Grant!")
  ;; Center contents
  (dashboard-center-content t)
  ;; Logo
  (dashboard-startup-banner "~/.emacs.d/img/firefly.jpeg")
  ;; Footnote
  (dashboard-footer-messages '
  ("True mastery of any skill takes a lifetime."))

  (dashboard-set-heading-icons t)
  (dashboard-set-file-icons t)
  (dashboard-set-init-info t)
  (dashboard-set-navigator t))

;;; Modeline
(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :config
  (display-time)
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

;; Posframe
(use-package posframe)

(use-package rime ;; 输入法
  :defer nil
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

  (defvar eh-space "  ")

  ;; 设置 mode-line
  ;; 在 mode-line 最后追加一个半角空格，一个全角空格，防止因为字体高度原因导致 mode-line 抖动。
  (setq mode-line-end-spaces
	'(:eval (if (display-graphic-p) eh-space "-%-")))

  (defun eh-tab-line-format (orig_func)
    "在 tab-line 的最后添加一个全角空格，防止 tab-line 抖动。"
    (list (funcall orig_func) eh-space))

  (advice-add 'tab-line-format :around #'eh-tab-line-format)

  (progn
    (set-face-attribute 'default nil    ;:font "Fantasque Sans Mono"
					;:font "DejaVu Sans Mono"
			                ;:font "IBM Plex Mono"
					:font "Ligconsolata"
					:height 160)
    (dolist (charset '(kana han symbol cjk-misc bopomofo)) ;; Chinese fonts
      (set-fontset-font (frame-parameter nil 'font)
			charset (font-spec :family "LXGW WenKai Mono")))))

;;; 文件管理
;; dired
(when (string= system-type "darwin")
  (setq dired-use-ls-dired t
        insert-directory-program "/opt/homebrew/bin/gls" ;; 因 ls 不能使用，设置为 gls
        dired-listing-switches "-aBhl --group-directories-first"))
;; dirvish
(use-package dirvish
  :defer nil
  :bind ("C-c l" . dirvish-side)
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
  (dirvish-side-follow-mode))

(provide 'init-ui)
;;; init-ui.el ends here
