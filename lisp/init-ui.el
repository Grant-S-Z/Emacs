;;; init-ui.el -- themes of emacs
;;; Commentary:
;;; Code:

;;; themes
;; 主题随时间变化
(require 'theme-changer)
(change-theme 'doom-one-light 'doom-shades-of-purple)

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

;;; pixel smooth scroll
(setq mac-mouse-wheel-smooth-scroll t) ;; 平滑滚动，只在 Mac 起作用
(use-package ultra-scroll-mac
  :if (eq window-system 'mac)
  :load-path "~/.emacs.d/site-lisp/ultra-scroll-mac"
  :init
  (setq scroll-conservatively 101 ; important!
        scroll-margin 0)
  :config
  (ultra-scroll-mac-mode 1))

;;; 开始界面
(use-package dashboard
  :init
  (add-hook 'after-init-hook 'dashboard-open)

  :config
  ;; Initial buffer
  (setq initial-buffer-choice (lambda () (get-buffer-create "*dashboard*")))

  ;; Items
  (setq dashboard-items '((recents . 9)
                          (agenda . 9)))
  (setq dashboard-item-shortcuts '((recents . "r")
				   (agenda . "a")))

  ;; Icons
  (setq dashboard-display-icons-p t)
  (setq dashboard-icon-type 'nerd-icons)

  :custom
  ;; Set the title
  (dashboard-banner-logo-title "Welcome Grant. Have a good time!")
  ;; Center contents
  (dashboard-center-content t)
  ;; Logo
  ;; (dashboard-startup-banner "~/.emacs.d/img/firefly.jpg")
  (dashboard-startup-banner "~/.emacs.d/img/Robin.jpg")
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
