;;; init-ui.el -- themes of emacs
;;; Commentary:
;;; Code:

;; (use-package night-owl-theme ;; theme
;;   :init (load-theme 'night-owl t))

;; (defun night-owl/ivy-format-function-line (cands)
;;   (let ((str (ivy-format-function-line cands)))
;;     (font-lock-append-text-property 0 (length str) 'face 'ivy-not-current str)
;;     str))

;; (setq ivy-format-function #'night-owl/ivy-format-function-line)

(use-package solo-jazz-theme
  :init (load-theme 'solo-jazz t))

;; (use-package modus-themes
;;   ;; :init (load-theme 'modus-operandi)
;;   :init (load-theme 'modus-operandi-deuteranopia t)
;; )

;; (load-theme 'leuven)

;; 主题随时间变化
;; (add-to-list 'load-path "~/.emacs.d/repos/theme-changer")
;; (require 'theme-changer)
;; (change-theme 'solo-jazz 'night-owl)

;; 尝试为org-mode单独配置主题
;; (use-package leuven-theme
;;   :after org)

;; (defvar saved-theme 'night-owl)

;; (defun my-org-mode-hook ()
;;   ;; 加载指定主题
;;   (load-theme 'leuven-dark t))

;; (defun my-change-major-mode-hook ()
;;   ;; 恢复默认主题
;;   (setq custom-enabled-themes saved-theme))

;; (add-hook 'org-mode-hook 'my-org-mode-hook)
;; (add-hook 'change-major-mode-hook 'my-change-major-mode-hook)

(use-package all-the-icons ;; icons
  :if (display-graphic-p))

(use-package dashboard ;; 开始界面
  :init
  (add-hook 'after-init-hook 'dashboard-open)
  
  :config
  (setq initial-buffer-choice (lambda () (get-buffer-create "*dashboard*")))

  :custom
  ;; Set the title
  (dashboard-banner-logo-title "Welcome, Grant!")
  ;; Center contents
  (dashboard-center-content t)
  ;; Logo
  (dashboard-startup-banner "~/.emacs.d/img/madeline-strawberry.gif")
  ;; Footnote
  (dashboard-footer-messages '("True mastery of any skill takes a lifetime."))
  
  (dashboard-set-heading-icons t)
  (dashboard-set-file-icons t)
  (dashboard-set-init-info t)
  (dashboard-set-navigator t))

(use-package time ;; 显示时间
  :init
  (setq display-time-24hr-format t ;; hours
	display-time-day-and-date t) ;; dates
  :config
  (display-time-mode t))

;; (use-package visual-fill-column ;; 居中
;;   :hook ((prog-mode) . (lambda () ;; tex-mode和org-mode不居中
;; 		     (setq visual-fill-column-width 100) ;; 宽度
;; 		     (setq visual-fill-column-center-text t) ;; 居中
;; 		     (setq adaptive-fill-mode t)
;; 		     (global-visual-fill-column-mode 1))))

(use-package smart-mode-line
  :init
  (setq sml/no-confirm-load-theme t
	sml/theme 'respectful)
  (sml/setup))

;;; minibuffer
;; (use-package ivy ;; 强化minibuffer
;;   :defer 1
;;   :demand
;;   :hook (after-init . ivy-mode)
;;   :config
;;   (ivy-mode 1)
;;   (setq ivy-use-virtual-buffers t
;; 	ivy-initial-inputs-alist nil
;; 	ivy-count-format "%d/%d "
;; 	enable-recursive-minibuffers t
;; 	ivy-re-builders-alist '((t . ivy--regex-ignore-order))))

;; (use-package counsel
;;   :after ivy
;;   :bind (("M-x" . counsel-M-x)
;; 	 ("C-x C-f" . counsel-find-file)
;; 	 ("C-c f" . counsel-recentf)
;; 	 ("C-c g" . counsel-git)))

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
  (rime-librime-root "~/.emacs.d/librime/dist")
  (rime-emacs-module-header-root "~/.emacs.d/librime"))

(use-package emacs
  :init
  ;; Add prompt indicator to `completing-read-multiple'.
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

  :config
  ;(setq display-line-numbers-type 'relative) ;; relative line numbers
  ;(global-display-line-numbers-mode t)

  (progn
    (set-face-attribute 'default nil ;; 英文字体
			:font "Fantasque Sans Mono"
			:height 150)
    (dolist (charset '(kana han symbol cjk-misc bopomofo)) ;; 中文字体
      (set-fontset-font (frame-parameter nil 'font)
			charset (font-spec :family "LXGW WenKai Mono")))))

(provide 'init-ui)
;;; init-ui.el ends here
