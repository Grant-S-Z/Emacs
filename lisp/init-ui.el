;;; init-ui.el -- themes of emacs
;;; Commentary:
;;; Code:

(use-package night-owl-theme ;; theme
  :init (load-theme 'night-owl t))

(defun night-owl/ivy-format-function-line (cands)
  "Transform CANDS into a string for minibuffer."
  (let ((str (ivy-format-function-line cands)))
    (font-lock-append-text-property 0 (length str) 'face 'ivy-not-current str)
    str))

(setq ivy-format-function #'night-owl/ivy-format-function-line)

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

(use-package visual-fill-column ;; 居中
  :hook ((prog-mode org-mode tex-mode) . (lambda ()
		     (setq visual-fill-column-width 100) ;; 宽度
		     (setq visual-fill-column-center-text t) ;; 居中
		     (setq adaptive-fill-mode t)
		     (global-visual-fill-column-mode 1))))

(use-package smart-mode-line
  :init
  (setq sml/no-confirm-load-theme t
	sml/theme 'respectful)
  (sml/setup))

(use-package rime ;; 输入法
  :custom
  (default-input-method "rime")
  (rime-librime-root "~/.emacs.d/librime/dist")
  (rime-emacs-module-header-root "~/.emacs.d/librime"))

(use-package emacs
  :config
  (setq display-line-numbers-type 'relative) ;; relative line numbers
  (global-display-line-numbers-mode t)
  (progn
    (set-face-attribute 'default nil ;; 英文字体
			:font "Fantasque Sans Mono"
			:height 150)
    (dolist (charset '(kana han symbol cjk-misc bopomofo)) ;; 中文字体
      (set-fontset-font (frame-parameter nil 'font)
			charset (font-spec :family "LXGW WenKai Mono")))))

(provide 'init-ui)
;;; init-ui.el ends here
