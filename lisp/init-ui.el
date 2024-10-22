;;; init-ui.el -- themes of emacs
;;; Commentary:
;;; Code:
;;; Themes
(add-to-list 'load-path "~/.emacs.d/site-lisp/moe-theme.el")
(require 'moe-theme)
(load-theme 'moe-light t)

;;; Pixel smooth scroll
(setq mac-mouse-wheel-smooth-scroll t) ;; 平滑滚动, only in Mac and always return error
(setq mac-mouse-wheel-mode t)
(use-package ultra-scroll-mac
  :if (eq window-system 'mac)
  :load-path "~/.emacs.d/site-lisp/ultra-scroll-mac"
  :init
  (setq scroll-conservatively 101 ; important!
        scroll-margin 0)
  :config
  (ultra-scroll-mac-mode 1))

;;; Line number
(defun grant/enable-line-numbers ()
  "Enable line numbers except in specific modes."
  (unless (or (derived-mode-p 'org-mode)
              (derived-mode-p 'latex-mode)
              (derived-mode-p 'pdf-view-mode)
              (derived-mode-p 'doc-view-mode))
    (display-line-numbers-mode 1)))
(add-hook 'prog-mode-hook 'grant/enable-line-numbers)
(custom-set-faces
 '(line-number ((nil (:font "Inconsolata"
		      :height 140))))
 '(line-number-current-line ((nil (:font "Inconsolata"
					 :height 140)))))

;;; Dashboard
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
  (setq doom-modeline-icon t)
  (setq doom-modeline-buffer-file-name-style 'auto))

;;; Minibuffer
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

(use-package rainbow-delimiters ;; 括号颜色
  :init (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

(use-package highlight-parentheses ;; 括号高亮
  :init (add-hook 'prog-mode-hook 'highlight-parentheses-mode))

(use-package highlight-indent-guides
  :hook (prog-mode . highlight-indent-guides-mode)
  :config
  (setq highlight-indent-guides-method 'bitmap))

;;; Fonts and input method
(use-package cnfonts
  :init (cnfonts-mode 1)
  :defer nil
  :bind (("C--" . cnfonts-decrease-fontsize)
  ("C-=" . cnfonts-increase-fontsize))
  :custom
  (cnfonts-personal-fontnames '(("Fira Code" "Ligconsolata" "Fantasque Sans Mono" "IBM Plex Mono" "FantasqueSansM Nerd Font Mono" "Iosevka")
                                ("LXGW WenKai Mono")
                                ("PragmataPro Mono Liga")
                                ("PragmataPro Mono Liga"))))

(use-package mixed-pitch
  :defer nil
  :hook (org-mode . mixed-pitch-mode)
  :config
  (set-face-attribute 'variable-pitch nil
                      :font "Iosevka")
  (setq fixed-pitch "Fantasque Sans Mono")) ;; when cnfonts change, this should changes as the same.

(use-package posframe)

(use-package rime ;; 输入法
  :init (toggle-input-method)
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
   (list :internal-border-width 4 ;; 调整 posframe 边框
         :font "PingFang SC"))
  (rime-posframe-style 'vertical)
  (mode-line-mule-info '((:eval (rime-lighter)))) ;; 在 modeline 显示输入法标志
  ;; 在 minibuffer 使用后自动关闭输入法
  (rime-deactivate-when-exit-minibuffer t))

;;; Dired and dirvish
;; Dired
(when (string= system-type "darwin")
  (setq dired-use-ls-dired t
        insert-directory-program "/opt/homebrew/bin/gls" ;; 因 ls 不能使用，设置为 gls
        dired-listing-switches "-aBhl --group-directories-first"))
;; Dirvish
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
