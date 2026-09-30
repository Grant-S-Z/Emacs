;;; init-ui.el -- themes of emacs  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;;; Line number
(defun grant/enable-line-numbers ()
  "Enable line numbers except in specific modes."
  (unless (or (derived-mode-p 'org-mode)
              (derived-mode-p 'latex-mode)
              (derived-mode-p 'pdf-view-mode)
              (derived-mode-p 'doc-view-mode))
    (display-line-numbers-mode 1)))
(add-hook 'prog-mode-hook 'grant/enable-line-numbers)

;;; Dashboard
(use-package dashboard
  :init
  (add-hook 'after-init-hook 'dashboard-open)
  :config
  ;; Initial buffer
  (setq initial-buffer-choice (lambda () (get-buffer-create "*dashboard*")))

  ;; Items
  (setq dashboard-items '((recents . 8)
                          (agenda . 7)))
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
  (dashboard-startup-banner "~/.emacs.d/img/celeste.png")
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
  (setq doom-modeline-height 20)
  (display-time)
  (setq doom-modeline-time t)
  (setq doom-modeline-icon t)
  (setq doom-modeline-github nil)
  (setq doom-modeline-battery nil)
  (setq doom-modeline-buffer-file-name-style 'buffer-name)
  (setq doom-modeline--eglot t)
  (setq doom-modeline-enable-word-count nil)
  (setq doom-modeline-env-python-executable "~/miniconda3/bin/python3"))

;;; Minibuffer
(use-package vertico
  :init (vertico-mode)
  :custom
  (vertico-count 15))

(use-package savehist ;; persist history over restarting Emacs, and vertico sorts by history position.
  :init (savehist-mode 1))

(use-package orderless ;; optionally use the orderless completion style.
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles basic partial-completion))))
  )

(use-package marginalia
  :init (marginalia-mode 1))

;;; Useful highlights and colors
(use-package paren
  :config
  (setq show-paren-when-point-inside-paren t
        show-paren-when-point-in-periphery t
        show-paren-context-when-offscreen t
        show-paren-delay 0.1)
  )

(use-package rainbow-delimiters ;; color of delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package hl-line
  :hook (after-init . global-hl-line-mode)
  :config
  (setq hl-line-sticky-flag nil)
  ;; Highlight starts from EOL, to avoid conflicts with other overlays
  (setq hl-line-range-function (lambda () (cons (line-end-position)
						(line-beginning-position 2))))
  )

(use-package indent-bars ;; indent lines
  :init
  (setq indent-tabs-mode t
	indent-bars-no-descend-lists t
	indent-bars-prefer-character t)
  :hook (prog-mode . indent-bars-mode))

;; Outli, unfold codes as org
(use-package outli
  :vc (:url "https://github.com/jdtsmith/outli")
  :hook (prog-mode . outli-mode))

(use-package hl-todo ;; highlight keywords when coding and jump
  :init (global-hl-todo-mode))

;;; Fonts and input method
(use-package cnfonts
  :init (cnfonts-mode 1)
  :bind (("C--" . cnfonts-decrease-fontsize)
	 ("C-=" . cnfonts-increase-fontsize))
  :custom
  (cnfonts-personal-fontnames '(("Ligconsolata" "FantasqueSansM Nerd Font Mono" "Iosevka" "Bookerly" "Literata" "STKaiti")
                                ("FZYouSong GBK" "Source Han Serif SC" "LXGW WenKai Mono")
                                ("PragmataPro Mono Liga")
                                ("PragmataPro Mono Liga"))))

(use-package posframe)

;; Input method
(use-package rime
  :custom
  (default-input-method "rime")
  (rime-librime-root "~/.emacs.d/librime/dist") ;; librime path
  (rime-emacs-module-header-root "")
  (rime-share-data-dir "~/Library/Rime") ;; share path
  (rime-user-data-dir "~/.emacs.d/rime") ;; real path used in Emacs rime
  (rime-cursor ".")
  (rime-show-candidate 'posframe) ;; use posframe
  (rime-commit1-forall t) ;; show the first choice
  (rime-posframe-properties
   (list :internal-border-width 1))
  (rime-posframe-style 'vertical)
  (mode-line-mule-info '((:eval (rime-lighter)))) ;; show rime symbol on modeline
  (rime-deactivate-when-exit-minibuffer t) ;; deactivate rime in minibuffer automatically
  )

;;; Themes
;; Use a "mixed" setup: proportional body, monospace code blocks.
;; `mixed-pitch-mode` cooperates better with org-src overlays than
;; enabling `variable-pitch-mode` directly.
(use-package mixed-pitch
  :hook (org-mode . mixed-pitch-mode)
  :config
  (set-face-attribute 'variable-pitch nil :family "Literata")
  ;; (set-face-attribute 'variable-pitch nil :family "LXGW WenKai Mono")
  ;; (set-face-attribute 'variable-pitch nil :family "STKaiti")
  (set-face-attribute 'fixed-pitch nil :family "FantasqueSansM Nerd Font Mono")
  ;; org-level-N 默认继承 outline-N -> font-lock-*-face，mixed-pitch 会把
  ;; font-lock-* 固定为 fixed-pitch（等宽），导致标题变成 Fantasque。
  ;; 用列表继承：variable-pitch 在前提供 Literata（family 属性优先取前者），
  ;; outline-N 在后保留主题（ef-frost）的标题颜色与字重。
  ;; 注意：不能单独 :inherit 'variable-pitch，那样会切断颜色继承链。
  (dolist (pair '((org-level-1 . outline-1)
                  (org-level-2 . outline-2)
                  (org-level-3 . outline-3)
                  (org-level-4 . outline-4)
                  (org-level-5 . outline-5)
                  (org-level-6 . outline-6)
                  (org-level-7 . outline-7)
                  (org-level-8 . outline-8)))
    (set-face-attribute (car pair) nil :inherit (list 'variable-pitch (cdr pair))))
  (set-face-attribute 'org-document-title nil :inherit '(variable-pitch)))


;; (set-face-attribute 'variable-pitch nil :family "Literata")
;; (set-face-attribute 'fixed-pitch nil :family "FantasqueSansM Nerd Font Mono")

;; (load-theme 'ef-trio-light :no-confirm)
;; (load-theme 'ef-arbutus :no-confirm)
;; (load-theme 'ef-light :no-confirm)
(load-theme 'ef-frost :no-confirm)
;; (load-theme 'ef-day :no-confirm)
;; (load-theme 'ef-night :no-confirm)
;; (load-theme 'ef-winter :no-confirm)
;; (load-theme 'ef-owl :no-confirm)
;; (load-theme 'catppuccin :no-confirm)
;; (setq catppuccin-flavor 'mocha)
;; (catppuccin-reload)
;; (load-theme 'modus-operandi)
;; (load-theme 'modus-operandi-tritanopia)
;; (load-theme 'modus-operandi-deuteranopia)

;; (add-to-list 'load-path "~/.emacs.d/site-lisp/moe-theme.el")
;; (require 'moe-theme)
;; (load-theme 'moe-light t)

(use-package writeroom-mode ;;; center texts
  :hook (org-mode . writeroom-mode)
  :custom
  (writeroom-maximize-window nil)
  (writeroom-mode-line t)
  (writeroom-width 80)
  (writeroom-global-effects '(writeroom-set-alpha
			      writeroom-set-menu-bar-lines
			      writeroom-set-tool-bar-lines
			      writeroom-set-vertical-scroll-bars
			      writeroom-set-bottom-divider-width))
  (setq-local line-spacing 0.1))

;;; Dired
(when *is-mac*
  (setq dired-use-ls-dired t
        insert-directory-program "/opt/homebrew/bin/gls" ;; replace ls with gls
        dired-listing-switches "-aBhl --group-directories-first"))
;; Dirvish
(use-package dirvish
  :bind
  (("C-c l" . dirvish-side)
   ("C-x d" . dirvish))
  :custom
  (dirvish-quick-access-entries
   '(("h" "~/" "Home")
     ("d" "~/Downloads" "Downloads")))
  (dirvish-attributes '(subtree-state
		        nerd-icons
		        collapse
			git-msg
		        file-size))
  :config
  (dirvish-override-dired-mode) ;; replace dired ui with dirvish
  (dirvish-side-follow-mode))

;;; Ligature
(let ((alist '((33 . ".\\(?:\\(?:==\\|!!\\)\\|[!=]\\)")
               (35 . ".\\(?:###\\|##\\|_(\\|[#(?[_{]\\)")
               (36 . ".\\(?:>\\)")
               (37 . ".\\(?:\\(?:%%\\)\\|%\\)")
               (38 . ".\\(?:\\(?:&&\\)\\|&\\)")
               ;; (42 . ".\\(?:\\(?:\\*\\*/\\)\\|\\(?:\\*[*/]\\)\\|[*/>]\\)")
               (43 . ".\\(?:\\(?:\\+\\+\\)\\|[+>]\\)")
               (45 . ".\\(?:\\(?:-[>-]\\|<<\\|>>\\)\\|[<>}~-]\\)")
               (46 . ".\\(?:\\(?:\\.[.<]\\)\\|[.=-]\\)")
               (47 . ".\\(?:\\(?:\\*\\*\\|//\\|==\\)\\|[*/=>]\\)")
               (48 . ".\\(?:x[a-zA-Z]\\)")
               (58 . ".\\(?:::\\|[:=]\\)")
               (59 . ".\\(?:;;\\|;\\)")
               (60 . ".\\(?:\\(?:!--\\)\\|\\(?:~~\\|->\\|\\$>\\|\\*>\\|\\+>\\|--\\|<[<=-]\\|=[<=>]\\||>\\)\\|[*$+~/<=>|-]\\)")
               (61 . ".\\(?:\\(?:/=\\|:=\\|<<\\|=[=>]\\|>>\\)\\|[<=>~]\\)")
               (62 . ".\\(?:\\(?:=>\\|>[=>-]\\)\\|[=>-]\\)")
               (63 . ".\\(?:\\(\\?\\?\\)\\|[:=?]\\)")
               (91 . ".\\(?:]\\)")
               (92 . ".\\(?:\\(?:\\\\\\\\\\)\\|\\\\\\)")
               (94 . ".\\(?:=\\)")
               (119 . ".\\(?:ww\\)")
               (123 . ".\\(?:-\\)")
               (124 . ".\\(?:\\(?:|[=|]\\)\\|[=>|]\\)")
               (126 . ".\\(?:~>\\|~~\\|[>=@~-]\\)")
               )
             ))
  (dolist (char-regexp alist)
    (set-char-table-range composition-function-table (car char-regexp)
                          `([,(cdr char-regexp) 0 font-shape-gstring]))))

(provide 'init-ui)
;;; init-ui.el ends here
