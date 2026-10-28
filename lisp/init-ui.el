;;; init-ui.el -- themes of emacs  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;;; Reading and scrolling
(pixel-scroll-precision-mode 1)
(add-hook 'text-mode-hook #'visual-line-mode)
(add-hook 'text-mode-hook #'visual-line-fill-column-mode)

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
  (setq dashboard-items '((recents . 8)))
  (setq dashboard-item-shortcuts '((recents . "r")
				   ))

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
;; Use one font family for Latin, CJK, code and proportional text.
(defun grant-setup-fonts (&optional frame)
  "Use Maple Mono NF CN for all Latin and CJK text."
  (when (display-graphic-p frame)
    (set-face-attribute 'default frame :family "Maple Mono NF CN" :height 170)
    (set-face-attribute 'fixed-pitch frame :family "Maple Mono NF CN")
    (set-face-attribute 'variable-pitch frame :family "Maple Mono NF CN")
    (dolist (charset '(kana han cjk-misc bopomofo hangul))
      (set-fontset-font "fontset-default" charset
                        (font-spec :family "Maple Mono NF CN" :size 17.0)))))

(grant-setup-fonts)
(add-hook 'after-make-frame-functions #'grant-setup-fonts)

(global-set-key (kbd "C--") #'text-scale-decrease)
(global-set-key (kbd "C-=") #'text-scale-increase)

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
;; ;; Use a "mixed" setup: proportional body, monospace code blocks.
;; ;; `mixed-pitch-mode` cooperates better with org-src overlays than
;; ;; enabling `variable-pitch-mode` directly.

;; (load-theme 'ef-frost :no-confirm)
(load-theme 'ef-reverie :no-confirm)
;; (load-theme 'ef-trio-light :no-confirm)
;; (load-theme 'doom-nord-light :no-confirm)
;; (load-theme 'catppuccin :no-confirm)
;; (setq catppuccin-flavor 'latte)
;; (catppuccin-reload)
;; (load-theme 'doric-beach :no-confirm) ;; yellow bkg, i do not like
;; (load-theme 'doric-beach :no-confirm) ;; few colors, black
;; (load-theme 'modus-operandi :no-confirm) ;; few colors
;; (load-theme 'doom-flatwhite :no-confirm) ;; no contrast


(use-package writeroom-mode ;;; center texts
  :hook (org-mode . writeroom-mode)
  :custom
  (writeroom-maximize-window nil)
  (writeroom-mode-line t)
  (writeroom-width 100)
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
  :bind ("C-x d" . dirvish)
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

;; Treemacs
(use-package treemacs
  :bind (("C-c t" . treemacs)
         ("M-0" . treemacs-select-window)
         ("C-c T" . treemacs-add-and-display-current-project-exclusively))
  :custom
  (treemacs-width 32)
  (treemacs-indentation 1)                  ;; more compact file tree
  (treemacs-collapse-dirs 3)                ;; collapse single-child directories
  (treemacs-sorting 'alphabetic-numeric-asc) ;; natural file name sorting
  (treemacs-silent-refresh t)               ;; suppress refresh messages
  :config
  (treemacs-follow-mode 1)       ;; locate the current file in the tree
  (treemacs-filewatch-mode 1)    ;; refresh when files change
  (treemacs-git-mode 'deferred)  ;; show Git status asynchronously
  (treemacs-fringe-indicator-mode 'always))

(use-package treemacs-nerd-icons
  :after treemacs
  :demand t
  :config
  (treemacs-load-theme "nerd-icons"))

;;; Ligature
(let ((alist '((33 . ".\\(?:\\(?:==\\|!!\\)\\|[!=]\\)")
               (35 . ".\\(?:###\\|##\\|_(\\|[#(?[_{]\\)")
               (36 . ".\\(?:>\\)")
               (37 . ".\\(?:\\(?:%%\\)\\|%\\)")
               (38 . ".\\(?:\\(?:&&\\)\\|&\\)")
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

;;; Frame borders and window dividers
(modify-all-frames-parameters
 '((right-divider-width . 5)
   (internal-border-width . 5)))
(dolist (face '(window-divider
                window-divider-first-pixel
                window-divider-last-pixel))
  (face-spec-reset-face face)
  (set-face-foreground face (face-attribute 'default :background)))
(set-face-background 'fringe (face-attribute 'default :background))

(provide 'init-ui)
;;; init-ui.el ends here
