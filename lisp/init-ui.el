;;; init-ui.el -- themes of emacs
;;; Commentary:
;;; Code:
;;; Themes
(use-package catppuccin-theme
  :init
  (load-theme 'catppuccin :no-confirm)
  :config
  (setq catppuccin-flavor 'latte) ;; frappe, latte, macchiato, mocha
  (catppuccin-reload))

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
  (setq doom-modeline-enable-word-count nil))

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
        completion-category-overrides '((file (styles basic partial-completion)))))

(use-package marginalia
  :init (marginalia-mode 1))

;;; Side tree
(use-package treemacs
  :bind ("C-c t" . treemacs)
  :config
  (setq treemacs-show-hidden-files nil))

;;; Useful highlights and colors
(use-package paren
  :config
  (setq show-paren-when-point-inside-paren t
        show-paren-when-point-in-periphery t
        show-paren-context-when-offscreen t
        show-paren-delay 0.1))

(use-package rainbow-delimiters ;; color of delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package hl-line
  :hook (after-init . global-hl-line-mode)
  :config
  (setq hl-line-sticky-flag nil)
  ;; Highlight starts from EOL, to avoid conflicts with other overlays
  (setq hl-line-range-function (lambda () (cons (line-end-position)
						(line-beginning-position 2)))))

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
  (cnfonts-personal-fontnames '(("Ligconsolata" "FantasqueSansM Nerd Font Mono" "Iosevka")
                                ("FZYouSong GBK")
                                ("PragmataPro Mono Liga")
                                ("PragmataPro Mono Liga"))))

(use-package posframe)

;; Input method
(use-package rime
  :custom
  (default-input-method "rime")
  (rime-librime-root "~/.emacs.d/librime/dist") ;; librime path
  (rime-share-data-dir "~/Library/Rime") ;; share path
  (rime-user-data-dir "~/.emacs.d/rime") ;; real path used in Emacs rime
  (rime-cursor ".")
  (rime-show-candidate 'posframe) ;; use posframe
  (rime-commit1-forall t) ;; show the first choice
  (rime-posframe-properties
   (list :internal-border-width 4))
  (rime-posframe-style 'vertical)
  (mode-line-mule-info '((:eval (rime-lighter)))) ;; show rime symbol on modeline
  (rime-deactivate-when-exit-minibuffer t) ;; deactivate rime in minibuffer automatically
  )

(use-package mixed-pitch
  :hook (org-mode . mixed-pitch-mode)
  :config
  (set-face-attribute 'variable-pitch nil
                      :font "Iosevka")
  (setq fixed-pitch "FantasqueSansM Nerd Font Mono") ;; this should change by cnfonts.
  )

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

(provide 'init-ui)
;;; init-ui.el ends here
