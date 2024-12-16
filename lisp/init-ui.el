;;; init-ui.el -- themes of emacs
;;; Commentary:
;;; Code:
;;; Themes
(add-to-list 'load-path "~/.emacs.d/site-lisp/moe-theme.el")
(require 'moe-theme)
(load-theme 'moe-light t)

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
  (setq doom-modeline-time t)
  (setq doom-modeline-icon t)
  (setq doom-modeline-github nil)
  (setq doom-modeline-battery nil)
  (setq doom-modeline-buffer-file-name-style 'auto)
  (setq doom-modeline--eglot t))

;;; Minibuffer
(use-package vertico
  :init (vertico-mode)
  :custom
  (vertico-count 15))

(use-package vertico-posframe
  :init (vertico-posframe-mode)
  :after vertico)

(use-package savehist ;; persist history over restarting Emacs, and vertico sorts by history position.
  :after vertico
  :init (savehist-mode))

(use-package orderless ;; optionally use the orderless completion style.
  :after vertico
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles basic partial-completion)))))

(use-package marginalia
  :after vertico
  :init (marginalia-mode t))

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
        show-paren-delay 0.2)
  )

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
  :hook (prog-mode . indent-bars-mode)
  :custom
  (indent-bars-no-descend-lists t))

;; Outli, unfold codes as org
(add-to-list 'load-path "~/.emacs.d/site-lisp/outli/")
(require 'outli)
(add-hook 'prog-mode-hook 'outli-mode)

(use-package hl-todo ;; highlight keywords when coding and jump
  :hook (prog-mode . hl-todo-mode))

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

(use-package mixed-pitch
  :hook (org-mode . mixed-pitch-mode)
  :config
  (set-face-attribute 'variable-pitch nil
                      :font "Iosevka")
  (setq fixed-pitch "FantasqueSansM Nerd Font Mono") ;; this should change by cnfonts.
  )

(use-package posframe)

;; Input method
(use-package rime
  :custom
  (default-input-method "rime")
  (rime-librime-root "~/.emacs.d/librime/dist") ;; librime path
  (rime-emacs-module-header-root "/opt/homebrew/cellar/emacs/29.4_1/include") ;; emacs include path
  (rime-share-data-dir "~/Library/Rime") ;; share path
  (rime-user-data-dir "~/.emacs.d/rime") ;; real path used in Emacs rime
  (rime-cursor ".")
  (rime-show-candidate 'posframe) ;; use posframe
  (rime-commit1-forall t) ;; show the first choice
  (rime-posframe-properties
   (list :internal-border-width 4 ;; posframe internal border width
         ;; :font "TsangerJinKai05"
   ))
  (rime-posframe-style 'vertical)
  (mode-line-mule-info '((:eval (rime-lighter)))) ;; show rime symbol on modeline
  (rime-deactivate-when-exit-minibuffer t) ;; deactivate rime in minibuffer automatically
  )

(provide 'init-ui)
;;; init-ui.el ends here
