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

;;; Useful highlights and colors
(use-package rainbow-delimiters ;; color of delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package indent-bars
  :hook (prog-mode . indent-bars-mode)
  :custom
  (indent-bars-no-descend-lists t))

;; Outli, unfold as org
(add-to-list 'load-path "~/.emacs.d/site-lisp/outli/")
(require 'outli)
(add-hook 'prog-mode-hook 'outli-mode)

;;; Fonts and input method
(use-package cnfonts
  :init (cnfonts-mode 1)
  :bind (("C--" . cnfonts-decrease-fontsize)
  ("C-=" . cnfonts-increase-fontsize))
  :custom
  (cnfonts-personal-fontnames '(("Fira Code" "Ligconsolata" "Fantasque Sans Mono" "FantasqueSansM Nerd Font Mono" "Iosevka")
                                ("LXGW WenKai Mono" "TsangerJinKai05")
                                ("PragmataPro Mono Liga")
                                ("PragmataPro Mono Liga"))))

(use-package mixed-pitch
  :hook (org-mode . mixed-pitch-mode)
  :config
  (set-face-attribute 'variable-pitch nil
                      :font "Iosevka")
  (setq fixed-pitch "Fantasque Sans Mono") ;; when cnfonts change, this should change as the same.
  )

(use-package posframe) ;; frame

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
