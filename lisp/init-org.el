;;; init-org.el --- for org
;;; Commentary:
;;; Code:
;;; Org
(use-package org
  :bind
  (("C-x C-y" . org-insert-image)
   ("C-/" . org-latex-preview))
  :config
  ;; Fold
  (setq org-startup-folded 'content) ;; show titles only

  ;; Inline image
  (auto-image-file-mode t)
  (setq org-image-actual-width 500)

  ;; Babel
  (setq org-confirm-babel-evaluate nil)
  (setq org-plantuml-jar-path "~/Code/plantuml/plantuml-1.2024.3.jar")
  (setq org-babel-python-command "~/miniconda3/bin/python3")

  (require 'ob-C)
  (require 'ob-shell)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((python . t)
     (emacs-lisp . t)
     (plantuml . t)
     (scheme . t)
     (C . t)
     (shell . t)
     (latex . t)))

  ;; LaTeX
  (setq org-startup-with-latex-preview nil)
  (setq org-latex-default-class "ctexart") ;; latex class
  (setq org-latex-compiler "xelatex") ;; latex compiler
  (turn-on-cdlatex)
  (add-hook 'org-mode-hook (lambda () ;; cdlatex
			     (setq truncate-lines nil)
			     (org-cdlatex-mode)))

  :custom
  (org-pretty-entities t) ;; pretty entities in org
  (org-startup-indented t) ;; indent
  (org-highlight-latex-and-related '(latex entities)) ;; latex highlight

  ;;; Agenda
  (setq org-agenda-include-diary t)

  ;; Agenda style
  (org-agenda-use-time-grid t)

  (org-agenda-tags-column 0)
  (org-agenda-block-separator ?─)
  (org-agenda-current-time-string
   "⭠ now ─────────────────────────────────────────────────")
  ;;---------------------------------------------
  ;;org-agenda-time-grid
  ;;--------------------------------------------
  (org-agenda-time-grid (quote ((daily today require-timed)
                                (700
                                 1300
                                 1800
                                 2400)
                                "......"
                                "-----------------------------------------------------"))))

;;; Org UI
;; org-modern
(use-package org-modern
  :after org
  :hook (org-mode . org-modern-mode)
  :custom
  (org-modern-hide-stars nil)
  (org-modern-todo t)
  (org-modern-table nil)
  (org-modern-timestamp t)
  (org-modern-tag t)
  (org-modern-priority t)
  (org-modern-star 'replace)
  :config
  (setq org-modern-list '((43 . "◦")
			  (45 . "•")
			  (42 . "–")))
  (add-hook 'org-agenda-finalize-hook #'org-modern-agenda)
  ;; Add frame borders and window dividers
  (modify-all-frames-parameters
   '((right-divider-width . 5)
     (internal-border-width . 5)))
  (dolist (face '(window-divider
                  window-divider-first-pixel
                  window-divider-last-pixel))
    (face-spec-reset-face face)
    (set-face-foreground face (face-attribute 'default :background)))
  (set-face-background 'fringe (face-attribute 'default :background))
  (setq
   ;; Edit settings
   org-auto-align-tags t
   org-tags-column 0
   org-fold-catch-invisible-edits 'show-and-error
   org-special-ctrl-a/e t
   org-insert-heading-respect-content t
   ;; Org styling, hide markup etc.
   org-hide-emphasis-markers t
   org-ellipsis "…")
  ;; Org todo keywords
  (setq org-todo-keywords '((sequence "TODO" "DONE" "CANCELED")))
  (setq org-modern-todo-faces
	(quote (("TODO" :background "pink" :foreground "black")
		("DONE" :background "green" :foreground "black")
		("CANCELED" :background "grey" :foreground "black")))))

;; Org-appear, convenient for editing LaTeX formula
(use-package org-appear
  :after org
  :hook (org-mode . org-appear-mode)
  :custom
  (org-appear-autoemphasis t)
  (org-appear-autolinks t)
  (org-appear-autoentities t)
  (org-appear-autosubmarkers t) ;; submarkers
  (org-appear-inside-latex t) ;; latex
  (org-appear-autokeywords t))

;; Valign for table
(use-package valign
  :after org
  :hook (org-mode . valign-mode)
  :custom
  (valign-fancy-bar t))

;; View pdf images inline
(use-package org-inline-pdf
  :after org
  :hook (org-mode . org-inline-pdf-mode))

;;; Org notes and Literature management
;; Org roam
(use-package org-roam
  :after org
  :custom
  (org-roam-directory "~/org/roam-notes/") ;; default dir
  :bind
  (("C-c n f" . org-roam-node-find)
   ("C-c n i" . org-roam-node-insert)
   ("C-c n c" . org-roam-capture)
   ("C-c n l" . org-roam-buffer-toggle) ;; 显示后链窗口
   )
  :config
  (org-roam-db-autosync-mode) ;; auto sync when starting
  )

;; Zotero path
(setq zot_bib '("~/Nutstore Files/zotero/Papers.bib") ;; zotero reference bib
      zot_pdf "~/Nutstore Files/zotero" ;; zotero zotfile dir
      org_refs "~/org/roam-notes/ref" ;; org-roam + helm-bibtex + org-noter notes dir
      )

;; Helm-bibtex to read Zotero information
(use-package helm-bibtex
  :bind (("C-c h" . helm-bibtex))
  :custom
  (bibtex-completion-notes-path org_refs)
  (bibtex-completion-bibliography zot_bib)
  (bibtex-completion-library-path zot_pdf))

;; Org-roam-bibtex combined with helm-bibtex
(use-package org-roam-bibtex
  :hook (org-roam-mode . org-roam-bibtex-mode)
  :bind (("C-c n k" . orb-insert-link)
	 ("C-c n a" . orb-note-action))
  :custom
  (orb-insert-interface 'helm-bibtex)
  (orb-insert-link-description 'citekey)
  (orb-preformat-keywords
   '("citekey" "title" "author-or-editor" "keywords" "file"))
  (orb-process-file-keyword t)
  (orb-attached-file-extensions '("pdf")))

;; Org-ref
(use-package org-ref
  :after org
  :bind (("C-c (" . org-ref-insert-link))
  )

;;; Hugo
(use-package easy-hugo
  :bind ("C-c b" . easy-hugo)
  :config
  (setq easy-hugo-basedir "~/research/code/Grant/") ;; website root
  (setq easy-hugo-postdir "content/post/")
  (setq easy-hugo-url "https://Grant-S-Z.github.io/Grant") ;; url
  (setq easy-hugo-sshdomain "grant-s-z.github.io")
  (setq easy-hugo-previewtime "300")
  (setq easy-hugo-default-ext ".md"))

(use-package ox-hugo
  :config
  (setq org-hugo-base-dir "~/research/code/Grant/")
  (setq org-hugo-section "post"))

(provide 'init-org)
;;; init-org.el ends here
