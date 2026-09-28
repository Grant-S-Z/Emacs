;;; init-org.el --- for org  -*- lexical-binding: t; -*-
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
  (setq org-image-actual-width 500)

  ;; Babel
  (setq org-confirm-babel-evaluate nil)
  (setq org-plantuml-jar-path "~/Code/plantuml/plantuml-1.2024.3.jar")
  (setq org-babel-python-command "~/miniconda3/bin/python3")

  (require 'ob-C)
  (require 'ob-shell)
  (require 'ob-latex)

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((python . t)
     (emacs-lisp . t)
     (plantuml . t)
     (scheme . t)
     (C . t)
     (shell . t)
     (latex . t)))

  (setq org-babel-latex-preamble (lambda (_) "\\documentclass[tikz]{standalone}\n\\usetikzlibrary{arrows.meta,patterns}"))
  (setq org-babel-latex-pdf-svg-process
	"dvisvgm %f --pdf --no-fonts --exact-bbox -o %O")
  (setq org-babel-latex-process-alist
	'((png :programs ("xelatex" "convert")
               :description "pdf > png"
               :message "you need to install the programs: xelatex and imagemagick."
               :image-input-type "pdf"
               :image-output-type "png"
               :image-size-adjust (1.0 . 1.0)
               :latex-compiler ("xelatex -interaction nonstopmode -output-directory %o %f")
               :image-converter ("convert -density %D -trim -antialias %f -quality 100 %O"))))


  ;; LaTeX
  (setq org-startup-with-latex-preview nil)
  (setq org-latex-default-class "ctexart") ;; latex class
  (setq org-latex-compiler "lualatex") ;; latex compiler
  (add-hook 'org-mode-hook (lambda () ;; cdlatex
			     (setq truncate-lines nil)
			     (org-cdlatex-mode)))

  ;; (add-to-list 'org-structure-template-alist
  ;;              '("th" . "theorem"))
  ;; (add-to-list 'org-structure-template-alist
  ;;              '("lm" . "lemma"))
  ;; (add-to-list 'org-structure-template-alist
  ;;              '("pf" . "proof"))

  ;; (defface grant/org-theorem
  ;;   '((t (:inherit org-block
  ;; 		   :background "#1e1e1e"
  ;; 		   :extend t)))
  ;;   "Face for theorem blocks")

  ;; (font-lock-add-keywords
  ;;  'org-mode
  ;;  '(("^#\\+begin_theorem" . 'grant/org-theorem)
  ;;    ("^#\\+end_theorem"   . 'grant/org-theorem)))

  ;; (defun grant/org-block-left-bar (limit)
  ;;   (when (re-search-forward "^#\\+begin_theorem" limit t)
  ;;     (let ((ov (make-overlay (line-beginning-position)
  ;;                             (line-end-position))))
  ;; 	(overlay-put ov 'before-string
  ;;                    (propertize "│ " 'face 'shadow)))))

  ;; (add-hook 'org-mode-hook
  ;;           (lambda ()
  ;;             (jit-lock-register #'grant/org-block-left-bar)))

  :custom
  ;; Prettify
  (org-pretty-entities t) ;; pretty entities in org
  (org-startup-indented t) ;; indent
  (org-highlight-latex-and-related '(latex entities)) ;; latex highlight

  ;; Agenda
  (org-agenda-include-diary t)

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
  (setq org-modern-block-name
	'(("src" . ("λ" ""))		;∎
	  ("quote" . ("❝" ""))		;❞
	  ("example" . ("⊢" ""))	;⊣
	  (t . t)))
  (setq org-modern-keyword
	'(("title"  . "𝒯")
	  ("subtitle" . "")
	  ("author" . "✍")
	  ("date"   . "◷")
          ("name" . "↪")
          ("caption" . "§")
          ("results" . "⟾")
	  ("attr_latex" . "ℒ")
	  (t . t)))



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
  (valign-fancy-bar nil))

;; View pdf images inline
(use-package org-inline-pdf
  :after org
  :hook (org-mode . org-inline-pdf-mode))

;; (use-package org-sliced-images
;;   :hook (org-mode . org-sliced-images-mode))

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

(use-package org-roam-ui)

(use-package citar
  :hook
  (LaTeX-mode . citar-capf-setup)
  (org-mode . citar-capf-setup)
  :bind
  (:map org-mode-map :package org ("C-c (" . #'org-cite-insert))
  :custom
  (org-cite-global-bibliography '("~/Nutstore Files/zotero/Papers.bib"))
  (org-cite-insert-processor 'citar)
  (org-cite-follow-processor 'citar)
  (org-cite-activate-processor 'citar)
  (citar-bibliography org-cite-global-bibliography)
  (citar-notes-paths '("~/org/roam-notes/citar-notes/"))
  :config
  (defvar citar-indicator-notes-icons
    (citar-indicator-create
     :symbol (nerd-icons-mdicon
              "nf-md-notebook"
              :face 'nerd-icons-blue
              :v-adjust -0.3)
     :function #'citar-has-notes
     :padding "  "
     :tag "has:notes"))

  (defvar citar-indicator-links-icons
    (citar-indicator-create
     :symbol (nerd-icons-octicon
              "nf-oct-link"
              :face 'nerd-icons-orange
              :v-adjust -0.1)
     :function #'citar-has-links
     :padding "  "
     :tag "has:links"))

  (defvar citar-indicator-files-icons
    (citar-indicator-create
     :symbol (nerd-icons-faicon
              "nf-fa-file"
              :face 'nerd-icons-green
              :v-adjust -0.1)
     :function #'citar-has-files
     :padding "  "
     :tag "has:files"))

  (setq citar-indicators
	(list citar-indicator-files-icons
	      citar-indicator-links-icons
              citar-indicator-notes-icons)))

(use-package citar-org-roam
  :ensure t
  :after (citar org-roam)
  :config
  (citar-org-roam-mode 1)
  (setq citar-org-roam-note-title-template "${author} - ${title}")
  (setq org-roam-capture-templates
        '(("d" "default" plain "%?"
           :target (file+head "reference/${citekey}.org"
                              "#+title: ${author} - ${title}\n#+filetags: :article:\n\n* Abstract\n\n%?\n\n* Notes\n")
           :unnarrowed t))))

;; ;; Zotero path
;; (setq zot_bib "~/Nutstore Files/zotero/Papers.bib" ;; zotero reference bib
;;       zot_pdf "~/Nutstore Files/zotero/" ;; zotero zotfile dir
;;       )

;; ;; Helm-bibtex to read Zotero information
;; (use-package helm-bibtex
;;   :bind (("C-c h" . helm-bibtex))
;;   :custom
;;   (bibtex-completion-bibliography zot_bib)
;;   (bibtex-completion-library-path zot_pdf))

;; ;; Org-roam-bibtex combined with helm-bibtex
;; (use-package org-roam-bibtex
;;   :hook (org-roam-mode . org-roam-bibtex-mode)
;;   :bind ("C-c n a" . orb-note-action)
;;   :custom
;;   (orb-insert-interface 'helm-bibtex)
;;   (orb-insert-link-description 'citekey)
;;   (orb-preformat-keywords
;;    '("citekey" "title" "author-or-editor" "keywords" "file"))
;;   (orb-process-file-keyword t)
;;   (orb-attached-file-extensions '("pdf")))

;; ;; Org-ref
;; (use-package org-ref
;;   :after org
;;   :demand t ;; ensure that it loads so that links work immediately
;;   :bind (("C-c (" . org-ref-insert-link))
;;   )

;;; Org Present
;; (use-package org-tree-slide
;;   :ensure t
;;   :config
;;   (setq org-tree-slide-header nil)
;;   (setq org-tree-slide-slide-in-effect t))

(use-package org-tree-slide
  :after org
  :commands (+org-slide-start +org-slide-stop)
  :config
  (defun +hide-tab-bar ()
    "Hide the tab bar (used when starting a presentation)."
    (when (bound-and-true-p tab-bar-mode)
      (tab-bar-mode -1)))

  (defun +show-tab-bar ()
    "Show the tab bar again (used when stopping a presentation)."
    (unless (bound-and-true-p tab-bar-mode)
      (tab-bar-mode 1)))

  (defun +org-slide-start ()
    (interactive)

    (when (eq major-mode 'org-mode)
      ;; hide emephasis marks
      (setq org-hide-emphasis-markers t)
      ;; restart emacs to apply new settings above
      (org-mode-restart)

      ;; set faces for better presentation
      (set-face-attribute 'org-meta-line nil :foreground (face-attribute 'default :background))
      (set-face-attribute 'org-tree-slide-header-overlay-face nil :foreground (face-attribute 'default :background))
      (set-face-attribute 'org-block-begin-line nil :background (face-attribute 'default :background))
      (set-face-attribute 'org-block-end-line nil :background (face-attribute 'default :background))
      (set-face-attribute 'org-block-begin-line nil :foreground (face-attribute 'default :background))
      (set-face-attribute 'org-block-end-line nil :foreground (face-attribute 'default :background))
      (set-face-attribute 'org-quote nil :foreground (face-attribute 'default :foreground))

      ;; the following settings must be set after restarting org-mode
      (setq-local header-line-format nil
                  mode-line-format nil
                  line-spacing 10)
      (+hide-tab-bar)
      (text-scale-increase 3)
      (visual-line-mode)
      (show-paren-local-mode -1)
      (hl-line-mode -1)
      (org-tree-slide-mode)
      ))

  (defun +org-slide-stop ()
    (interactive)

    (when (eq major-mode 'org-mode)
      ;; reset
      (setq org-hide-emphasis-markers nil)
      (set-face-attribute 'org-meta-line nil :foreground nil)
      (set-face-attribute 'org-tree-slide-header-overlay-face nil :foreground nil)
      (set-face-attribute 'org-block-begin-line nil :background nil)
      (set-face-attribute 'org-block-end-line nil :background nil)
      (set-face-attribute 'org-block-begin-line nil :foreground nil)
      (set-face-attribute 'org-block-end-line nil :foreground nil)
      (set-face-attribute 'org-quote nil :foreground nil)
      (+show-tab-bar)

      (org-tree-slide-mode -1)

      ;; `org-mode-restart' will clear all local variables,
      ;; so there is no need to reset them manually
      (org-mode-restart)
      )
    )
  (setq org-tree-slide-heading-emphasis t
        org-tree-slide-content-margin-top 1
        org-tree-slide-slide-in-effect nil)
  )

;;; Hugo
(use-package easy-hugo
  :bind ("C-c b" . easy-hugo)
  :config
  (setq easy-hugo-basedir "~/research/code/Grant/") ;; website root
  (setq easy-hugo-postdir "content/org/")
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
