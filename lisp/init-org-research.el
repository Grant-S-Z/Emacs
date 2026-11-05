;;; init-org-research.el --- Org notes, references and spaced repetition  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;;; Spaced repetition
(use-package org-drill
  :after org
  :commands (org-drill org-drill-directory org-drill-resume org-drill-cram)
  :bind (("C-c v d" . org-drill)
	 ("C-c v r" . org-drill-resume)
	 ("C-c v D" . org-drill-directory))
  :custom
  (org-drill-save-buffers-after-drill-sessions-p nil) ; don't save every buffer afterwards
  (org-drill-maximum-items-per-session 30)            ; at most 30 cards per session
  (org-drill-maximum-duration 20)                     ; at most 20 minutes per session
  (org-drill-learn-fraction 0.5)                      ; SM2 interval scaling factor
  (org-drill-leech-method 'warn))                     ; warn about cards that never stick

(use-package org-fc
  :ensure nil ; not on MELPA, loaded from site-lisp
  :load-path "~/.emacs.d/site-lisp/org-fc"
  :after org
  :commands (org-fc-review org-fc-dashboard)
  :bind (("C-c v f" . org-fc-review)
	 ("C-c v F" . org-fc-review-all))
  :custom
  (org-fc-directories '("~/org/fc/")) ; index only the vocabulary dir, not roam notes
  :config
  (require 'org-fc-hydra)
  (org-fc-cache-mode 1)) ; cache the index so thousands of cards load fast

;;; Notes and literature
(use-package org-roam
  :after org
  :custom
  (org-roam-directory "~/org/roam-notes/")
  :bind
  (("C-c n f" . org-roam-node-find)
   ("C-c n i" . org-roam-node-insert)
   ("C-c n c" . org-roam-capture)
   ("C-c n l" . org-roam-buffer-toggle))
  :config
  (org-roam-db-autosync-mode))

(use-package org-roam-ui)

(use-package citar
  :hook
  (LaTeX-mode . citar-capf-setup)
  (org-mode . citar-capf-setup)
  :bind
  (:map org-mode-map :package org ("C-c (" . #'org-cite-insert))
  :custom
  (org-cite-global-bibliography '("~/Nutstore Files/zotero/My Library.bib"))
  (org-cite-insert-processor 'citar)
  (org-cite-follow-processor 'citar)
  (org-cite-activate-processor 'citar)
  ;; Keep this a literal list, not `org-cite-global-bibliography': Custom
  ;; stores the value as a form and evaluates it when citar is loaded,
  ;; which can happen before Org (and thus `oc') is, giving
  ;; "void-variable org-cite-global-bibliography" during mode setup.
  (citar-bibliography '("~/Nutstore Files/zotero/My Library.bib"))
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

(provide 'init-org-research)
;;; init-org-research.el ends here
