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
  (org-drill-save-buffers-after-drill-sessions-p nil) ; 复习完不自动保存所有 buffer
  (org-drill-maximum-items-per-session 30)            ; 每次最多 30 张卡
  (org-drill-maximum-duration 20)                     ; 每次最多 20 分钟
  (org-drill-learn-fraction 0.5)                      ; SM2 间隔缩放因子
  (org-drill-leech-method 'warn))                     ; 总是记不住的卡给警告

(use-package org-fc
  :ensure nil ;; 不在 MELPA，从 site-lisp 手动加载
  :load-path "~/.emacs.d/site-lisp/org-fc"
  :after org
  :commands (org-fc-review org-fc-dashboard)
  :bind (("C-c v f" . org-fc-review)
	 ("C-c v F" . org-fc-review-all))
  :custom
  (org-fc-directories '("~/org/fc/")) ; 只索引词库目录，不扫 roam 笔记
  :config
  (require 'org-fc-hydra)
  (org-fc-cache-mode 1)) ; 缓存索引，千级卡片秒开

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

(provide 'init-org-research)
;;; init-org-research.el ends here
