;;; init-org-research.el --- Org notes, references and spaced repetition  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;;; Spaced repetition

;; Flashcards with review scheduling in Org files.
(use-package org-fc
  :vc (:url "https://git.sr.ht/~l3kn/org-fc")
  :after org
  :commands (org-fc-review org-fc-dashboard)
  :bind (("C-c v f" . org-fc-review)
	 ("C-c v F" . org-fc-review-all))
  :custom
  ;; The vocabulary deck lives under ~/org/.
  (org-fc-directories '("~/org/English/"))
  :config
  ;; No `org-fc-cache-mode': its empty-file `find -L' call breaks BSD find.
  ;; The bundled checkout ships `org-fc-type-vocab.el' but never loads it.
  (require 'org-fc-type-vocab)
  (require 'org-fc-hydra)
  (advice-add 'org-fc-set-header-line :override #'grant/org-fc-review-header-line)
  (dolist (map '(org-fc-review-flip-mode-map org-fc-review-rate-mode-map))
    (define-key (symbol-value map) (kbd "k") #'grant/org-fc-mark-known)))

;; org-fc has no "known" state, only `suspended'; keep `:known:' as the label
;; and retire the card so tag and scheduling agree.
(defun grant/org-fc-mark-known ()
  "Tag the card at point as known and stop scheduling it."
  (interactive)
  (org-fc-with-point-at-entry (org-toggle-tag "known" 'on))
  (if (bound-and-true-p org-fc-review--session)
      (org-fc-review-suspend-card)
    (org-fc-suspend-card)))

(defun grant/org-fc-unmark-known ()
  "Put a `:known:' card back into the review queue."
  (interactive)
  (org-fc-with-point-at-entry
   (org-toggle-tag "known" 'off)
   (org-toggle-tag org-fc-suspended-tag 'off)))

;; Replace org-fc's terse header with the keys for the active state; each
;; entry is a (mode-variable . string) pair re-evaluated on redisplay.
(defun grant/org-fc-review-header-line ()
  "Set the review header line to the keys of the current state."
  (let* ((session org-fc-review--session)
         (remaining (1+ (length (oref (oref session scheduler) positions))))
         (current (oref session current-item))
         (title (unless (or org-fc-review-hide-title-in-header-line
                            (member "notitle" (oref (oref current card) tags)))
                  (oref (oref (oref current card) file) title))))
    (setq org-fc-original-header-line-format header-line-format)
    (setq-local header-line-format
                `((org-fc-review-flip-mode
                   " Flip:  RET 翻面 / k 已掌握 / s 暂停 / p 编辑 / q 退出 ")
                  (org-fc-review-rate-mode
                   " Rate:  a again / h hard / g good / e easy / k 已掌握 / s 暂停 / p 编辑 / q 退出 ")
                  (org-fc-review-edit-mode
                   " Edit:  C-c C-c 继续 / C-c C-k 退出 ")
                  ,(format " (%d) " remaining)
                  ,title))))

;;; Notes and literature
;; Zettelkasten notes with backlinks and a database.
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

;; Citation completion and notes for Org and LaTeX.
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
  ;; Keep a literal list, not `org-cite-global-bibliography': Custom evaluates
  ;; that form when citar loads, possibly before Org (and `oc') exists.
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

;; Create Org-roam notes from Citar citations.
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

;;; Literature export
(with-eval-after-load 'org
  (defun grant/literature-export (&optional file)
    "Export PDF paths referenced by the current Org buffer or FILE.
Write the result to papers.json in the same directory."
    (interactive)
    (let ((source (or file (buffer-file-name))))
      (unless source (user-error "Buffer is not visiting a file"))
      (require 'org-element)
      (require 'json)
      (with-current-buffer (find-file-noselect source)
        (unless (derived-mode-p 'org-mode)
          (user-error "Not an Org buffer: %s" source))
        (let* ((keys
                (delete-dups
                 (delq nil
                       (org-element-map (org-element-parse-buffer)
                           'citation-reference
                         (lambda (ref)
                           (org-element-property :key ref))))))
               (citar-library-file-extensions '("pdf"))
               (files (progn (require 'citar)
                             (citar-get-files keys)))
               (result (make-hash-table :test 'equal))
               (output (expand-file-name
                        "papers.json"
                        (file-name-directory source))))
          (dolist (key keys)
            (puthash key
                     (or (and files (gethash key files)) [])
                     result))
          (with-temp-file output
            (insert (json-encode result) "\n"))
          (message "Exported %d references to %s"
                   (length keys) output))))))

(with-eval-after-load 'org
  (defun grant/literature-auto-export ()
    "Update the paper manifest when literature.org is saved."
    (when (and buffer-file-name
               (derived-mode-p 'org-mode)
               (equal (file-name-nondirectory buffer-file-name)
                      "literature.org"))
      (grant/literature-export buffer-file-name)))

  (add-hook 'after-save-hook #'grant/literature-auto-export))

(provide 'init-org-research)
;;; init-org-research.el ends here
