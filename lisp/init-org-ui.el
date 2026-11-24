;;; init-org-ui.el --- Org appearance  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;;; Org Modern
;; Unicode bullets, tags, and keyword faces.
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
  (setq org-modern-list '((43 . "•")
			  (45 . "✶")
			  (42 . "◦")))
  (setq org-modern-replace-stars "✺☽✦✧⋆")
  (setq org-modern-block-name
	'(("src" . ("λ" ""))
	  ("quote" . ("❝" ""))
	  ("example" . ("⊢" ""))
	  (t . t)))
  (setq org-modern-keyword
	'(("title"  . "𝒯")
	  ("subtitle" . "𝒮")
	  ("author" . "✍")
	  ("date"   . "◷")
          ("name"   . "↪")
          ("caption" . "§")
          ("results" . "⟾")
	  ("attr_latex" . "ℒ")
	  (t . t)))
  (add-hook 'org-agenda-finalize-hook #'org-modern-agenda)
  (setq org-modern-todo-faces
	(quote (("TODO" :background "pink" :foreground "black")
		("DONE" :background "green" :foreground "black")
		("CANCELED" :background "grey" :foreground "black")))))

;;; Source visibility and inline content
;; Reveal Org markup only at point; formula previews are handled by
;; `org-latex-preview-mode'.
(use-package org-appear
  :after org
  :hook (org-mode . org-appear-mode)
  :custom
  (org-appear-autoemphasis t)
  (org-appear-autolinks t)
  (org-appear-autoentities t)
  (org-appear-autosubmarkers t)
  (org-appear-autokeywords t)
  (org-appear-inside-latex t))

;; Skip valign in table-heavy buffers; measuring 1500+ tables in jit-lock
;; stalls words.org.  Define this before valign loads, not in its :config.
(defcustom grant/valign-max-tables 100
  "Turn off `valign-mode' in Org buffers with more tables than this.
Table separator lines are counted, one per table.  Nil disables the check."
  :type '(choice (const :tag "No limit" nil) integer)
  :group 'valign)

(defun grant/valign--too-many-tables-p (limit)
  "Return non-nil when the current buffer has more than LIMIT Org tables."
  (save-excursion
    (goto-char (point-min))
    (let ((count 0))
      (while (and (<= count limit)
                  (re-search-forward "^[ \t]*|[-+]" nil t))
        (setq count (1+ count)))
      (> count limit))))

(defun grant/valign-disable-in-table-heavy-buffer ()
  "Disable `valign-mode' when the current buffer has too many tables.
The threshold is `grant/valign-max-tables'."
  (when (and (bound-and-true-p valign-mode)
             grant/valign-max-tables
             (grant/valign--too-many-tables-p grant/valign-max-tables))
    (valign-mode -1)))

;; Appended so this runs after `valign-mode', which `:hook' prepends.
(add-hook 'org-mode-hook #'grant/valign-disable-in-table-heavy-buffer 'append)

;; Pixel-align Org table columns.
(use-package valign
  :hook (org-mode . valign-mode)
  :custom
  (valign-fancy-bar nil)
  ;; Skip pixel alignment for very large tables; measuring cells can stall.
  (valign-max-table-size 2000)
  :config
  (defun grant/org-shrink-tables-with-cookies (beg end)
    "Shrink every Org table between BEG and END that carries width cookies."
    (save-excursion
      (goto-char beg)
      (while (re-search-forward "^[ \t]*|" end t)
        (if (org-at-table-p)
            (let ((table-beg (org-table-begin))
                  (table-end (org-table-end)))
              (when (save-excursion
                      (goto-char table-beg)
                      (re-search-forward
                       "|[ \t]*<[lrc]?[0-9]+>[ \t]*\\(|\\|$\\)"
                       table-end t))
                (org-table-shrink table-beg table-end))
              (goto-char table-end))
          (end-of-line)))))

  (defun grant/valign-after-org-cycle (state)
    "Shrink cookie-sized tables after Org reveals content, then refit valign."
    (when (bound-and-true-p valign-mode)
      (let ((region
             (pcase state
               ('children
                (cons (point)
                      (save-excursion (outline-next-heading) (point))))
               ('subtree
                (cons (point)
                      (save-excursion (org-end-of-subtree t t) (point))))
               ('all
                (cons (point-min) (point-max))))))
        (when region
          (grant/org-shrink-tables-with-cookies (car region) (cdr region))
          (jit-lock-refontify (car region) (cdr region))))))

  (add-hook 'org-cycle-hook #'grant/valign-after-org-cycle))

;; Preview PDF links inline in Org buffers.
(use-package org-inline-pdf
  :after org
  :hook (org-mode . org-inline-pdf-mode))

(provide 'init-org-ui)
;;; init-org-ui.el ends here
