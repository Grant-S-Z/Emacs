;;; init-org-ui.el --- Org appearance  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;;; Org Modern
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
			  ;; (45 . "☄")
			  (45 . "✶")
			  (42 . "◦")))
  (setq org-modern-replace-stars "✺☽✦✧⋆")
  ;; (setq org-modern-replace-stars "⬢⬡❖⟡⋄")
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
;; Toggling formula source against its image is handled by org-latex-preview-mode.
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

(use-package valign
  :hook (org-mode . valign-mode)
  :custom
  (valign-fancy-bar nil)
  ;; Skip pixel alignment for very large tables: measuring cell widths
  ;; synchronously can stall opening big org files.
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

(use-package org-inline-pdf
  :after org
  :hook (org-mode . org-inline-pdf-mode))

(provide 'init-org-ui)
;;; init-org-ui.el ends here
