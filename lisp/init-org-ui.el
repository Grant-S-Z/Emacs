;;; init-org-ui.el --- Org appearance and presentation  -*- lexical-binding: t; -*-
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
  (setq org-modern-list '((43 . "◦")
			  (45 . "•")
			  (42 . "–")))
  (setq org-modern-block-name
	'(("src" . ("λ" ""))
	  ("quote" . ("❝" ""))
	  ("example" . ("⊢" ""))
	  (t . t)))
  (setq org-modern-keyword
	'(("title"  . "𝒯")
	  ("subtitle" . "")
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
;; 公式源码与图片的切换由 org-latex-preview-mode 负责。
(use-package org-appear
  :after org
  :hook (org-mode . org-appear-mode)
  :custom
  (org-appear-autoemphasis t)
  (org-appear-autolinks t)
  (org-appear-autoentities t)
  (org-appear-autosubmarkers t)
  (org-appear-autokeywords t))

(use-package valign
  :hook (org-mode . valign-mode)
  :custom
  (valign-fancy-bar t)
  :config
  (defun grant/org-shrink-tables-with-cookies (beg end)
    "收缩 BEG 和 END 之间所有带列宽标记的 Org 表格。"
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
    "Org 展开内容后，收缩带列宽标记的表格并刷新 valign 对齐。"
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

;;; Presentations
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
      (setq org-hide-emphasis-markers t)
      (org-mode-restart)

      (set-face-attribute 'org-meta-line nil :foreground (face-attribute 'default :background))
      (set-face-attribute 'org-tree-slide-header-overlay-face nil :foreground (face-attribute 'default :background))
      (set-face-attribute 'org-block-begin-line nil :background (face-attribute 'default :background))
      (set-face-attribute 'org-block-end-line nil :background (face-attribute 'default :background))
      (set-face-attribute 'org-block-begin-line nil :foreground (face-attribute 'default :background))
      (set-face-attribute 'org-block-end-line nil :foreground (face-attribute 'default :background))
      (set-face-attribute 'org-quote nil :foreground (face-attribute 'default :foreground))

      (setq-local header-line-format nil
                  mode-line-format nil
                  line-spacing 10)
      (+hide-tab-bar)
      (text-scale-increase 3)
      (visual-line-mode)
      (show-paren-local-mode -1)
      (hl-line-mode -1)
      (org-tree-slide-mode)))

  (defun +org-slide-stop ()
    (interactive)
    (when (eq major-mode 'org-mode)
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
      (org-mode-restart)))

  (setq org-tree-slide-heading-emphasis t
        org-tree-slide-content-margin-top 1
        org-tree-slide-slide-in-effect nil))

(provide 'init-org-ui)
;;; init-org-ui.el ends here
