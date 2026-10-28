;;; init-org-latex.el --- Org LaTeX preview, Babel and export  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;;; LaTeX templates
(defconst grant/org-latex-template-directory
  (expand-file-name "tex" user-emacs-directory))

(defun grant/org-latex-read-template (filename)
  "Read Org LaTeX template FILENAME as a string."
  (with-temp-buffer
    (insert-file-contents
     (expand-file-name filename grant/org-latex-template-directory))
    (buffer-string)))

(defconst grant/org-latex-math-macros
  (grant/org-latex-read-template "org-math-macros.tex"))

(defun grant/org-latex-template (filename)
  "Read template FILENAME and expand the shared mathematics macros."
  (replace-regexp-in-string
   "%%ORG-MATH-MACROS%%" grant/org-latex-math-macros
   (grant/org-latex-read-template filename) t t))

(defconst grant/org-latex-section-structure
  '(("\\section{%s}" . "\\section*{%s}")
    ("\\subsection{%s}" . "\\subsection*{%s}")
    ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
    ("\\paragraph{%s}" . "\\paragraph*{%s}")
    ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

;;; Org LaTeX and Babel settings
(with-eval-after-load 'org
  (setq org-startup-with-latex-preview t)
  (setq org-latex-default-class "ctexart")
  (setq org-latex-compiler "lualatex")

  ;; Babel LaTeX output
  (require 'ob-latex)
  (setq org-babel-latex-preamble
	(lambda (_) "\\documentclass[tikz]{standalone}\n\\usetikzlibrary{arrows.meta,patterns}"))
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

  ;; Export and hyperref
  (setq org-latex-hyperref-template "
\\hypersetup{
pdfauthor={%a},
pdftitle={%t},
pdfkeywords={%k},
pdfsubject={%d},
pdfcreator={%c},
pdflang={%L},
colorlinks=true,
linkcolor=black
}
")
  (setq org-latex-prefer-user-labels t)
  (setq org-beamer-theme "metropolis")
  ;; 导出的 PDF 用 Emacs（pdf-tools）打开，不走系统默认（Skim）
  (add-to-list 'org-file-apps '("\\.pdf\\'" . emacs))
  (setq org-latex-pdf-process
	'("latexmk --lualatex -shell-escape -interaction=nonstopmode -output-directory=%o %f"))

  ;; Keep the previous effective class order: beamer-en, beamer, ctexart, article.
  (setq org-latex-classes nil)
  (dolist (class '("article" "ctexart" "beamer" "beamer-en"))
    (add-to-list 'org-latex-classes
                 (append
                  (list class
                        (grant/org-latex-template (format "org-%s.tex" class)))
                  grant/org-latex-section-structure))))

;;; Org LaTeX preview（Org main 原生异步实现）
(use-package org-latex-preview
  :ensure nil ;; 由 Org main 提供，不是独立 ELPA 包
  :after org
  :hook (org-mode . org-latex-preview-mode)
  :custom
  (org-latex-preview-process-default 'dvisvgm)
  (org-latex-preview-mode-display-live t)
  (org-latex-preview-mode-track-inserts t)
  (org-latex-preview-mode-update-delay 0.1)
  ;; LuaLaTeX/XeLaTeX 不支持新预览的头文件预编译。
  (org-latex-preview-process-precompile nil)
  (org-latex-preview-appearance-options
   '(:foreground auto :background "Transparent"
     :scale 1.6 :zoom 1.05 :page-width 0.6))
  (org-latex-preview-preamble
   (grant/org-latex-template "org-preview-preamble.tex")))

(provide 'init-org-latex)
;;; init-org-latex.el ends here
