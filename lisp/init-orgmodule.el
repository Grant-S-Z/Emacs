;;; init-orgmodule.el --- org latex modules and task templates  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
;;; Org exports to LaTeX settings
(with-eval-after-load 'org
  ;; org-latex-hyperref-template
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
  ;; Beamer
  (setq org-latex-prefer-user-labels t)
  (setq org-beamer-theme "metropolis")

  ;; babel output
  ;; (setq org-latex-listings 'minted)
  (setq org-latex-pdf-process '("latexmk --lualatex -shell-escape -interaction=nonstopmode -output-directory=%o %f")) ;; add "-shell-escape" for minted
  ;; (setq org-latex-pdf-process '("latexmk --xelatex -shell-escape -interaction=nonstopmode -output-directory=%o %f")) ;; add "-shell-escape" for minted

  ;; latex classes
  (setq org-latex-classes '(("article" "
\\documentclass[11pt]{article}
\% fonts
\\usepackage{fontspec}
\\setmainfont{Times New Roman}
\\setmonofont{Ligconsolata}

\\usepackage{amsfonts}
\\usepackage{amsthm}
\\usepackage{bm}
\\usepackage{siunitx}
\\usepackage[version=4]{mhchem}
\\usepackage{xcolor}

\\usepackage{cite}
\\usepackage{booktabs}
\\usepackage{graphicx}
\\usepackage{subfigure}

\\usepackage[margin=1in]{geometry}
\\geometry{a4paper}

\\usepackage{mathrsfs}
\% commands
\\newcommand{\\mr}[1]{\\mathrm{#1}}
\\newcommand{\\mb}[1]{\\mathbf{#1}}
\\newcommand{\\mc}[1]{\\mathcal{#1}}
\\newcommand{\\ms}[1]{\\mathscr{#1}}
\\renewcommand{\\d}{\\mathrm{d}}          % 微分算子（正体 d）
\\newcommand{\\kb}{k_{\\mathrm{B}}}        % 玻尔兹曼常数
\\newcommand{\\zpart}{\\mathcal{Z}}        % 巨正则配分函数
"

			     ("\\section{%s}" . "\\section*{%s}")
			     ("\\subsection{%s}" . "\\subsection*{%s}")
			     ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
			     ("\\paragraph{%s}" . "\\paragraph*{%s}")
			     ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))))


  (add-to-list 'org-latex-classes '("ctexart" "
\\documentclass[UTF8, a4paper, 11pt, fontset=none]{ctexart}

\% fonts
\\usepackage{fontspec}
\\setmainfont{Times New Roman}
\\setmonofont{Ligconsolata}
\\setCJKmainfont{SimSong}
\\setCJKmonofont{SimSong}

\\usepackage{amsfonts}
\\usepackage{amsthm}
\\usepackage{amssymb}
\\usepackage{bm}
\\usepackage{siunitx}
\\usepackage{xcolor}
\\usepackage[version=4]{mhchem}

\\usepackage{cite}
\\usepackage{booktabs}
\\usepackage{graphicx}
\\usepackage{subfigure}

\\usepackage[margin=1in]{geometry}
\\geometry{a4paper}

\\usepackage{mathrsfs}

\\usepackage{slashed}
\\usepackage{cancel}

\\usepackage{tikz}
\%commands
\\newcommand{\\mr}[1]{\\mathrm{#1}}
\\newcommand{\\mb}[1]{\\mathbf{#1}}
\\newcommand{\\mc}[1]{\\mathcal{#1}}
\\newcommand{\\ms}[1]{\\mathscr{#1}}
\\renewcommand{\\d}{\\mathrm{d}}          % 微分算子（正体 d）
\\newcommand{\\kb}{k_{\\mathrm{B}}}        % 玻尔兹曼常数
\\newcommand{\\zpart}{\\mathcal{Z}}        % 巨正则配分函数
"

				    ("\\section{%s}" . "\\section*{%s}")
				    ("\\subsection{%s}" . "\\subsection*{%s}")
				    ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
				    ("\\paragraph{%s}" . "\\paragraph*{%s}")
				    ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))) ;error "Non-hex character used for Unicode escape: s (115)"

  ;; beamer
  (add-to-list 'org-latex-classes '("beamer" "
\\documentclass[aspectratio=169,10pt]{ctexbeamer}
\\usetheme[block=fill, progressbar=frametitle]{metropolis}
\% fonts
\\usepackage{fontspec}
\\setmainfont{Times New Roman}
\\setsansfont{Times New Roman}
\\setmonofont{Fantasque Sans Mono}
\\setCJKmainfont{SimSun}
\\setCJKsansfont{Kai}
\\setCJKmonofont{LXGW WenKai Mono}

\\usepackage{amsfonts}
\\usepackage{amssymb}
\\usepackage{amsthm}
\\usepackage{bm}
\\usepackage{esint}
\\usepackage{siunitx}
\\usepackage{xcolor}
\\usepackage[version=4]{mhchem}
\% usepackage{minted}
\% setminted{frame=lines, framesep=2mm, baselinestretch=1.2, fontsize=\\small}

\\usepackage{tikz}

\\usepackage{slashed}
\\usepackage{cancel}

\\usepackage{unicode-math}
\\setmathfont{STIX Two Math}
\% keep unicode-math symbols out of PDF bookmarks (hyperref)
\\AtBeginDocument{\\pdfstringdefDisableCommands{%
\\def\\nu{ν}\\def\\mu{μ}\\def\\tau{τ}\\def\\beta{β}\\def\\theta{θ}%
\\def\\Gamma{Γ}\\def\\Phi{Φ}\\def\\Omega{Ω}\\def\\ce#1{#1}}}
\% commands
\\newcommand{\\mr}[1]{\\mathrm{#1}}
\\newcommand{\\mb}[1]{\\mathbf{#1}}
\\newcommand{\\mc}[1]{\\mathcal{#1}}
\\newcommand{\\ms}[1]{\\mathscr{#1}}
\\renewcommand{\\d}{\\mathrm{d}}          % 微分算子（正体 d）
\\newcommand{\\kb}{k_{\\mathrm{B}}}        % 玻尔兹曼常数
\\newcommand{\\zpart}{\\mathcal{Z}}        % 巨正则配分函数
"

				    ("\\section{%s}" . "\\section*{%s}")
				    ("\\subsection{%s}" . "\\subsection*{%s}")
				    ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
				    ("\\paragraph{%s}" . "\\paragraph*{%s}")
				    ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

  (add-to-list 'org-latex-classes '("beamer-en" "
\\documentclass[aspectratio=1610, 10pt]{beamer}
\\usetheme[block=fill, progressbar=frametitle]{metropolis}
\% fonts
\\usepackage{fontspec}
\\setmainfont{STIX Two Text}
\\setsansfont{STIX Two Text}
\\setmonofont{Fantasque Sans Mono}

\\usepackage{amsfonts}
\\usepackage{amsthm}
\\usepackage{amssymb}
\\usepackage{amsmath}
\\usepackage{bm}
\\usepackage{siunitx}
\\usepackage{xcolor}
\\usepackage[version=4]{mhchem}

\\usepackage{tikz}

\\usepackage{slashed}
\\usepackage{cancel}

\\usepackage{unicode-math}
\\setmathfont{STIX Two Math}
\% keep unicode-math symbols out of PDF bookmarks (hyperref)
\\AtBeginDocument{\\pdfstringdefDisableCommands{%
\\def\\nu{ν}\\def\\mu{μ}\\def\\tau{τ}\\def\\beta{β}\\def\\theta{θ}%
\\def\\Gamma{Γ}\\def\\Phi{Φ}\\def\\Omega{Ω}\\def\\ce#1{#1}}}
\% commands
\\newcommand{\\mr}[1]{\\mathrm{#1}}
\\newcommand{\\mb}[1]{\\mathbf{#1}}
\\newcommand{\\mc}[1]{\\mathcal{#1}}
\\newcommand{\\ms}[1]{\\mathscr{#1}}
\\renewcommand{\\d}{\\mathrm{d}}          % 微分算子（正体 d）
\\newcommand{\\kb}{k_{\\mathrm{B}}}        % 玻尔兹曼常数
\\newcommand{\\zpart}{\\mathcal{Z}}        % 巨正则配分函数
"

				    ("\\section{%s}" . "\\section*{%s}")
				    ("\\subsection{%s}" . "\\subsection*{%s}")
				    ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
				    ("\\paragraph{%s}" . "\\paragraph*{%s}")
				    ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
  )

;;; Org latex preview settings
;; (use-package org-xlatex ;; real-time preview
;;   :hook (org-mode . org-xlatex-mode))

(with-eval-after-load 'org
  ;; Process
  (setq org-preview-latex-default-process 'dvisvgm)

  ;; Format
  (setq org-format-latex-options
	(list :foreground 'default
              :background 'default
              :scale 1.6
              :matchers '("begin" "$1" "$" "$$" "\\(" "\\[")))

  (setq org-preview-latex-process-alist
	'((dvisvgm :programs ("xelatex" "dvisvgm")
                   :description "xdv > svg"
                   :message "XeLaTeX and dvisvgm process..."
                   :image-input-type "xdv"
                   :image-output-type "svg"
                   :image-size-adjust (1.7 . 1.5)
                   :latex-compiler ("xelatex -no-pdf -interaction nonstopmode -output-directory %o %f")
                   :image-converter ("dvisvgm %f -n -b min -c %S -o %O"))))
  )

;;   ;; (setq org-preview-latex-process-alist
;;   ;; 	'((dvisvgm :programs ("lualatex" "dvisvgm") :description "pdf > svg"
;;   ;; 		   :message
;;   ;; 		   "you need to install the programs: lualatex and dvisvgm."
;;   ;; 		   :image-input-type "pdf" :image-output-type "svg"
;;   ;; 		   :image-size-adjust (1.7 . 1.5) :latex-compiler
;;   ;; 		   ("lualatex -interaction nonstopmode -output-directory %o %f")
;;   ;; 		   :image-converter
;;   ;; 		   ("dvisvgm %f --pdf --no-fonts --exact-bbox --scale=%S --output=%O"))))

;;   ;; (setq org-preview-latex-process-alist
;;   ;; 	'((dvisvgm :programs ("latex" "dvisvgm") :description "dvi > svg"
;;   ;; 		   :message
;;   ;; 		   "you need to install the programs: latex and dvisvgm."
;;   ;; 		   :image-input-type "dvi" :image-output-type "svg"
;;   ;; 		   :image-size-adjust (1.7 . 1.5) :latex-compiler
;;   ;; 		   ("latex -interaction nonstopmode -output-directory %o %f")
;;   ;; 		   :image-converter
;;   ;; 		   ("dvisvgm %f --no-fonts --exact-bbox --scale=%S --output=%O"))))

;; Header
;; \\usepackage{newtxtext,newtxmath}
;; \\setmainfont{STIX Two Text} \\setmathfont{STIX Two Math}
(setq org-format-latex-header "\\documentclass[preview]{standalone}
\\usepackage[usenames]{color}
\\usepackage{amsmath}
\\usepackage{amsfonts}
\\usepackage{unicode-math}
\\setmainfont{Libertinus Serif}
\\setmathfont{Libertinus Math}
\\usepackage{siunitx}
\\usepackage[version=4]{mhchem}
\\usepackage{tikz}
\\usepackage{tikz-feynman}
\\pagestyle{empty}             % do not remove
% unicode-math 下 bm/mathrsfs 会引发 \"Extended mathchar\" 冲突，
% 用 \\symbf 替代 \\bm，\\mathscr 由 unicode-math 原生提供
\\providecommand{\\bm}[1]{\\symbf{#1}}
% New commands
\\newcommand{\\mr}[1]{\\mathrm{#1}}
\\newcommand{\\mb}[1]{\\mathbf{#1}}
\\newcommand{\\mc}[1]{\\mathcal{#1}}
\\newcommand{\\ms}[1]{\\mathscr{#1}}
\\renewcommand{\\d}{\\mathrm{d}}          % 微分算子（正体 d）
\\newcommand{\\kb}{k_{\\mathrm{B}}}        % 玻尔兹曼常数
\\newcommand{\\zpart}{\\mathcal{Z}}        % 巨正则配分函数")

;;   (setq org-latex-default-packages-alist
;; 	'(("" "amsmath" t ("lualatex" "xetex"))
;; 	  ("" "fontspec" t ("lualatex" "xetex"))
;; 	  ("" "graphicx" t) ("" "longtable" nil) ("" "wrapfig" nil)
;; 	  ("" "rotating" nil) ("normalem" "ulem" t)
;; 	  ("" "amsmath" t ("pdflatex")) ("" "amssymb" t ("pdflatex"))
;; 	  ("" "capt-of" nil) ("" "hyperref" nil)))
;;   ;; Center vertically
;;   ;; (defun grant/org-latex-preview-advice (beg end &rest _args)
;;   ;;   (let* ((ov (car (overlays-in beg end)))
;;   ;;          (img (cdr (overlay-get ov 'display)))
;;   ;;          (new-img (plist-put img :ascent 90)))
;;   ;;     (overlay-put ov 'display (cons 'image new-img))))
;;   ;; (advice-add 'org--make-preview-overlay
;;   ;;             :after #'grant/org-latex-preview-advice)

;;   ;; from: https://kitchingroup.cheme.cmu.edu/blog/2016/11/06/
;;   ;; Justifying LaTeX preview fragments in org
;;   ;; (plist-put org-format-latex-options :justify 'center)

;;   ;; (defun eli/org-justify-fragment-overlay (beg end image imagetype)
;;   ;;   (let* ((position (plist-get org-format-latex-options :justify))
;;   ;;          (img (create-image image 'svg t))
;;   ;;          (ov (car (overlays-at (/ (+ beg end) 2) t)))
;;   ;;          (width (car (image-display-size (overlay-get ov 'display))))
;;   ;;          offset)
;;   ;;     (cond
;;   ;;      ((and (eq 'center position)
;;   ;;            (= beg (line-beginning-position)))
;;   ;; 	(setq offset (floor (- (/ fill-column 2)
;;   ;;                              (/ width 2))))
;;   ;; 	(if (< offset 0)
;;   ;;           (setq offset 0))
;;   ;; 	(overlay-put ov 'before-string (make-string offset ? )))
;;   ;;      ((and (eq 'right position)
;;   ;;            (= beg (line-beginning-position)))
;;   ;; 	(setq offset (floor (- fill-column
;;   ;;                              width)))
;;   ;; 	(if (< offset 0)
;;   ;;           (setq offset 0))
;;   ;; 	(overlay-put ov 'before-string (make-string offset ? ))))))
;;   ;; (advice-add 'org--make-preview-overlay
;;   ;;             :after 'eli/org-justify-fragment-overlay)

;;   ;; from: https://kitchingroup.cheme.cmu.edu/blog/2016/11/07/
;;   ;; Better-equation-numbering-in-LaTeX-fragments-in-org-mode/
;;   ;; (defun org-renumber-environment (orig-func &rest args)
;;   ;;   (let ((results '())
;;   ;;         (counter -1)
;;   ;;         (numberp))
;;   ;;     (setq results (cl-loop for (begin .  env) in
;;   ;;                            (org-element-map (org-element-parse-buffer)
;;   ;; 				 'latex-environment
;;   ;;                              (lambda (env)
;;   ;; 				 (cons
;;   ;;                                 (org-element-property :begin env)
;;   ;;                                 (org-element-property :value env))))
;;   ;;                            collect
;;   ;;                            (cond
;;   ;;                             ((and (string-match "\\\\begin{equation}" env)
;;   ;;                                   (not (string-match "\\\\tag{" env)))
;;   ;;                              (cl-incf counter)
;;   ;;                              (cons begin counter))
;;   ;;                             ((and (string-match "\\\\begin{align}" env)
;;   ;;                                   (string-match "\\\\notag" env))
;;   ;;                              (cl-incf counter)
;;   ;;                              (cons begin counter))
;;   ;;                             ((string-match "\\\\begin{align}" env)
;;   ;;                              (prog2
;;   ;;                                  (cl-incf counter)
;;   ;;                                  (cons begin counter)
;;   ;; 				 (with-temp-buffer
;;   ;;                                  (insert env)
;;   ;;                                  (goto-char (point-min))
;;   ;;                                  ;; \\ is used for a new line. Each one leads
;;   ;;                                  ;; to a number
;;   ;;                                  (cl-incf counter (count-matches "\\\\$"))
;;   ;;                                  ;; unless there are nonumbers.
;;   ;;                                  (goto-char (point-min))
;;   ;;                                  (cl-decf counter
;;   ;;                                           (count-matches "\\nonumber")))))
;;   ;;                             (t
;;   ;;                              (cons begin nil)))))
;;   ;;     (when (setq numberp (cdr (assoc (point) results)))
;;   ;; 	(setf (car args)
;;   ;;             (concat
;;   ;;              (format "\\setcounter{equation}{%s}\n" numberp)
;;   ;;              (car args)))))
;;   ;;   (apply orig-func args))
;;   ;; (advice-add 'org-create-formula-image :around #'org-renumber-environment)

;; LaTeX Preview
(use-package xenops
  :hook (org-mode . my/xenops-mode-safe)
  :config
  (setq org-format-latex-options
        (plist-put org-format-latex-options :justify 'center))

  ;; Fix incompatibility with Org 9.7+ deferred element properties (Emacs 31).
  ;; `xenops-src-do-in-org-mode' parses the src block in a temp buffer; the
  ;; returned org-element still references that buffer, so resolving deferred
  ;; properties (e.g. :value) after the temp buffer is killed throws
  ;; (error "Selecting deleted buffer").  Compute the babel info while the
  ;; temp buffer is still alive.
  (defun grant/xenops-src-parse-at-point ()
    "Fixed `xenops-src-parse-at-point' for Org 9.7+ deferred properties."
    (when-let* ((element (xenops-parse-element-at-point 'src))
                (org-babel-info
                 (xenops-src-do-in-org-mode
                  (when-let* ((org-element (org-element-context)))
                    (org-babel-get-src-block-info 'light org-element)))))
      (xenops-util-plist-update
       element
       :type 'src
       :language (nth 0 org-babel-info)
       :org-babel-info org-babel-info)))
  (advice-add 'xenops-src-parse-at-point :override #'grant/xenops-src-parse-at-point)
  ;; Xenops adds a 20 px horizontal image margin by default.  It shifts the
  ;; visible formula to the right after Emacs has centered the image spec.
  (setq xenops-math-image-margin 0)

  (defun my/xenops-mode-safe--callback (buf)
    "Callback for deferred xenops init in BUF."
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (condition-case err
            (xenops-mode 1)
          (error
           (message "xenops-mode init skipped (non-fatal): %s" err)
           (ignore-errors
             (when xenops-mode
               (xenops-mode -1))
             (font-lock-flush)
             (font-lock-ensure)))))))

  (defun my/xenops-mode-safe ()
    "Enable xenops-mode with cleanup on failure."
    (run-with-idle-timer 0.3 nil
      #'my/xenops-mode-safe--callback (current-buffer)))
  (setq xenops-math-image-scale-factor 1.2)
  (setq xenops-math-image-current-scale-factor 1.2)
  (setq xenops-math-latex-process-alist
	'((dvisvgm :programs ("xelatex" "dvisvgm")
                   :description "xdv > svg"
                   :message "XeLaTeX and dvisvgm process..."
                   :image-input-type "xdv"
                   :image-output-type "svg"
                   :image-size-adjust (1.7 . 1.5)
                   :latex-compiler ("xelatex -no-pdf -interaction nonstopmode -output-directory %o %f")
                   ;; -e: 用精确字形轮廓计算边界框（默认按字体度量值，
                   ;; 会裁掉 f/j 等斜体字符右侧伸出的笔画）；
                   ;; -b 0.5: 精确边界框外加 0.5pt 内边距防锯齿裁切。
                   :image-converter ("dvisvgm %f -n -e -b 0.5 -c %S -o %O"))))

  (defun grant/xenops-display-math-p (element)
    "Return non-nil when ELEMENT uses display-math delimiters.

Xenops classifies a single-line \\[...\\] expression as `inline-math',
so inspect the source delimiters instead of relying only on its type."
    (or (memq (plist-get element :type) '(block-math table))
        (save-excursion
          (goto-char (plist-get element :begin))
          (skip-chars-forward " \t")
          (or (looking-at-p "\\\\\\[")
              (looking-at-p "\\$\\$")
              (looking-at-p
               "\\\\begin{\\(?:align\\|equation\\|gather\\)\\*?}")))))

  (defun grant/xenops-center-math-preview (element &rest _args)
    "Center a Xenops display-math overlay in the current window."
    (when (grant/xenops-display-math-p element)
      (let* ((beg (plist-get element :begin))
             (end (plist-get element :end))
             (search-beg (save-excursion
                           (goto-char beg)
                           (line-beginning-position)))
             (search-end (save-excursion
                           (goto-char end)
                           (min (point-max) (1+ (line-end-position)))))
             (ov (seq-find
                  (lambda (candidate)
                    (and (overlay-get candidate 'xenops-overlay-type)
                         (imagep (overlay-get candidate 'display))))
                  (overlays-in search-beg search-end))))
        (when ov
          (let* ((image (overlay-get ov 'display))
                 (width (car (image-size image t)))
                 (offset (max 0 (/ (- (window-body-width nil t) width) 2))))
            (overlay-put ov 'before-string
                         (propertize " " 'display
                                     `(space :align-to (,offset)))))))))

  ;; Remove legacy advice when re-evaluating this configuration.
  (advice-remove 'xenops-math-display-image #'eli/xenops-justify-fragment-overlay)
  (advice-remove 'xenops-math-display-image #'grant/xenops-justify-fragment-overlay)
  (advice-remove 'xenops-math-display-image #'grant/xenops-center-math-preview)
  (advice-add 'xenops-math-display-image :after #'grant/xenops-center-math-preview))


;;; Org agenda and capture templates
(with-eval-after-load 'org
  (setq org-agenda-files '("~/org/class.org" "~/org/task.org" "~/org/journal.org"))

  (setq org-capture-templates nil)
  (add-to-list 'org-capture-templates '("t" "Tasks"))
  (add-to-list 'org-capture-templates
	       '("tw" "Work" entry
		 (file+headline "~/org/task.org" "Work")
		 "* TODO %^{Workname}\n%u\n"))
  (add-to-list 'org-capture-templates
	       '("th" "Homework" entry
		 (file+headline "~/org/task.org" "Homework")
		 "* TODO %^{Homeworkname}\n%u\n"))
  (add-to-list 'org-capture-templates
	       '("tl" "Long Task" entry
		 (file+headline "~/org/task.org" "Long Task")
		 "* TODO %^{Longtaskname}\n%u\n"))
  (add-to-list 'org-capture-templates
	       '("tq" "Questions" entry
		 (file+headline "~/org/task.org" "Questions")
		 "* TODO %^{Questionname}\n%u\n"))
  (add-to-list 'org-capture-templates
	       '("c" "Class" entry
		 (file "~/org/class.org")
		 "* TODO %^{Coursename}\n%u\n"))
  (add-to-list 'org-capture-templates
	       '("i" "Inbox" entry (file "~/org/inbox.org")
		 "* %U - %^{Inboxname}\n%?"))
  (add-to-list 'org-capture-templates
	       '("j" "Journal" entry (file "~/org/journal.org")
		 "* %U - Journal\n  %?"))
  (add-to-list 'org-capture-templates
	       '("e" "Event" entry (file "~/org/event.org")
		 "* TODO %^{Eventname}\n  %?"))
  (add-to-list 'org-capture-templates
	       `("s" "Sketch"
		 plain
		 (file (lambda ()
			 (let ((title (read-string "Title: ")))
			   (setq my/sketch-title title)
			   (format "~/org/sketch/%s.org" title))))
		 "#+title: %(capitalize my/sketch-title)\n#+author: Grant\n#+date: %t\n-----\n"))
  )

(provide 'init-orgmodule)
;;; init-orgmodule.el ends here
