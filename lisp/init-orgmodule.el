;;; init-orgmodule.el --- org latex modules and task templates
;;; Commentary:
;;; Code:
;;; Org exports to LaTeX settings
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

;; babel output
(setq org-latex-listings 'minted)
(setq org-latex-pdf-process '("latexmk -f -pdf -shell-escape -%latex -interaction=nonstopmode -output-directory=%o %f")) ;; add "-shell-escape" for minted

;; latex classes
(setq org-latex-classes '(("article" "
\\documentclass[11pt]{article}
\% fonts
\\usepackage{fontspec}
\\setmainfont{Times New Roman}
\\setmonofont{Inconsolata}

\\usepackage{amsfonts}
\\usepackage{amsthm}
\\usepackage{bm}
\\usepackage{siunitx}
\\usepackage{xcolor}

\\usepackage{cite}
\\usepackage{booktabs}
\\usepackage{graphicx}
\\usepackage{subfigure}
\\usepackage{minted}

\\usepackage[margin=1in]{geometry}
\\geometry{a4paper}

\\usepackage{mathrsfs}
\% commands
\\newcommand{\\mr}[1]{\\mathrm{#1}}
\\newcommand{\\mb}[1]{\\mathbf{#1}}
\\newcommand{\\mc}[1]{\\mathcal{#1}}
\\newcommand{\\ms}[1]{\\mathscr{#1}}
"

  ("\\section{%s}" . "\\section*{%s}")
  ("\\subsection{%s}" . "\\subsection*{%s}")
  ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
  ("\\paragraph{%s}" . "\\paragraph*{%s}")
  ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))))

(add-to-list 'org-latex-classes '("ctexart" "
\\documentclass[UTF8, a4paper, 11pt]{ctexart}

\% fonts
\\usepackage{fontspec}
\\setmainfont{Times New Roman}
\\setmonofont{Ligconsolata}
\\setCJKmainfont{SimSong}
\\setCJKmonofont{SimSong}

\\usepackage{amsfonts}
\\usepackage{amsthm}
\\usepackage{bm}
\\usepackage{siunitx}
\\usepackage{xcolor}

\\usepackage{cite}
\\usepackage{booktabs}
\\usepackage{graphicx}
\\usepackage{subfigure}
\\usepackage{minted}

\\usepackage[margin=1in]{geometry}
\\geometry{a4paper}

\\usepackage{mathrsfs}

\\usepackage{tikz}
\\usepackage{tikz-feynman}
\% commands
\\newcommand{\\mr}[1]{\\mathrm{#1}}
\\newcommand{\\mb}[1]{\\mathbf{#1}}
\\newcommand{\\mc}[1]{\\mathcal{#1}}
\\newcommand{\\ms}[1]{\\mathscr{#1}}
"

("\\section{%s}" . "\\section*{%s}")
("\\subsection{%s}" . "\\subsection*{%s}")
("\\subsubsection{%s}" . "\\subsubsection*{%s}")
("\\paragraph{%s}" . "\\paragraph*{%s}")
("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

;; beamer
(add-to-list 'org-latex-classes '("beamer" "
\\documentclass[10pt]{beamer}

\% fonts
\\usepackage{ctex}
\\usepackage{fontspec}
\\setmainfont{Times New Roman}
\\setmonofont{Inconsolata}
\\setsansfont{Times New Roman}
\\setCJKmainfont{SimSong} % much more beautiful than STSong, which is sans serif
\\setCJKsansfont{SimSong}
\\setCJKmonofont{LXGW WenKai Mono}

\\usepackage{amsfonts}
\\usepackage{amsthm}
\\usepackage{bm}
\\usepackage{siunitx}
\\usepackage{xcolor}

\\usepackage{mathrsfs}
\% commands
\\newcommand{\\mr}[1]{\\mathrm{#1}}
\\newcommand{\\mb}[1]{\\mathbf{#1}}
\\newcommand{\\mc}[1]{\\mathcal{#1}}
\\newcommand{\\ms}[1]{\\mathscr{#1}}
"

("\\section{%s}" . "\\section*{%s}")
("\\subsection{%s}" . "\\subsection*{%s}")
("\\subsubsection{%s}" . "\\subsubsection*{%s}")
("\\paragraph{%s}" . "\\paragraph*{%s}")
("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

;; book
(add-to-list 'org-latex-classes '("book" "
\\documentclass[10pt, a4paper, pagesize=auto]{book}

\% fonts
\\usepackage{ctex}
\\usepackage{fontspec}
\\setmainfont{Times New Roman}
\\setmonofont{Inconsolata}
\\setsansfont{Times New Roman}
\%\setCJKmainfont{SimSong}
\%\setCJKsansfont{Kai}
\%\setCJKmonofont{Kai}
\\setcounter{secnumdepth}{3}

\\usepackage{amsfonts}
\\usepackage{amsthm}
\\usepackage{bm}
\\usepackage{siunitx}
\\usepackage{xcolor}
"

("\\section{%s}" . "\\section*{%s}")
("\\subsection{%s}" . "\\subsection*{%s}")
("\\subsubsection{%s}" . "\\subsubsection*{%s}")
("\\paragraph{%s}" . "\\paragraph*{%s}")
("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

;;; Org latex preview settings
;; Process
(setq org-preview-latex-default-process 'dvisvgm)

;; Format
(setq org-format-latex-options
        (list :foreground 'default
              :background 'default
              :scale 1.3
              :matchers '("begin" "$1" "$" "$$" "\\(" "\\[")))

;; Header
(setq org-format-latex-header "\\documentclass{article}
\\usepackage[usenames]{color}
\\usepackage{siunitx}
\\usepackage{mathrsfs}
\\usepackage{amsfonts}
\\usepackage{bm}
\\usepackage{tikz}
\\usepackage{tikz-feynman}
\[DEFAULT-PACKAGES]
\[PACKAGES]
\\pagestyle{empty}             % do not remove
% New commands
\\newcommand{\\mr}[1]{\\mathrm{#1}}
\\newcommand{\\mb}[1]{\\mathbf{#1}}
\\newcommand{\\mc}[1]{\\mathcal{#1}}
\\newcommand{\\ms}[1]{\\mathscr{#1}}
% The settings below are copied from fullpage.sty
\\setlength{\\textwidth}{\\paperwidth}
\\addtolength{\\textwidth}{-3cm}
\\setlength{\\oddsidemargin}{1.5cm}
\\addtolength{\\oddsidemargin}{-2.54cm}
\\setlength{\\evensidemargin}{\\oddsidemargin}
\\setlength{\\textheight}{\\paperheight}
\\addtolength{\\textheight}{-\\headheight}
\\addtolength{\\textheight}{-\\headsep}
\\addtolength{\\textheight}{-\\footskip}
\\addtolength{\\textheight}{-3cm}
\\setlength{\\topmargin}{1.5cm}
\\addtolength{\\topmargin}{-2.54cm}")

(use-package org-fragtog ;; auto preview
  :hook (org-mode . org-fragtog-mode))

;; Center vertically
(defun grant/org-latex-preview-advice (beg end &rest _args)
  (let* ((ov (car (overlays-in beg end)))
         (img (cdr (overlay-get ov 'display)))
         (new-img (plist-put img :ascent 90)))
    (overlay-put ov 'display (cons 'image new-img))))
(advice-add 'org--make-preview-overlay
            :after #'grant/org-latex-preview-advice)

;; from: https://kitchingroup.cheme.cmu.edu/blog/2016/11/06/
;; Justifying LaTeX preview fragments in org
(plist-put org-format-latex-options :justify 'center)

(defun eli/org-justify-fragment-overlay (beg end image imagetype)
  (let* ((position (plist-get org-format-latex-options :justify))
         (img (create-image image 'svg t))
         (ov (car (overlays-at (/ (+ beg end) 2) t)))
         (width (car (image-display-size (overlay-get ov 'display))))
         offset)
    (cond
     ((and (eq 'center position)
           (= beg (line-beginning-position)))
      (setq offset (floor (- (/ fill-column 2)
                             (/ width 2))))
      (if (< offset 0)
          (setq offset 0))
      (overlay-put ov 'before-string (make-string offset ? )))
     ((and (eq 'right position)
           (= beg (line-beginning-position)))
      (setq offset (floor (- fill-column
                             width)))
      (if (< offset 0)
          (setq offset 0))
      (overlay-put ov 'before-string (make-string offset ? ))))))
(advice-add 'org--make-preview-overlay
            :after 'eli/org-justify-fragment-overlay)

;; from: https://kitchingroup.cheme.cmu.edu/blog/2016/11/07/
;; Better-equation-numbering-in-LaTeX-fragments-in-org-mode/
(defun org-renumber-environment (orig-func &rest args)
  (let ((results '())
        (counter -1)
        (numberp))
    (setq results (cl-loop for (begin .  env) in
                           (org-element-map (org-element-parse-buffer)
                               'latex-environment
                             (lambda (env)
                               (cons
                                (org-element-property :begin env)
                                (org-element-property :value env))))
                           collect
                           (cond
                            ((and (string-match "\\\\begin{equation}" env)
                                  (not (string-match "\\\\tag{" env)))
                             (cl-incf counter)
                             (cons begin counter))
                            ((and (string-match "\\\\begin{align}" env)
                                  (string-match "\\\\notag" env))
                             (cl-incf counter)
                             (cons begin counter))
                            ((string-match "\\\\begin{align}" env)
                             (prog2
                                 (cl-incf counter)
                                 (cons begin counter)
                               (with-temp-buffer
                                 (insert env)
                                 (goto-char (point-min))
                                 ;; \\ is used for a new line. Each one leads
                                 ;; to a number
                                 (cl-incf counter (count-matches "\\\\$"))
                                 ;; unless there are nonumbers.
                                 (goto-char (point-min))
                                 (cl-decf counter
                                          (count-matches "\\nonumber")))))
                            (t
                             (cons begin nil)))))
    (when (setq numberp (cdr (assoc (point) results)))
      (setf (car args)
            (concat
             (format "\\setcounter{equation}{%s}\n" numberp)
             (car args)))))
  (apply orig-func args))
(advice-add 'org-create-formula-image :around #'org-renumber-environment)

;;; Org agenda and capture templates
(setq org-agenda-files '("~/org/class.org" "~/org/task.org" "~/org/journal.org"))

(setq org-capture-templates nil)
(add-to-list 'org-capture-templates '("t" "Tasks")) ;; 任务模版
(add-to-list 'org-capture-templates
       '("tw" "Work" entry
	 (file+headline "~/org/task.org" "Work")
	 "* TODO %^{Workname}\n%u\n"))
(add-to-list 'org-capture-templates
       '("th" "Homework" entry
	 (file+headline "~/org/task.org" "Homework")
	 "* TODO %^{Homeworkname}\n%u\n")) ;; 没必要放文件位置，实际很难对应
(add-to-list 'org-capture-templates
       '("tl" "Long Task" entry
	 (file+headline "~/org/task.org" "Long Task")
	 "* TODO %^{Longtaskname}\n%u\n"))
(add-to-list 'org-capture-templates
       '("tq" "Questions" entry
	 (file+headline "~/org/task.org" "Questions")
	 "* TODO %^{Questionname}\n%u\n"))
(add-to-list 'org-capture-templates ;; 课程
	     '("c" "Class" entry
	       (file "~/org/class.org")
	       "* TODO %^{Coursename}\n%u\n"))
(add-to-list 'org-capture-templates ;; 日志
             '("j" "Journal" entry (file "~/org/journal.org")
	 "* %U - 日志\n  %?"))
(add-to-list 'org-capture-templates ;; 事件
             '("e" "Event" entry (file "~/org/event.org")
	 "* TODO %^{Eventname}\n  %?"))

(provide 'init-orgmodule)
;;; init-orgmodule.el ends here
