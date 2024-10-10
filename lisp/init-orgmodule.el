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
(setq org-latex-classes '(("art" "
\\documentclass[11pt]{article}

"

  ("\\section{%s}" . "\\section*{%s}")
  ("\\subsection{%s}" . "\\subsection*{%s}")
  ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
  ("\\paragraph{%s}" . "\\paragraph*{%s}")
  ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))))

(add-to-list 'org-latex-classes '("chap" ""))

(add-to-list 'org-latex-classes '("ctexart" "
\\documentclass[UTF8, a4paper, 11pt]{ctexart}

\% fonts
\\usepackage{fontspec}
\\setmainfont{Times New Roman}
\\setmonofont{Inconsolata}
\\setCJKmainfont{SimSong}
\\setCJKmonofont{PingFang SC}

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
\\newcommand{\\m}[1]{\\mathrm{#1}}
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
\\setCJKmainfont{SimSong}
\\setCJKsansfont{SimSong}

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

;; book
(add-to-list 'org-latex-classes '("book" "
\\documentclass[10pt, a4paper, pagesize=auto]{book}

\% fonts
\\usepackage{ctex}
\\usepackage{fontspec}
\\setmainfont{Times New Roman}
\\setmonofont{Inconsolata}
\\setsansfont{Times New Roman}
\\setCJKmainfont{SimSong}
\\setCJKsansfont{Kai}
\\setCJKmonofont{Kai}
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

;;; 待办事项关键词
(setq org-todo-keyword-faces '(("TODO" . "red") "|" ("DONE" . "blue") ("CANCELED" . "black")))

;;; 日程文件位置
(setq org-agenda-files '("~/org/class.org" "~/org/task.org" "~/org/event.org"))

;;; 任务 capture-templetes
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
