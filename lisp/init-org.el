;;; init-org.el --- Org core, Babel, agenda and capture  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;;; Org-specific functions
(defun org-insert-image ()
  "Insert an image from the system clipboard."
  (interactive)
  (let* ((path (concat default-directory "./img/"))
	 ;; Remove the file extension from the buffer name
	 (base-name (file-name-sans-extension (buffer-name)))
	 (image-file (concat
		      path
		      base-name
		      (format-time-string "_%Y%m%d%H%M%S.png"))))
    (if (not (file-exists-p path))
	(mkdir path))
    (cond
     ((executable-find "pngpaste")
      (unless (zerop (call-process "pngpaste" nil nil nil image-file))
        (user-error "Clipboard does not contain an image")))
     (*is-mac*
      (do-applescript (concat
		       "set the_path to \"" image-file "\" \n"
		       "set png_data to the clipboard as «class PNGf» \n"
		       "set the_file to open for access (POSIX file the_path as string) with write permission \n"
		       "write png_data to the_file \n"
		       "close access the_file")))
     (*is-wsl*
      (grant/org-insert-image-wsl image-file))
     ((executable-find "wl-paste")
      (unless (zerop (call-process "wl-paste" nil image-file nil "--type" "image/png"))
	(user-error "Clipboard does not contain an image")))
     (t
      (user-error "No clipboard image support on this system")))
    (org-insert-link nil
		     (concat
		      "file:" image-file)
		     "")
    (message "%s" image-file)))

(defun grant/org-insert-image-wsl (image-file)
  "Save the Windows clipboard image to IMAGE-FILE (WSL only)."
  (let* ((win-file
          (string-trim
           (shell-command-to-string
            (concat "wslpath -w " (shell-quote-argument image-file)))))
         (script
          (format (concat "Add-Type -AssemblyName System.Windows.Forms; "
                          "Add-Type -AssemblyName System.Drawing; "
                          "$img=[System.Windows.Forms.Clipboard]::GetImage(); "
                          "if ($img -eq $null) { exit 2 }; "
                          "$img.Save('%s',[System.Drawing.Imaging.ImageFormat]::Png);")
                  (replace-regexp-in-string "'" "''" win-file))))
    (unless (zerop (call-process "powershell.exe" nil nil nil
                                 "-NoProfile" "-NonInteractive" "-STA" "-Command" script))
      (user-error "Clipboard does not contain an image"))))

;;; LaTeX delimiter completion
(defun grant/org-electric-latex-pairs ()
  "Auto-close inline and display LaTeX math delimiters in Org buffers."
  (setq-local electric-pair-pairs
              (append
               `((,(regexp-quote "\\(") . "\\)")
                 (,(regexp-quote "\\[") . "\\]"))
               electric-pair-pairs))
  (electric-pair-local-mode 1))
(add-hook 'org-mode-hook #'grant/org-electric-latex-pairs)

;; (defun grant/org-return-between-display-math (orig &optional indent)
;;   "Open an empty display-math pair, or call ORIG with INDENT."
;;   (interactive "P")
;;   (if (and (>= (- (point) (point-min)) 2)
;;            (string= (buffer-substring-no-properties (- (point) 2) (point)) "\\[")
;;            (looking-at-p (regexp-quote "\\]")))
;;       (progn
;;         (newline)
;;         (save-excursion (newline)))
;;     (funcall orig indent)))
;; (with-eval-after-load 'org
;;   (advice-add 'org-return :around #'grant/org-return-between-display-math))

;;; Org global keys
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c w") 'open-words)
(global-set-key (kbd "C-c j") 'open-journal-at-today)
(global-set-key (kbd "C-c b") 'open-blog-dir)

;;; Org core
(use-package org
  :bind
  (("C-x C-y" . org-insert-image)
   ("C-/" . org-latex-preview))
  :config
  ;; Fold and inline images
  (setq org-startup-folded 'content)
  (setq org-image-actual-width 500)

  ;; Editing and TODO settings
  (setq org-auto-align-tags t
	org-tags-column 0
	org-fold-catch-invisible-edits 'show-and-error
	org-special-ctrl-a/e t
	org-insert-heading-respect-content t
	org-hide-emphasis-markers t
	org-ellipsis "…")
  (setq org-todo-keywords '((sequence "TODO" "DONE" "CANCELED")))
  (add-hook 'org-mode-hook (lambda ()
			     (setq truncate-lines nil)))

  ;; Babel
  (setq org-confirm-babel-evaluate nil)
  (setq org-babel-python-command "~/miniconda3/bin/python3")
  ;; (setq org-plantuml-jar-path "~/Code/plantuml/plantuml-1.2024.3.jar")
  (require 'ob-C)
  (require 'ob-shell)
  (require 'ob-mermaid)

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((python . t)
     (emacs-lisp . t)
     ;; (plantuml . t)
     ;; (scheme . t)
     (C . t)
     (shell . t)
     (latex . t)))

  ;; Agenda files and capture templates
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

  :custom
  ;; Prettify
  (org-pretty-entities t)
  (org-startup-indented t)
  (org-highlight-latex-and-related '(latex entities))

  ;; Agenda
  (org-agenda-include-diary t)
  (org-agenda-use-time-grid t)
  (org-agenda-tags-column 0)
  (org-agenda-block-separator ?─)
  (org-agenda-current-time-string
   "⭠ now ─────────────────────────────────────────────────")
  (org-agenda-time-grid (quote ((daily today require-timed)
                                (700
                                 1300
                                 1800
                                 2400)
                                "......"
                                "-----------------------------------------------------"))))

;;; Preload one-time heavy modes during idle time
;; org and AUCTeX are deferred by use-package; without preloading, the
;; first org/LaTeX file open pays the full load cost up front (and org
;; src blocks containing LaTeX load AUCTeX mid-fontification).
(run-with-idle-timer
 3 nil
 (lambda ()
   (require 'org nil t)
   (require 'latex nil t)))

(provide 'init-org)
;;; init-org.el ends here
