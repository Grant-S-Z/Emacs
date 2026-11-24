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

;;; Org global keys
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c w") 'open-words)
(global-set-key (kbd "C-c j") 'open-journal-at-today)
(global-set-key (kbd "C-c b") 'open-blog-dir)

;;; Org core
;; Folding, Babel, agenda, capture, and LaTeX preview.
(use-package org
  :bind
  (("C-x C-y" . org-insert-image)
   ("C-/" . org-latex-preview))
  :config
  ;; Folding and inline image display.
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
  (setq org-todo-keywords '((sequence "TODO" "|" "DONE" "CANCELED")))
  (add-hook 'org-mode-hook (lambda ()
			     (setq truncate-lines nil)))

  ;; Babel
  (setq org-confirm-babel-evaluate nil)
  (setq org-babel-python-command "~/miniconda3/bin/python3")
  (require 'ob-C)
  (require 'ob-shell)
  (require 'ob-mermaid)

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((python . t)
     (emacs-lisp . t)
     (C . t)
     (shell . t)
     (latex . t)))

  ;; Agenda files and capture templates
  (setq org-agenda-files '("~/org/journal.org" "~/org/orgtd/gtd.org"))
  (setq org-capture-templates nil)
  (add-to-list 'org-capture-templates
	       '("i" "Inbox" entry (file "~/org/inbox.org")
		 "* %U - %^{Inboxname}\n%?"))
  (add-to-list 'org-capture-templates
	       '("j" "Journal" entry (file "~/org/journal.org")
		 "* %U - Journal\n  %?"))
  (add-to-list 'org-capture-templates
	       '("g" "GTD" entry
		 (file "~/org/orgtd/gtd.org")
		 "* TODO %^{GTD}\n"))

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
;; Preload deferred Org and AUCTeX after startup so the first file open does
;; not pay the full load cost.
(run-with-idle-timer
 3 nil
 (lambda ()
   (require 'org nil t)
   (require 'latex nil t)))

(provide 'init-org)
;;; init-org.el ends here
