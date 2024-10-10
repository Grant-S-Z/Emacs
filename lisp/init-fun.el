;;; init-fun.el --- for functions
;;; Commentary:
;;; Code:

;;; Quite useful to avoid the note insert position error
(defun grant/outline-show-entry ()
  "Show the body directly following this heading.
Show the heading too, if it is currently invisible."
  (interactive)
  (save-excursion
    (outline-back-to-heading t)
    (outline-flag-region (max (1- (point)) (point-min))
                         (progn
                           (outline-next-preface)
                           (if (= 1 (- (point-max) (point)))
                               (point-max)
                             (point)))
                         nil)))
(advice-add 'outline-show-entry :override #'grant/outline-show-entry)

;;; Org insert images
(defun org-insert-image ()
  "insert a image from clipboard"
  (interactive)
  (let* ((path (concat default-directory "./img/"))
	 (image-file (concat
		      path
		      (buffer-name)
		      (format-time-string "_%Y%m%d_%H%M%S.png"))))
    (if (not (file-exists-p path))
	(mkdir path))
    (do-applescript (concat
		     "set the_path to \"" image-file "\" \n"
		     "set png_data to the clipboard as «class PNGf» \n"
		     "set the_file to open for access (POSIX file the_path as string) with write permission \n"
		     "write png_data to the_file \n"
		     "close access the_file"))
    ;; (shell-command (concat "pngpaste " image-file))
    (org-insert-link nil
		     (concat
		      "file:" image-file)
		     "")
    (message image-file))
  (org-display-inline-images))

(defun open-journal-at-today ()
  "Open journal at today."
  (interactive)
  (find-file-other-window "~/org/journal.org")
  (goto-char (point-max))) ; 移动至最后

(defun open-words-recited ()
  "Open words recited."
  (interactive)
  (find-file-other-window "~/org/words.org")
  (goto-char (point-max)))

(defun grant/open-in-finder ()
  "Show the current file in finder."
  (interactive)
  (let ((path (or (buffer-file-name) default-directory)))
    (shell-command (concat "open -R " (shell-quote-argument path)))))

(defun grant/rename-this-file-and-buffer (new-name)
  "Rename both current buffer and file to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
	(filename (buffer-file-name)))
    (unless filename
      (error "Buffer '%s' is not visiting a file" name))
    (progn
      (when (file-exists-p filename)
	(rename-file filename new-name 1))
      (set-visited-file-name new-name)
      (rename-buffer new-name))))

(defun grant/delete-file-and-buffer ()
  "Kill current buffer and delete the file."
  (interactive)
  (let ((filename (buffer-file-name)))
    (when (and filename (y-or-n-p (concat "Do you really want to delete " filename "?")))
      (delete-file filename t)
      (message "Deleted file %s." filename)
      (kill-buffer))))

(defun grant/clear-messages-buffer ()
  "Clear Message buffer."
  (interactive)
  (let ((inhibit-read-only t))
    (with-current-buffer "*Messages*"
      (erase-buffer))))

(defun grant/kill-unused-buffers ()
  "Kill unused buffers."
  (interactive)
  (ignore-errors
    (save-excursion
      (dolist (buf (buffer-list))
	(set-buffer buf)
	(when (and (string-prefix-p "*" (buffer-name)) (string-suffix-p "*" (buffer-name)))
	  (kill-buffer buf))))))

(defun grant/indent-all ()
  "Indent for all code."
  (interactive)
  (indent-region (point-min) (point-max))
  (message "format successfully"))

(defun grant/make-in-current-directory ()
  "Run `make` in the directory of the current buffer's file."
  (interactive)
  (let ((default-directory (file-name-directory (or (buffer-file-name) ""))))
    (compile "make")))

(defun grant/make-in-parent-directory ()
  "Run `make` in the parent directory of the current buffer's file."
  (interactive)
  (let ((default-directory (file-name-directory (directory-file-name (file-name-directory (or (buffer-file-name) ""))))))
    (compile "make")))

(provide 'init-fun)
;;; init-fun.el ends here
