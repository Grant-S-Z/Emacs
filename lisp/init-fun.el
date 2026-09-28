;;; init-fun.el --- for functions  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
;;; Org insert images in Macos
(defun org-insert-image ()
  "Insert a image from clipboard."
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
    (do-applescript (concat
		     "set the_path to \"" image-file "\" \n"
		     "set png_data to the clipboard as «class PNGf» \n"
		     "set the_file to open for access (POSIX file the_path as string) with write permission \n"
		     "write png_data to the_file \n"
		     "close access the_file"))
    (org-insert-link nil
		     (concat
		      "file:" image-file)
		     "")
    (message "%s" image-file)))

;;; Open files and dirs
(defun open-words ()
  "Open words."
  (interactive)
  (find-file-other-window "~/org/english/words.org"))

(defun open-journal-at-today ()
  "Open journal at today."
  (interactive)
  (find-file-other-window "~/org/journal.org")
  (goto-char (point-max))) ; 移动至最后

(defun open-blog-dir ()
  "Open blog dir."
  (interactive)
  (find-file-other-window "~/research/code/Grant/content/post/"))

(defun grant/open-in-finder ()
  "Show the current file in finder."
  (interactive)
  (let ((path (or (buffer-file-name) default-directory)))
    (shell-command (concat "open -R " (shell-quote-argument path)))))

(defun grant/open-directory-in-vscode ()
  "Open current file's directory in VSCode."
  (interactive)
  (let ((dir (if (buffer-file-name)
		 (file-name-directory (buffer-file-name))
	       default-directory)))
    (start-process "vscode" nil "code" dir)))



(defun grant/open-pdf-with-presentation (pdf)
  "Open PDF with Présentation.app."
  (interactive
   (list
    (read-file-name
     "Choose PDF: " default-directory nil t nil
     (lambda (f)
       (or (file-directory-p f)
           (string-match-p "\\.pdf\\'" (downcase f)))))))
  (setq pdf (expand-file-name pdf))
  (unless (file-exists-p pdf)
    (user-error "File does not exist: %s" pdf))
  (unless (file-directory-p "/Applications/Présentation.app")
    (user-error "Présentation.app not found at /Applications/Présentation.app"))
  ;; (start-process "open-presentation" nil
  ;;                "open" "-a" "/Applications/Présentation.app" pdf)

  (start-process "open-presentation" nil
		 "open" "-b" "fr.imag.iihm.blanch.osx-presentation" pdf))


(defun grant/find-file-make-directory-maybe (filename &optional _wildcards)
  "Create parent directory if not exists while visiting file."
  (unless (file-exists-p filename)
    (let ((dir (file-name-directory filename)))
      (unless (file-exists-p dir)
        (make-directory dir t)))))
(advice-add 'find-file :before #'grant/find-file-make-directory-maybe)

;;; Remember one position when editing a file
(defun remember-init ()
  "Remember current position and setup."
  (interactive)
  (point-to-register 8)
  (message "Have remember one position"))

(defun remember-jump ()
  "Jump to latest position and setup."
  (interactive)
  (let ((tmp (point-marker)))
    (jump-to-register 8)
    (set-register 8 tmp))
  (message "Have back to remember position"))

;;; Rename file and buffer
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

;;; Run Makefile
(defun grant/make-in-current-directory ()
  "Run `make` in the directory of the current buffer's file."
  (interactive)
  (let ((default-directory (file-name-directory (or (buffer-file-name) ""))))
    (compile "make")))

;;; Copy buffer name
(defun grant/copy-buffer-filename (&optional strip-extension)
  "Copy buffer file name to kill ring.
With prefix argument, strip file extension."
  (interactive "P")
  (if-let* ((filename (buffer-file-name)))
      (kill-new (if strip-extension
                    (file-name-base filename)
                  (file-name-nondirectory filename)))
    (message "No file associated with buffer")))

;;; Count chinese characters asynchronously
(defun grant/count-chinese-characters-fast ()
  "Count Chinese characters fastly."
  (interactive)
  (let ((count 0)
        (chunk-size 100000))  ; 每次处理 100KB
    (save-excursion
      (goto-char (point-min))
      (while (< (point) (point-max))
        (let ((end (min (+ (point) chunk-size) (point-max))))
          (while (re-search-forward "[\u4e00-\u9fff]" end t)
            (setq count (1+ count)))
          (goto-char end)
          (message "已统计: %d 字..." count)  ; 显示进度
          (redisplay))))  ; 保持界面响应
    (message "中文字数总计: %d" count)))

(provide 'init-fun)
;;; init-fun.el ends here
