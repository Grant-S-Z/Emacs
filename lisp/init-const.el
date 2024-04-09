;;; init-const.el --- for some const
;;; Commentary:
;;; Code:

;;; 判断操作系统
(defconst *is-mac* (eq system-type 'darwin))
(defconst *is-linux* (eq system-type 'gnu/linux))
(defconst *is-windows* (or (eq system-type 'ms-dos) (eq system-type 'windows-nt)))

;;; 位置
(setq calendar-location-name "Beijing, CN")
(setq calendar-latitude 39.9042)
(setq calendar-longitude 116.4074)

;;; 获取密码
(defun get-or-create-password ()
  (setq password (read-string "Password: "))
  (if (string= password "")
      (create-password)
    password))

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
					   (concat "file:" image-file)
					   "")
	  (message image-file))
      (org-display-inline-images))


(defun open-journal-at-today ()
  "Open journal at today."
  (interactive)
  (let ((today (format-time-string "%Y-%m-%d")))
    (find-file-other-window "~/org/journal.org")
    (goto-char (point-min)) ; 移动至开始位置
    ;; 搜索时间戳
    ;; (ipf (re-search-forward (concat "[" today) nil t)
    ;; 	(beginning-of-line)
    ;;   (message "Today not found."))
    ))

(defun grant/rename-this-file-and-buffer (new-name)
  "Rename both current buffer and file to NEW-NAME."
  (interactive "File's new name: ")
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

;;; Some useful Elisp functions
(defun random-num (max &optional min)
  "Random number between MAX and MIN."
  (let* ((num (random max)))
    (if min (max min num) num)))

(defun random-color-rgb ()
  "Random color in rgb."
  (list (random 255) (random 255) (random 255)))

(defun random-color-html ()
  "Random color in html."
  (apply #'format "#%x%x%x" (random-color-rgb)))

(defun random-color-face ()
  "Generate a random valid color for a font face."
  (let* ((colors (defined-colors))
     (n (length colors)))
    (nth (random n) colors)))

(provide 'init-const)
;;; init-const.el ends here
