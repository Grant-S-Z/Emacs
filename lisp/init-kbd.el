;;; init-kbd.el --- for kbd
;;; Commentary:
;;; Code:
;;; alias
(defalias 'yes-or-no-p 'y-or-n-p) ;; yes-or-no

;;; 快捷键
;; writing
(global-set-key (kbd "C-c C-c") 'recenter) ;; 居中

;; org
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)

(provide 'init-kbd)
;;; init-kbd.el ends here
