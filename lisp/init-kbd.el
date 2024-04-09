;;; init-kbd.el --- for kbd
;;; Commentary:
;;; Code:
;;; alias
(defalias 'yes-or-no-p 'y-or-n-p) ;; yes-or-no

;;; 快捷键
;; org
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c j") 'open-journal-at-today)

;; lsp
(global-set-key (kbd "C-x c") 'compile)

;; tex
(global-set-key (kbd "C-c o") 'cdlatex-environment)

(provide 'init-kbd)
;;; init-kbd.el ends here
