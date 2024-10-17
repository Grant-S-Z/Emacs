;;; init-kbd.el --- for kbd
;;; Commentary:
;;; Code:
;;; alias
(defalias 'yes-or-no-p 'y-or-n-p) ;; yes-or-no

;;; 快捷键
;; global
(global-set-key (kbd "C-x k") 'kill-buffer)

;; org
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c w") 'open-words)
(global-set-key (kbd "C-c j") 'open-journal-at-today)
(global-set-key (kbd "C-c z") 'org-zettel-ref-add-quick-note)
(global-set-key (kbd "C-c 9") 'org-zettel-ref-quick-markup)

;; lsp
(global-set-key (kbd "C-x c") 'compile)

;; tex
(global-set-key (kbd "C-c o") 'cdlatex-environment)

(provide 'init-kbd)
;;; init-kbd.el ends here
