;;; init-kbd.el --- for kbd
;;; Commentary:
;;; Code:
;;; Key settings
;; Global
(global-set-key (kbd "C-M-<return>") 'toggle-frame-fullscreen)
(global-set-key (kbd "M-[") 'backward-paragraph)
(global-set-key (kbd "M-]") 'forward-paragraph)

;; Org
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c w") 'open-words)
(global-set-key (kbd "C-c j") 'open-journal-at-today)
(global-set-key (kbd "C-c b") 'open-blog-dir)
(global-set-key (kbd "C-c z") 'org-zettel-ref-add-quick-note)

;; TeX
(global-set-key (kbd "C-c o") 'cdlatex-environment)

(provide 'init-kbd)
;;; init-kbd.el ends here
