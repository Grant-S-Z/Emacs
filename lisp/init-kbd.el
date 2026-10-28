;;; init-kbd.el --- for kbd  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
;;; Key settings
;; Global
(global-set-key (kbd "C-M-<return>") 'toggle-frame-fullscreen)
(global-set-key (kbd "M-[") 'backward-paragraph)
(global-set-key (kbd "M-]") 'forward-paragraph)

;; TeX
(global-set-key (kbd "C-c o") 'cdlatex-environment)

(provide 'init-kbd)
;;; init-kbd.el ends here
