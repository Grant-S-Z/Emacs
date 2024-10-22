;;; init-startup.el -- when starting
;;; Commentary:
;;; Code:
(setq make-backup-files nil) ;; no backup files
(setq inhibit-startup-message t) ;; no startup message
(setq frame-title-format "Emacs") ;; frame title
(setq-default cursor-type 'bar) ;; set cursor type to hollow box
(setq blink-cursor-mode nil) ;; no blinking cursor, which is very distracting

(scroll-bar-mode -1) ;; no scroll bar
(tool-bar-mode -1) ;; no tool bar
(winner-mode 1) ;; undo window operation
(delete-selection-mode 1) ;; replace the contents in selected region
(global-auto-revert-mode 1) ;; auto refresh changed windows
(global-subword-mode 1) ;; go through camel words
(mac-auto-operator-composition-mode 1) ;; Ligature for mac port

(electric-pair-mode 1) ;; generate parens automatically
(add-hook 'prog-mode-hook #'show-paren-mode) ;; show paren
(setq show-paren-style 'expression) ;; show expression between parens
(add-hook 'prog-mode-hook #'hs-minor-mode) ;; hideshow

;; System locale to use for formatting time values.
(setq system-time-locale "C") ;; in English

(setq load-prefer-newer t) ;; avoid byte-compiled files that are older than their source files

;; Load the contents of load-file into custom.el
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load-file custom-file))

(provide 'init-startup)
;;; init-startup.el ends here
