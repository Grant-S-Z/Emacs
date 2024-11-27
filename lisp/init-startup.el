;;; init-startup.el -- when starting
;;; Commentary:
;;; Code:
;;; Basic
(setq make-backup-files nil) ;; no backup files
(setq inhibit-startup-message t) ;; no startup message
(setq frame-title-format "Emacs") ;; frame title
(setq-default cursor-type 'bar) ;; set cursor type to hollow box

(scroll-bar-mode -1) ;; no scroll bar
(tool-bar-mode -1) ;; no tool bar
(winner-mode 1) ;; undo window operation
(delete-selection-mode 1) ;; replace the contents in selected region
(global-auto-revert-mode 1) ;; auto refresh changed windows
(when *is-linux*
  (menu-bar-mode -1))

(add-hook 'prog-mode-hook #'subword-mode) ;; operation on camel words
(electric-pair-mode 1) ;; generate parens automatically
(add-hook 'prog-mode-hook #'show-paren-mode) ;; show paren
(add-hook 'prog-mode-hook #'hs-minor-mode) ;; hideshow

;; System locale to use for formatting time values.
(setq system-time-locale "C") ;; in English

;; Avoid byte-compiled files that are older than their source files
(setq load-prefer-newer t)

;; Scratch
(setq initial-scratch-message nil)

;;; Mac ligature and scroll
(when *is-mac*
  (mac-auto-operator-composition-mode 1) ;; ligature for mac port
  (setq scroll-margin 2)
  (setq mac-mouse-wheel-smooth-scroll t) ;; mac pixel scroll
  (setq mac-mouse-wheel-mode t)
  (setq mac-redisplay-dont-reset-vscroll t)
  ;; (let ((inhibit-message t)) ;; inhibit scroll errors
  ;;   (message "number-or-marker-p, nil"))
  )

;;; Load the contents of load-file into custom.el
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load-file custom-file))

(provide 'init-startup)
;;; init-startup.el ends here
