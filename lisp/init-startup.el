;;; init-startup.el -- when starting  -*- lexical-binding: t; -*-
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
(when *is-mac*
  (menu-bar-mode 1))
(when *is-linux*
  (menu-bar-mode -1))

(add-hook 'prog-mode-hook #'subword-mode) ;; operation on camel words
(electric-pair-mode 1) ;; generate parens automatically
(show-paren-mode 1) ;; 全局括号匹配高亮（show-paren-mode 是全局模式，不应挂进 prog-mode-hook）
(add-hook 'prog-mode-hook #'hs-minor-mode) ;; hideshow

;; System locale to use for formatting time values.
(setq system-time-locale "C") ;; in English

;; Avoid byte-compiled files that are older than their source files
(setq load-prefer-newer t)

;; Prevent `so-long' from disabling font-lock in org files with long lines
;; (e.g. long tables in journal.org).
(with-eval-after-load 'so-long
  (setq so-long-threshold 1000))

;; Scratch
(setq initial-scratch-message nil)

;;; Mac ligature and scroll
(when *is-mac*
  ;; (mac-auto-operator-composition-mode 1) ;; ligature for mac port
  ;; (setq scroll-margin 1)
  ;; (setq mac-mouse-wheel-smooth-scroll t) ;; mac pixel scroll
  ;; (setq mac-mouse-wheel-mode t)
  ;; (setq mac-redisplay-dont-reset-vscroll t)
  (setq mac-option-modifier nil		;; for emacs-plus
	mac-command-modifier 'meta))

;;; Load the contents of load-file into custom.el
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load-file custom-file))

(provide 'init-startup)
;;; init-startup.el ends here
