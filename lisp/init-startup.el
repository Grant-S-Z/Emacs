;;; init-startup.el --- startup settings  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
;;; Basic
(setq make-backup-files t
      backup-directory-alist
      `(("." . ,(expand-file-name ".cache/backups/" user-emacs-directory)))
      backup-by-copying t
      version-control t
      kept-new-versions 6
      kept-old-versions 2
      delete-old-versions t)
(make-directory (expand-file-name ".cache/backups/" user-emacs-directory) t)
;; Read subprocess output in 1 MB chunks; 4 KB throttles clangd/pyright.
(setq read-process-output-max (* 1024 1024))

;; Log native-compilation warnings instead of popping up *Warnings*.
(setq native-comp-async-report-warnings-errors 'silent)

;; Skip re-compacting font caches; the pause is noticeable on macOS.
(setq inhibit-compacting-font-caches t)
(setq inhibit-startup-message t)
(setq frame-title-format "Emacs")
(setq-default cursor-type 'bar)
;; Prefer tabs for indentation; major modes may still override this locally.
(setq-default indent-tabs-mode t)

(winner-mode 1)
(delete-selection-mode 1)
(global-auto-revert-mode 1)
(when *is-mac*
  (menu-bar-mode 1))
(when *is-linux*
  (menu-bar-mode -1))

(add-hook 'prog-mode-hook #'subword-mode) ; camelCase-aware motion
(electric-pair-mode 1)
(repeat-mode 1)
(show-paren-mode 1) ; global mode, so it stays out of prog-mode-hook
(add-hook 'prog-mode-hook #'hs-minor-mode)

;; Format time values in English.
(setq system-time-locale "C")

;; Prefer sources over stale byte-compiled files.
(setq load-prefer-newer t)

;; Keep `so-long' from taking over Org files with very long lines.
(with-eval-after-load 'so-long
  (setq so-long-threshold 20000))

(setq initial-scratch-message nil)

;;; macOS modifier keys
(when *is-mac*
  (setq mac-option-modifier nil        ; for emacs-plus
        mac-command-modifier 'meta))

;;; Custom file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load-file custom-file))

(provide 'init-startup)
;;; init-startup.el ends here
