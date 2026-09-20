;;; early-init.el --- early init settings  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
;; (setq frame-inhibit-implied-resize t)
(setq window-resize-pixelwise t
      frame-resize-pixelwise t)
(add-to-list 'default-frame-alist '(undecorated-round . t))

;; Some third-party packages in elpa/ lack a `lexical-binding' cookie.
;; We cannot patch them in place (upgrades would overwrite the fix),
;; so completely ignore this specific warning class: suppress both the
;; popup display (`warning-suppress-types') and the *Warnings* log entry
;; (`warning-suppress-log-types').  The `defvar's are needed because
;; warnings.el is not loaded yet this early; `defcustom' there will not
;; override values already set here.
(defvar warning-suppress-types nil)
(defvar warning-suppress-log-types nil)
(add-to-list 'warning-suppress-types '(files missing-lexbind-cookie))
(add-to-list 'warning-suppress-log-types '(files missing-lexbind-cookie))

(provide 'early-init)
