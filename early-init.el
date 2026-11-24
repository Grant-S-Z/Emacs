;;; early-init.el --- early init settings  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
(setq window-resize-pixelwise t
      frame-resize-pixelwise t)
;; Keep the borderless style on the main frame only; undecorated child frames
;; (corfu, posframe) confuse macOS's window handling.  The parameter is NS-only.
(when (eq system-type 'darwin)
  (add-to-list 'initial-frame-alist '(undecorated-round . t)))

;; Let init-elpa activate packages after it fixes `load-path';
;; `package-quickstart' avoids re-reading every autoload file.
(setq package-enable-at-startup nil
      package-quickstart t)

;; Hide these before the first frame is drawn.
(scroll-bar-mode -1)
(tool-bar-mode -1)

;; Postpone GC during init, then collect at 32 MB / 10% heap growth.
(setq gc-cons-threshold most-positive-fixnum)
(add-hook 'after-init-hook
          (lambda ()
            (setq gc-cons-threshold (* 32 1024 1024)
                  gc-cons-percentage 0.1)))

;; Keep native-comp's async process away from these use-package files: it does
;; not load init-elpa, so the macros would not expand.
(defvar native-comp-jit-compilation-deny-list nil)
(add-to-list 'native-comp-jit-compilation-deny-list
             (concat (regexp-quote (expand-file-name "lisp/init-" user-emacs-directory))
                     ".*\\.elc?\\'"))

(provide 'early-init)
