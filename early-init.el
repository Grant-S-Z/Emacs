;;; early-init.el --- early init settings  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
(setq window-resize-pixelwise t
      frame-resize-pixelwise t)
;; Keep the borderless style on the main frame only: `default-frame-alist'
;; also applies to the tiny child frames made by corfu/posframe, and an
;; undecorated window there confuses macOS's window/Space handling.  The
;; parameter itself is NS-only, so skip it elsewhere.
(when (eq system-type 'darwin)
  (add-to-list 'initial-frame-alist '(undecorated-round . t)))

;; Leave package activation to init-elpa, so its load-path ordering happens
;; before any package is put on `load-path'.  `package-initialize' there will
;; load `package-quickstart-file', which skips re-reading every autoload file;
;; the flag also keeps that cache refreshed after package installs.
(setq package-enable-at-startup nil
      package-quickstart t)

;; Turn these off before the first frame is drawn; doing it in init.el lets
;; the initial frame flash with a toolbar and scroll bars.
(scroll-bar-mode -1)
(tool-bar-mode -1)

;; Postpone GC while loading init files; restore a generous threshold
;; after startup.  Repeated garbage collection during init is a common
;; cause of slow startup.
(setq gc-cons-threshold most-positive-fixnum)
(add-hook 'emacs-startup-hook
          (lambda () (setq gc-cons-threshold (* 32 1024 1024))))

(provide 'early-init)
