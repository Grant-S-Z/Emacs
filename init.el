;;; init.el --- for emacs all  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
(add-to-list 'load-path "~/.emacs.d/lisp")
(add-to-list 'load-path "~/.emacs.d/site-lisp/dsh")

(require 'early-init)
(require 'init-const)
(require 'init-startup)
(require 'init-elpa)
;; Neomacs (Rust Emacs fork) compatibility workarounds; no-op under GNU Emacs.
(when (featurep 'neomacs)
  (require 'init-neomacs))
(require 'init-ui)
(require 'init-package)
(require 'init-mail)
(require 'init-shell)
(require 'init-lsp)
(require 'init-tex)
(require 'init-org)
(require 'init-orgmodule)
(require 'init-fun)
(require 'init-kbd)

(provide 'init)
;;; init.el ends here
