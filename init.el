;;; init.el --- for emacs all
;;; Commentary:
;;; Code:
(add-to-list 'load-path "~/.emacs.d/lisp")

(setq gc-cons-threshold most-positive-fixnum)

(require 'init-const)
(require 'init-fun)
(require 'init-startup)
(require 'init-elpa)
(require 'init-ui)
(require 'init-package)
(require 'init-mail)
(require 'init-lsp)
(require 'init-shell)
(require 'init-cpp)
(require 'init-python)
(require 'init-tex)
(require 'init-orgmodule)
(require 'init-org)
(require 'init-kbd)

(setq gc-cons-threshold (* 1024 1024 10)) ; 10 M

(provide 'init)
;;; init.el ends here
