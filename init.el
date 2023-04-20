;;; init.el --- for emacs all
;;; Commentary:
;;; Code:
(add-to-list 'load-path "~/.emacs.d/lisp")

(require 'init-none)
(require 'init-const)
(require 'init-kbd)
(require 'init-startup)
(require 'init-elpa)
(require 'init-package)
(require 'init-ui)
;(require 'init-lsp)
(require 'init-org)
(require 'init-tex)

(provide 'init)
;;; init.el ends here
