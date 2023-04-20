;;; init.el --- for emacs all
;;; Commentary:
;;; Code:
(add-to-list 'load-path "~/.emacs.d/lisp")

(require 'init-none)
(require 'init-const)
(require 'init-kbd)
(require 'init-startup)
(require 'init-elpa)
(require 'init-ui)
(require 'init-package)
(require 'init-org)
(require 'init-tex)
;(require 'init-lsp)

(provide 'init)
;;; init.el ends here
