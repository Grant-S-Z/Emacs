;;; init.el --- for emacs all
;;; Commentary:
;;; Code:
;; 路径
(add-to-list 'load-path "~/.emacs.d/lisp")

;; Require
(require 'init-const)
(require 'init-startup)
(require 'init-elpa)
(require 'init-ui)
(require 'init-package)
(require 'init-mail)
(require 'init-tex)
(require 'init-org)
(require 'init-lsp)
(require 'init-kbd)

(provide 'init)
;;; init.el ends here
