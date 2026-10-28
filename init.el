;;; init.el --- for emacs all  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
(add-to-list 'load-path "~/.emacs.d/lisp")

(require 'init-const)
(require 'init-startup)
(require 'init-elpa)
(require 'init-ui)
(require 'init-package)
(require 'init-ai)
(require 'init-mail)
(require 'init-shell)
(require 'init-lsp)
(require 'init-tex)
(require 'init-org)
(require 'init-org-latex)
(require 'init-org-ui)
(require 'init-org-research)
(require 'init-org-publish)
(require 'init-fun)
(require 'init-kbd)

(provide 'init)
;;; init.el ends here
