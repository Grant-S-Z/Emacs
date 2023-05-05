;;; init-lsp.el --- for languages
;;; Commentary:
;;; Code:

;; language server
(use-package lsp-mode
  :init
  :hook
  (python-mode . lsp-deferred)
  (c++-mode . lsp-deferred)
  (c-mode . lsp-deferred)
  (json-mode . lsp-deferred)
  (lsp-mode . lsp-enable-which-key-integration)
  
  :custom
  (lsp-enable-snippet t)
  (lsp-keep-workspace-alive t)
  (lsp-enable-imenu t)
  (lsp-enable-completion-at-point nil)

  :commands lsp)

(use-package lsp-ui
  :after lsp-mode
  :commands lsp-ui-mode)

;; (use-package lsp-treemacs :commands lsp-treemacs-errors-list)

;; for debugger
;; (use-package dapmode)

;; (use-package dap-LANGUAGE)

;; python
;; (use-package lsp-pyright
;;   :ensure t
;;   :hook (python-mode . (lambda ()
;;                           (require 'lsp-pyright)
;;                           (lsp-deferred))))

;; R
(use-package ess)

(provide 'init-lsp)
;;; init-lsp.el ends here
