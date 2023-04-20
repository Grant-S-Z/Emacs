;;; init-lsp.el --- for languages
;;; Commentary:
;;; Code:

;; language server
(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c l")

  :hook
  (python-mode . lsp-deferred)
  (c++-mode . lsp-deferred)
  (tex-mode . lsp-deferred)
  (latex-mode . lsp-deferred)
  (lsp-mode . lsp-enable-which-key-integration)
  
  :custom
  (lsp-enable-snippet t)
  (lsp-keep-workspace-alive t)
  (lsp-enable-imenu t)
  (lsp-enable-imenu t)
  (lsp-enable-completion-at-point nil)

  :commands lsp
  )

(use-package lsp-ui :commands lsp-ui-mode)

(use-package lsp-ivy :commands lsp-ivy-workspace-symbol)

(use-package lsp-treemacs :commands lsp-treemacs-errors-list)

;; for debugger
(use-package dapmode)

(use-package dap-LANGUAGE)

;; python
(use-package elpy
  :init (advice-add 'python-mode :before 'elpy-enable)
  :config (setq elpy-rpc-backend "jedi"))

;; (use-package lsp-jedi
;;   :config
;;   (with-eval-after-load "lsp-mode"
;;     (add-to-list 'lsp-disabled-clients 'pyls)
;;     (add-to-list 'lsp-enabled-clients 'jedi))
;;   (setq lsp-jedi-workspace-extra-paths
;; 	["~/code/python/venv/lib/python3.11/site-packages"]))

;; (use-package company-jedi
;;   :config
;;   (add-to-list 'company-backends 'company-jedi))

;; (use-package pyvenv-mode
;;   :hook ((python-mode . pyvenv-mode)))

(provide 'init-lsp)
;;; init-lsp.el ends here
