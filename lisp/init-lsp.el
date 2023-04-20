;;; init-lsp.el --- for languages
;;; Commentary:
;;; Code:

;; language server

;; (require 'lsp-bridge)
;; (global-lsp-bridge-mode)
;; (setq lsp-bridge-tex-lsp-server 'texlab)

(use-package lsp-bridge
  :load-path "site-lisp/lsp-bridge"
  :hook (prog-mode . lsp-bridge-mode)
  :config
  (setq lsp-bridge-enable-signature-help t
	lsp-bridge-enable-log t
	lsp-bridge-enable-org-babel t ;; 启用org下的lsp
	lsp-bridge-python-command "python3"
	lsp-bridge-python-lsp-server "pyright"))

(use-package acm
  :after lsp-bridge
  :load-path "site-lisp/lsp-bridge/acm"
  :config
  (setq acm-mode t
	acm-enable-yas t
     	acm-backend-tempel-candidates-number 4))

;; markdown
(use-package markdown-mode
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown")
  :bind (:map markdown-mode-map ("C-c C-e" . markdown-do)))

;; python
;; (use-package elpy
;;   :init (advice-add 'python-mode :before 'elpy-enable)
;;   :config (setq elpy-rpc-backend "jedi"))

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
