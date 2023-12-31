;;; init-lsp.el --- for languages
;;; Commentary:
;;; Code:

;;; Language Server
;; lsp-bridge
(add-to-list 'load-path "~/.emacs.d/site-lisp/lsp-bridge")
(require 'lsp-bridge)
(global-lsp-bridge-mode)
(add-hook 'find-file-hook #'lsp-bridge-restart-process)

;; remote
(setq lsp-bridge-remote-start-automatically t)

;;; Grammer
(use-package flycheck
  :init (global-flycheck-mode t))
  ;:hook (prog-mode . flycheck-mode))

;;; Debug
(use-package realgud)
(use-package realgud-lldb)

;;; Org-babel
(setq lsp-bridge-enable-org-babel t)
(setq lsp-bridge-org-babel-lang-list '("python" "latex" "c" "c++"))

;;; R
(use-package ess)

;;; Python
(setq lsp-bridge-python-lsp-server "pyright")
(setq lsp-bridge-python-multi-lsp-server "pyright_ruff")
(setq python-indent-guess-indent-offset t)
(setq python-indent-guess-indent-offset-verbose nil)
;; Change mode for virtual environment
(use-package direnv
 :config
 (direnv-mode))
(use-package exec-path-from-shell
  :ensure t
  :config
  (when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize)))

;;; LaTeX
(setq lsp-bridge-tex-lsp-server "texlab")

;;; Markdown
(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

;;; Jupyter notebook
(use-package ein)

(provide 'init-lsp)
;;; init-lsp.el ends here
