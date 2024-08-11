;;; init-python.el --- for Python
;;; Commentary:
;;; Code:

;; Python lsp
(setq lsp-bridge-python-command "/Users/grant/miniconda3/envs/pygeant4/bin/python")
(setq lsp-bridge-python-lsp-server "pyright")
(setq lsp-bridge-python-multi-lsp-server "pyright_ruff")

;; Python interpreter
(setq python-interpreter "~/miniconda3/envs/pygeant4/bin/python")
(setq python-shell-interpreter "~/miniconda3/envs/pygeant4/bin/ipython")
(setq python-shell-exec-path "~/miniconda3/envs/pygeant4/bin/python")
(setq python-indent-guess-indent-offset t)
(setq python-indent-guess-indent-offset-verbose nil)

;; Flycheck
(setq flycheck-python-pycompile-executable "~/miniconda3/envs/pygeant4/bin/python")

;; Doom-modeline
(setq doom-modeline-env-python-executable "~/miniconda3/envs/pygeant4/bin/python")

;; Quickrun

(provide 'init-python)
;;; init-python.el ends here
