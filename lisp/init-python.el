;;; init-python.el --- for Python
;;; Commentary:
;;; Code:

;; Python lsp
(setq lsp-bridge-python-command "~/miniconda3/bin/python")
(setq lsp-bridge-python-lsp-server "~/miniconda3/bin/pyright")
;(setq lsp-bridge-python-multi-lsp-server "pyright_ruff")

;; Python interpreter
(setq python-interpreter "~/miniconda3/bin/python")
(setq python-shell-interpreter "~/miniconda3/bin/ipython")
(setq python-shell-exec-path "~/miniconda3/bin/python")
(setq python-indent-guess-indent-offset t)
(setq python-indent-guess-indent-offset-verbose nil)

(setq python-shell-completion-native-enable t)
(setq org-babel-python-command "~/miniconda3/bin/python")

;; Flycheck
(setq flycheck-python-pycompile-executable "~/miniconda3/bin/python")

;; Doom-modeline
(setq doom-modeline-env-python-executable "~/miniconda3/bin/python")

;; Quickrun
(quickrun-add-command "python"
  '((:command . "~/miniconda3/bin/python")
    (:exec . ("%c %s"))
    (:template . nil)
    (:description . "Run Python script..."))
  :default "python")

(provide 'init-python)
;;; init-python.el ends here