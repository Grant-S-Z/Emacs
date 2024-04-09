;;; init-lsp.el --- for languages
;;; Commentary:
;;; Code:

;;; Get commands work in shell
(use-package exec-path-from-shell
  :defer nil
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

;;; Language Server
;; lsp-bridge
(use-package lsp-bridge
  :load-path "~/.emacs.d/site-lisp/lsp-bridge"
  :bind ("C-x C-l" . lsp-bridge-mode)
  :config
  (setq-default
   acm-enable-icon t
   acm-enable-doc t
   acm-enable-yas nil
   acm-enable-tempel nil
   acm-enable-quick-access t
   acm-enable-search-file-words nil
   acm-enable-telega nil
   acm-enable-tabnine nil
   lsp-bridge-enable-log nil
   lsp-bridge-enable-signature-help nil
   lsp-bridge-enable-diagnostics nil
   lsp-bridge-complete-manually nil
   lsp-bridge-enable-search-words nil)
  ;; Languages
  (setq lsp-bridge-c-lsp-server "clangd") ;; c and c++
  (setq lsp-bridge-enable-org-babel t) ;; org
  (setq lsp-bridge-python-command "/Users/grant/anaconda3/bin/python") ;; python
  (setq lsp-bridge-python-lsp-server "pyright")
  (setq lsp-bridge-python-multi-lsp-server "pyright_ruff")
  (setq lsp-bridge-tex-lsp-server "texlab") ;; LaTeX
  ;; Else
  (setq lsp-bridge-default-mode-hooks (remove 'org-mode-hook lsp-bridge-default-mode-hooks))
  (add-hook 'find-file-hook #'lsp-bridge-restart-process)) ;; 每进入一次其他文件，重启 lsp

;;; Grammer
(use-package flycheck
  :init (global-flycheck-mode t))

;;; Tree-sitter
(setq treesit-extra-load-path '("~/.emacs.d/tree-sitter/"))

;;; R
(use-package ess)

;;; Python
(setq python-interpreter "~/anaconda3/bin/python")
(setq python-shell-interpreter "~/anaconda3/bin/ipython")
(setq python-indent-guess-indent-offset t)
(setq python-indent-guess-indent-offset-verbose nil)

;;; Markdown
(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

;;; Cmake
(use-package cmake-mode)

;;; Lua
(use-package lua-mode)

;;; Json
(use-package json-mode)

;;; Quickrun
(use-package quickrun
  :commands (quickrun)
  :bind ("C-<return>" . quickrun)
  :config
  (quickrun-add-command "c++"
    '((:command . "g++")
      (:exec . ("%c %o -o %e %s"
		"%e %a"))
      (:remove . ("%e")))
    :default "c++"))

(provide 'init-lsp)
;;; init-lsp.el ends here
