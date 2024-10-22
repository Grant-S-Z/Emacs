;;; init-lsp.el --- for languages
;;; Commentary:
;;; Code:
;;; LSP
;; Lsp bridge
(use-package lsp-bridge
  :defer nil
  :load-path "~/.emacs.d/site-lisp/lsp-bridge"
  :bind (("C-x C-l" . lsp-bridge-mode)
	 ("C-x C-p" . lsp-bridge-peek) ;; 8 -> lsp-bridge-peek-jump
	 ("C-x C-8" . lsp-bridge-peek-jump-back)
	 ("C-c <return>" . lsp-bridge-code-format))
  :config
  (setq lsp-bridge-peek-file-content-height 14)
  (setq lsp-bridge-peek-file-content-scroll-margin 2)
  (setq-default
   acm-enable-icon t
   acm-enable-doc t
   acm-enable-yas nil
   acm-enable-tempel nil
   acm-enable-quick-access t
   acm-enable-search-file-words nil
   acm-enable-telega nil
   acm-enable-tabnine nil
   acm-enable-doc-markdown-render 'async
   acm-candidate-match-function 'orderless-flex
   lsp-bridge-enable-log nil
   lsp-bridge-enable-signature-help nil
   lsp-bridge-enable-diagnostics nil
   lsp-bridge-complete-manually nil
   lsp-bridge-enable-search-words nil
   lsp-bridge-enable-auto-format-code nil)
  ;; languages
  (setq lsp-bridge-default-mode-hooks '(c-mode-hook c++-mode-hook python-mode-hook bash-mode-hook sh-mode-hook emacs-lisp-mode-hook lisp-interaction-mode-hook LaTeX-mode-hook bibtex-mode-hook))
  (setq lsp-bridge-c-lsp-server "clangd")
  (setq lsp-bridge-python-command "~/miniconda3/envs/hep/bin/python")
  (setq lsp-bridge-python-lsp-server "~/miniconda3/envs/hep/bin/pyright")
  (setq lsp-bridge-python-multi-lsp-server "pyright_ruff")
  (setq lsp-bridge-enable-org-babel t)
  ;; start
  (add-hook 'find-file-hook #'lsp-bridge-restart-process) ;; 每进入一次其他文件，重启 lsp
  (global-lsp-bridge-mode))

;; Grammer
(use-package flycheck
  :config
  (setq truncate-lines nil) ; 如果单行信息很长会自动换行
  :hook
  (prog-mode . flycheck-mode))

;; Debug
(use-package realgud)

;; Tree-sitter
(setq treesit-extra-load-path '("~/.emacs.d/tree-sitter/"))

;; Quickrun
(use-package quickrun
  :bind ("C-<return>" . quickrun)
  :config
  (setq quickrun-timeout-seconds 600))

;;; Languages
;; Python basic settings
(setq python-interpreter "~/miniconda3/envs/hep/bin/python")
(setq python-shell-interpreter "~/miniconda3/envs/hep/bin/python")
(setq python-indent-guess-indent-offset t)
(setq python-indent-guess-indent-offset-verbose nil)

(setq python-shell-completion-native-enable t)

(setq flycheck-python-pycompile-executable "~/miniconda3/envs/hep/bin/python") ;; flycheck

(setq doom-modeline-env-python-executable "~/miniconda3/envs/hep/bin/python") ;; doom-modeline

(use-package numpydoc ;; Numpydoc
  :bind ("C-x C-n" . numpydoc-generate)
  :config
  (setq numpydoc-insert-examples-block nil)
  (setq numpydoc-insert-return-without-typehint t))

(quickrun-add-command "python" ;; Quickrun
  '((:command . "~/miniconda3/envs/hep/bin/python")
    (:exec . ("%c %s"))
    (:template . nil)
    (:description . "Run Python script..."))
  :default "python")

;; C++ and ROOT
(quickrun-add-command "c++/c1z" ;; Quickrun
  '((:command . "clang++")
    (:exec    . ("%c -std=c++1z %o -o %e %s"
		 "%e %a"))
    (:remove  . ("%e")))
  :default "c++")

(use-package cern-root-mode ;; ROOT
  :config
  (setq cern-root-filepath "~/miniconda3/envs/hep/bin/root"))

;; R
(use-package ess)

;; Markdown
(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown")
  :config
  (setq prettify-symbols-mode t))

;; Cmake
(use-package cmake-mode)

;; Lua
(use-package lua-mode)

;; Json
(use-package json-mode)

;; Csv
(use-package csv-mode)

;; Yaml
(use-package yaml-mode)

(provide 'init-lsp)
;;; init-lsp.el ends here
