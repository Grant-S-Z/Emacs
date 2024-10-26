;;; init-lsp.el --- for languages
;;; Commentary:
;;; Code:
;;; LSP
(use-package lsp-mode
  :init
  (defun grant/lsp-mode-setup-completion ()
     (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
	   '(orderless)))
  :commands (lsp lsp-deferred)
  :hook
  ((c++-mode . lsp-deferred)
   (c-mode . lsp-deferred)
   (lsp-mode . lsp-enable-which-key-integration)
   (lsp-completion-mode . grant/lsp-mode-setup-completion))
  :custom
  (lsp-keymap-prefix "C-x C-l")
  (lsp-file-watch-threshold 500)
  ;; Completion provider
  (lsp-completion-provider :none)
  ;; Python ruff
  (lsp-ruff-python-path "~/miniconda3/bin/python3")
  (lsp-ruff-server-command '("~/.local/bin/ruff" "server"))
  )

(use-package lsp-pyright
  :custom (lsp-pyright-langserver-command "~/.local/bin/pyright")
  :hook (python-mode . (lambda ()
                          (require 'lsp-pyright)
                          (lsp-deferred))))

(use-package lsp-ui)
(use-package lsp-treemacs)
(use-package dap-mode
  :config
  (require 'dap-python)
  (setq dap-python-debugger 'debugpy)
  (require 'dap-lldb))

;; Corfu
(use-package corfu
  :hook ((emacs-lisp-mode . corfu-mode)
	 (lisp-interaction-mode . corfu-mode)
	 (c-mode . corfu-mode)
	 (c++-mode . corfu-mode)
	 (python-mode . corfu-mode))
  :bind (:map corfu-map
              ("M-n" . corfu-next)
              ("M-p" . corfu-previous))
  :config
  (setq corfu-auto t
        corfu-auto-prefix 1
        corfu-auto-delay 0.1
        corfu-quit-no-match t
        corfu-quit-at-boundary t
	)
  (corfu-popupinfo-mode) ;; show doc
  (corfu-indexed-mode) ;; show index
  (dotimes (i 10) ;; use M-num to select index
    (define-key corfu-mode-map
                (kbd (format "M-%s" i))
                (kbd (format "C-%s <tab>" i))))
  )

(use-package nerd-icons-corfu
  :after corfu
  :init (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

;; Grammer
(setq flymake-start-on-save-buffer nil)
(setq flymake-start-on-flymake-mode nil)
(use-package flycheck
  :config
  (setq truncate-lines nil) ; 如果单行信息很长会自动换行
  (setq flycheck-python-pycompile-executable "~/miniconda3/bin/python3")
  (setq flycheck-python-ruff-executable "~/.local/bin/ruff")

  :hook
  (prog-mode . flycheck-mode))

;; Tree-sitter
(setq treesit-extra-load-path '("~/.emacs.d/tree-sitter/"))

;; Quickrun
(use-package quickrun
  :bind ("C-<return>" . quickrun)
  :config
  (setq quickrun-timeout-seconds 600))

;;; Languages
;; Emacs lisp
;; (use-package elisp-autofmt)

;; Python basic settings
(setq python-interpreter "~/miniconda3/bin/python3")
(setq python-shell-interpreter "~/miniconda3/bin/python3")
(setq python-indent-guess-indent-offset t)
(setq python-indent-guess-indent-offset-verbose nil)
(setq python-shell-completion-native-enable t)
(setq doom-modeline-env-python-executable "~/miniconda3/bin/python3") ;; doom-modeline

(use-package numpydoc ;; Numpydoc
  :bind ("C-x C-n" . numpydoc-generate)
  :config
  (setq numpydoc-insert-examples-block nil)
  (setq numpydoc-insert-return-without-typehint t))

(quickrun-add-command "python" ;; Quickrun
  '((:command . "~/miniconda3/bin/python3")
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
  (setq cern-root-filepath "/opt/homebrew/bin/root"))

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
