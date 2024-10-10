;;; init-lsp.el --- for languages
;;; Commentary:
;;; Code:

;;; Language Server
;;; lsp-bridge
(use-package lsp-bridge
  :defer nil
  :load-path "~/.emacs.d/site-lisp/lsp-bridge"
  :bind (("C-x C-l" . lsp-bridge-mode)
	 ("C-x C-p" . lsp-bridge-peek)
	 ("C-c <RET>" . lsp-bridge-code-format))
  :config
  (setq-default
   acm-enable-icon t
   acm-enable-doc t
   acm-enable-yas t
   acm-enable-tempel nil
   acm-enable-quick-access t
   acm-enable-search-file-words nil
   acm-enable-telega nil
   acm-enable-tabnine nil
   lsp-bridge-enable-log nil
   lsp-bridge-enable-signature-help nil
   lsp-bridge-enable-diagnostics nil
   lsp-bridge-complete-manually nil
   lsp-bridge-enable-search-words nil
   lsp-bridge-enable-auto-format-code nil)
  (setq lsp-bridge-default-mode-hooks '(c-mode-hook c++-mode-hook cmake-mode-hook java-mode-hook julia-mode-hook python-mode-hook ruby-mode-hook lua-mode-hook rust-mode-hook typescript-mode-hook css-mode-hook emacs-lisp-mode-hook lisp-interaction-mode-hook fortran-mode-hook sh-mode-hook bash-mode-hook ess-r-mode-hook verilog-mode-hook csharp-mode-hook))
  (global-lsp-bridge-mode))

(use-package realgud)

;;; Grammer
(use-package flycheck
  :defer nil
  :config
  (setq truncate-lines nil) ; 如果单行信息很长会自动换行
  :hook
  (prog-mode . flycheck-mode))

;;; Tree-sitter
(setq treesit-extra-load-path '("~/.emacs.d/tree-sitter/"))

;;; R
(use-package ess)

;;; Markdown
(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown")
  :config
  (setq prettify-symbols-mode t))

;;; Cmake
(use-package cmake-mode)

;;; Lua
(use-package lua-mode)

;;; Json
(use-package json-mode)

;;; Csv
(use-package csv-mode)

;; Quickrun
(use-package quickrun
  :bind ("C-<return>" . quickrun))

(provide 'init-lsp)
;;; init-lsp.el ends here
