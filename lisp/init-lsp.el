;;; init-lsp.el --- for languages
;;; Commentary:
;;; Code:

;;; Language Server
;; lsp-bridge
(use-package lsp-bridge
  :load-path "~/.emacs.d/site-lisp/lsp-bridge"
  :init (global-lsp-bridge-mode)
  :bind (("C-x C-l" . lsp-bridge-mode)
	 ("C-c <RET>" . lsp-bridge-code-format))
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
  ;;; Languages
  ;; c
  (setq lsp-bridge-c-lsp-server "clangd")
  ;; org
  (setq lsp-bridge-enable-org-babel t)
  ;; LaTeX
  (setq lsp-bridge-tex-lsp-server "texlab")
  ;; Else
  (setq lsp-bridge-default-mode-hooks (remove 'org-mode-hook lsp-bridge-default-mode-hooks))
  ;(add-hook 'find-file-hook #'lsp-bridge-restart-process) ;; 每进入一次其他文件，重启 lsp
  )

;;; Grammer
(use-package flycheck
  :init (global-flycheck-mode t))

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
  :init (setq markdown-command "multimarkdown"))

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
