;;; init-lsp.el --- for languages  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
;;; TS
(use-package treesit-auto
  :hook (prog-mode . global-treesit-auto-mode)
  :config
  (setq treesit-auto-install 'prompt)
  (setq treesit-font-lock-level 4)
  ;; Prefer ready tree-sitter modes; keep classic modes as fallback.
  (treesit-auto-add-to-auto-mode-alist))


;;; Format
(use-package format-all
  :bind ("C-c C-<return>" . format-all-buffer))


;;; Corfu
;; Cape
(use-package cape
  :init
  (add-hook 'completion-at-point-functions #'cape-elisp-block) ;; elisp in org babel
  (add-hook 'completion-at-point-functions #'cape-file) ;; file path
  )

;; Corfu
(use-package corfu
  :hook ((emacs-lisp-mode . corfu-mode)
	 (lisp-interaction-mode . corfu-mode)
	 (prog-mode . corfu-mode)
	 (TeX-mode . corfu-mode)
		 (message-mode . corfu-mode))
  :bind (:map corfu-map
              ("M-n" . corfu-next)
              ("M-p" . corfu-previous))
  :config
  (setq corfu-auto t
        corfu-auto-prefix 1
        corfu-auto-delay 0.2
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


;;; Eglot
(with-eval-after-load 'eglot
  ;; Ignore dynamic LSP file-watch registration; `eglot-ignored-server-capabilities'
  ;; does not intercept this path on macOS, where recursive watchers can exhaust kqueue.
  (cl-defmethod eglot-register-capability :around
      (_server (method (eql workspace/didChangeWatchedFiles)) _id &rest _params)
    "Disable recursive LSP file watchers to preserve file descriptors."
    nil)
;; Pyright 的分析解释器（与 Emacs 的 python-shell-interpreter 分开配置）。
;; 不设置时 Pyright 会按 PATH 自动选择，可能误用 Homebrew Python。
(setq-default eglot-workspace-configuration
              '(:python (:pythonPath "/Users/grant/miniconda3/bin/python3")))
(add-to-list 'eglot-server-programs
	     (list '(python-ts-mode python-mode)
		   (expand-file-name "~/miniconda3/bin/pyright-langserver") "--stdio"))
(add-to-list 'eglot-server-programs
             '((c-mode c-ts-mode c++-mode c++-ts-mode) . ("clangd"))))

;;; Languages
;; Python
(use-package python
  :init (defvar python-path "~/miniconda3/bin/python3")
  :mode ("\\.py\\'" . python-ts-mode)
  :hook ((python-ts-mode . eglot-ensure)
         (python-mode . eglot-ensure))
  :config
  ;; Python basic settings
  (setq python-interpreter python-path)
  (setq python-shell-interpreter python-path)

  (setq python-indent-guess-indent-offset t)
  (setq python-indent-guess-indent-offset-verbose nil)
  (setq python-shell-completion-native-enable t))

(use-package numpydoc ;; numpydoc to generate doc
  :bind ("C-x C-n" . numpydoc-generate)
  :config
  (setq numpydoc-insert-examples-block nil)
  (setq numpydoc-insert-return-without-typehint t))

;; Markdown
(use-package markdown-ts-mode
  :mode (("README\\.md\\'" . markdown-ts-mode)
         ("\\.md\\'" . markdown-ts-mode)))
(use-package markdown-ts-appear
  :hook ((markdown-ts-mode . markdown-ts-appear-mode)
	 (markdown-ts-mode . markdown-ts-appear-mathjax-mode)))
;; Cmake
(use-package cmake-mode)

;; C/C++
(add-hook 'c-mode-hook #'eglot-ensure)
(add-hook 'c-ts-mode-hook #'eglot-ensure)
(add-hook 'c++-mode-hook #'eglot-ensure)
(add-hook 'c++-ts-mode-hook #'eglot-ensure)

;; Csv
(use-package csv-mode)

;; Yaml
(use-package yaml-mode)

(provide 'init-lsp)
;;; init-lsp.el ends here
