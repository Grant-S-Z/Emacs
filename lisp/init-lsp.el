;;; init-lsp.el --- language modes and LSP  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
;;; TS
;; Emacs 31 remaps classic modes to tree-sitter when `treesit-enabled-modes'
;; is set through Custom (hence `setopt', not `setq').
(require 'treesit)
;; `init-org', `init-tex' and `init-fun' use `string-trim' / `when-let*'.
(require 'subr-x)
(setq treesit-font-lock-level 4)
(when (boundp 'treesit-enabled-modes)
  (setopt treesit-enabled-modes t))

;;; Format
;; Format buffers with external formatters.
(use-package format-all
  :bind ("C-c C-<return>" . format-all-buffer))


;;; Corfu
;; Extra completion-at-point backends (files, Elisp in Org).
(use-package cape
  :init
  (add-hook 'completion-at-point-functions #'cape-elisp-block) ; elisp in org babel
  (add-hook 'completion-at-point-functions #'cape-file))

;; Popup completion UI for in-buffer completion.
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
  (corfu-popupinfo-mode)
  (corfu-indexed-mode)
  (dotimes (i 10) ; M-<num> selects entry <num>
    (define-key corfu-mode-map
                (kbd (format "M-%s" i))
                (kbd (format "C-%s <tab>" i)))))

;; Nerd icons in the Corfu completion margin.
(use-package nerd-icons-corfu
  :after corfu
  :init (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))


;;; Eglot
(with-eval-after-load 'eglot
  ;; Skip dynamic file-watch registration; `eglot-ignored-server-capabilities'
  ;; cannot block it, and recursive watchers exhaust kqueue on macOS.
  (cl-defmethod eglot-register-capability :around
      (_server (_method (eql workspace/didChangeWatchedFiles)) _id &rest _params)
    "Disable recursive LSP file watchers to preserve file descriptors."
    nil)
  ;; Give Pyright its own interpreter; otherwise it picks one from PATH and may
  ;; land on the Homebrew Python.
  (setq-default eglot-workspace-configuration
                (list :python (list :pythonPath
                                    (if *is-wsl*
                                        (expand-file-name "~/miniconda3/bin/python3")
                                      "/Users/grant/miniconda3/bin/python3"))))
  (add-to-list 'eglot-server-programs
               (list '(python-ts-mode python-mode)
                     (expand-file-name "~/miniconda3/bin/pyright-langserver") "--stdio"))
  (add-to-list 'eglot-server-programs
               '((c-mode c-ts-mode c++-mode c++-ts-mode) . ("clangd"))))


;;; Languages
;; Python mode with the Miniconda interpreter and Eglot.
(use-package python
  :init (defvar python-path "~/miniconda3/bin/python3")
  :mode ("\\.py\\'" . python-ts-mode)
  :hook (python-ts-mode . eglot-ensure)
  :config
  (setq python-interpreter python-path)
  (setq python-shell-interpreter python-path)

  (setq python-indent-guess-indent-offset t)
  (setq python-indent-guess-indent-offset-verbose nil)
  (setq python-shell-completion-native-enable t))

;; Tree-sitter major mode for Markdown.
(use-package markdown-ts-mode
  :mode (("README\\.md\\'" . markdown-ts-mode)
         ("\\.md\\'" . markdown-ts-mode)))
;; Reveal Markdown markup and math only at point.
(use-package markdown-ts-appear
  :hook ((markdown-ts-mode . markdown-ts-appear-mode)
         (markdown-ts-mode . markdown-ts-appear-mathjax-mode))
  :custom
  (markdown-ts-appear-mathjax-scale 0.9))

;; C/C++
(add-hook 'c-ts-mode-hook #'eglot-ensure)
(add-hook 'c++-ts-mode-hook #'eglot-ensure)

;; Major mode for editing CSV files.
(use-package csv-mode)

(provide 'init-lsp)
;;; init-lsp.el ends here
