;;; init-lsp.el --- for languages
;;; Commentary:
;;; Code:
;;; LSP
(require 'eglot)

(add-to-list 'eglot-server-programs
	     '((c-mode c++-mode) . ("clangd")))
(add-to-list 'eglot-server-programs
             '(python-mode . ("~/.local/bin/pyright-langserver" "--stdio")))
(add-to-list 'eglot-server-programs
	     '(TeX-mode . ("texlab")))

(add-hook 'c-mode-hook 'eglot-ensure)
(add-hook 'c++-mode-hook 'eglot-ensure)
(add-hook 'python-mode-hook 'eglot-ensure)
(add-hook 'TeX-mode-hook 'eglot-ensure)

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
	 (c-mode . corfu-mode)
	 (c++-mode . corfu-mode)
	 (python-mode . corfu-mode)
	 (rust-mode . corfu-mode)
	 (TeX-mode . corfu-mode)
	 (org-mode . corfu-mode))
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

;; Quickrun
(use-package quickrun
  :bind ("C-<return>" . quickrun)
  :config
  (setq quickrun-timeout-seconds nil) ;; no time limit
  )

;;; Languages
;; Python basic settings
(setq python-path "~/miniconda3/bin/python3")
(setq python-interpreter python-path)
(setq python-shell-interpreter python-path)
(setq doom-modeline-env-python-executable python-path) ;; modeline

(setq python-indent-guess-indent-offset t)
(setq python-indent-guess-indent-offset-verbose nil)
(setq python-shell-completion-native-enable t)

(use-package numpydoc ;; numpydoc to generate doc
  :bind ("C-x C-n" . numpydoc-generate)
  :config
  (setq numpydoc-insert-examples-block nil)
  (setq numpydoc-insert-return-without-typehint t))

(quickrun-add-command "python/base" ;; quickrun
  '((:command . "~/miniconda3/bin/python3")
    (:exec . ("%c %s"))
    (:tempfile . nil)
    (:description . "Run Python/base ..."))
  :default "python")

(quickrun-add-command "python/hep"
  '((:command . "~/miniconda3/envs/hep/bin/python3")
    (:exec . ("%c %s"))
    (:tempfile . nil)
    (:description . "Run Python/hep ...")))

;; C++ and ROOT
(quickrun-add-command "c++/c1z" ;; quickrun
  '((:command . "clang++")
    (:exec    . ("%c -std=c++1z %o -o %e %s"
		 "%e %a"))
    (:remove  . ("%e")))
  :default "c++")

;; Rust
(use-package rust-mode)

;; Markdown
(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)))

;; Cmake
(use-package cmake-mode)

(provide 'init-lsp)
;;; init-lsp.el ends here
