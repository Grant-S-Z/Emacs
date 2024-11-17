;;; init-lsp.el --- for languages
;;; Commentary:
;;; Code:
;;; LSP
;; Eglot
(require 'eglot)
(global-set-key (kbd "C-x C-l") 'eglot)
(global-set-key (kbd "C-c <RET>") 'eglot-format)
(global-set-key (kbd "C-x C-p") 'eglot-find-declaration)
(add-to-list 'eglot-server-programs
	     '((c-mode c++-mode) . ("clangd")))
(add-to-list 'eglot-server-programs
             '((python-mode) . ("~/.local/bin/pyright-langserver" "--stdio")))
(add-to-list 'eglot-server-programs
	     '((TeX-mode) . ("texlab")))

(add-hook 'c-mode-hook 'eglot-ensure)
(add-hook 'c++-mode-hook 'eglot-ensure)
(add-hook 'python-mode-hook 'eglot-ensure)
(add-hook 'rust-mode-hook 'eglot-ensure)
(add-hook 'TeX-mode-hook 'eglot-ensure)

;; Corfu
(use-package corfu
  :hook ((emacs-lisp-mode . corfu-mode)
	 (lisp-interaction-mode . corfu-mode)
	 (c-mode . corfu-mode)
	 (c++-mode . corfu-mode)
	 (python-mode . corfu-mode)
	 (rust-mode . corfu-mode)
	 (TeX-mode . corfu-mode))
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

(use-package eldoc-box
  :hook (emacs-lisp-mode . eldoc-box-hover-at-point-mode)
  :config
  (add-hook 'eglot-managed-mode-hook #'eldoc-box-hover-at-point-mode)
  (setq eldoc-box-cleanup-interval 2))

;; Quickrun
(use-package quickrun
  :bind ("C-<return>" . quickrun)
  :config
  (setq quickrun-timeout-seconds nil) ;; no time limit
  )

;;; Languages
;; Python basic settings
(setq python-interpreter "~/miniconda3/bin/python3")
(setq python-shell-interpreter "~/miniconda3/bin/python3")
(setq python-indent-guess-indent-offset t)
(setq python-indent-guess-indent-offset-verbose nil)
(setq python-shell-completion-native-enable t)
(setq doom-modeline-env-python-executable "~/miniconda3/bin/python3") ;; doom-modeline python version

(use-package numpydoc ;; numpydoc to generate doc
  :bind ("C-x C-n" . numpydoc-generate)
  :config
  (setq numpydoc-insert-examples-block nil)
  (setq numpydoc-insert-return-without-typehint t))

(quickrun-add-command "python/base" ;; quickrun
  '((:command . "~/miniconda3/bin/python3")
    (:exec . ("%c %s"))
    (:tempfile . nil)
    (:description . "Run Python..."))
  :default "python")

(quickrun-add-command "python/hep"
  '((:command . "~/miniconda3/envs/hep/bin/python3")
    (:exec . ("%c %s"))
    (:tempfile . nil)
    (:description . "Run Python in hep...")))

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
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown")
  :config
  (setq prettify-symbols-mode t))

;; Cmake
(use-package cmake-mode)

(provide 'init-lsp)
;;; init-lsp.el ends here
