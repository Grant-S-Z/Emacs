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
   (rust-mode . lsp-deferred)
   (lsp-mode . lsp-enable-which-key-integration)
   (lsp-completion-mode . grant/lsp-mode-setup-completion))
  :custom
  (lsp-keymap-prefix "C-c l")
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

(use-package lsp-ui
  :custom
  (lsp-ui-sideline-enable nil)
  (lsp-ui-peek-enable t)
  (lsp-ui-doc-enable t))

(use-package lsp-treemacs)

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

;; Cape
(use-package cape
  :init
  ;; (add-hook 'completion-at-point-functions #'cape-dabbrev) ;; word from current buffers
  (add-hook 'completion-at-point-functions #'cape-elisp-block) ;; elisp in org babel
  (add-hook 'completion-at-point-functions #'cape-file) ;; file path
  )

;; Flycheck
(use-package flycheck
  :config
  (setq truncate-lines nil)
  (setq flycheck-python-pycompile-executable "~/miniconda3/bin/python3")
  (setq flycheck-python-ruff-executable "~/.local/bin/ruff")
  :hook
  (prog-mode . flycheck-mode))

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

;; Yaml
(use-package yaml-mode)

;; Csv
(use-package csv-mode)

(provide 'init-lsp)
;;; init-lsp.el ends here
