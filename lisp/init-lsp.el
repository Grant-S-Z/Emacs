;;; init-lsp.el --- for languages  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
;;; DOCS
;; (use-package devdocs)


;;; TS
(use-package treesit-auto
  :demand t
  :config
  (setq treesit-auto-install 'prompt)
  (setq treesit-font-lock-level 4))


;;; Check
;; (use-package flycheck
;;   :hook (prog-mode . flycheck-mode))


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
  :init (global-corfu-mode)
  ;; :hook ((emacs-lisp-mode . corfu-mode)
  ;; 	 (lisp-interaction-mode . corfu-mode)
  ;; 	 (prog-mode . corfu-mode)
  ;; 	 (TeX-mode . corfu-mode)
  ;; 	 (org-mode . corfu-mode)
  ;; 	 (message-mode . corfu-mode))
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


;;; Lsp-mode
;; (use-package lsp-mode
;;   :init
;;   (defun grant/lsp-mode-setup-completion ()
;;     (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
;; 	  '(orderless)))
;;   :commands (lsp lsp-deferred)
;;   :hook
;;   ((c++-mode . lsp-deferred)
;;    (c-mode . lsp-deferred)
;;    (python-mode . lsp-deferred)
;;    (python-ts-mode .lsp-deferred)
;;    (TeX-mode . lsp-deferred)
;;    (lsp-mode . lsp-enable-which-key-integration)
;;    (lsp-completion-mode . grant/lsp-mode-setup-completion))
;;   :custom
;;   (lsp-keymap-prefix "C-x C-l")
;;   (lsp-file-watch-threshold 500)
;;   ;; Completion provider
;;   (lsp-completion-provider :capf)
;;   ;; Python ruff
;;   (lsp-ruff-python-path "~/miniconda3/bin/python3")
;;   (lsp-ruff-server-command '("~/miniconda3/bin/ruff" "server")))

;; (use-package lsp-pyright
;;   :custom (lsp-pyright-langserver-command "~/miniconda3/bin/pyright")
;;   :hook (python-ts-mode . (lambda ()
;;                             (require 'lsp-pyright)
;;                             (lsp-deferred))))

;; (use-package lsp-ui
;;   :after lsp-mode
;;   :custom
;;   (lsp-ui-sideline-enable nil)
;;   (lsp-ui-peek-enable t)
;;   (lsp-ui-doc-enable t))

;; (use-package lsp-treemacs)

;; (use-package dap-mode
;;   :config
;;   (dap-mode 1)
;;   (dap-ui-mode 1)
;;   (dap-tooltip-mode 1)
;;   (tooltip-mode 1)
;;   (dap-ui-controls-mode 1)
;;   ;; Python
;;   (require 'dap-python)
;;   (setq dap-python-debugger 'debugpy)
;;   ;; C
;;   (require 'dap-gdb-lldb)
;;   ;(setq dap-lldb-debug-program "/usr/bin/lldb")
;;   )


;;; Lsp-bridge
;; (add-to-list 'load-path "~/.emacs.d/site-lisp/lsp-bridge/")
;; (require 'lsp-bridge)
;; (global-lsp-bridge-mode)


;;; Eglot
(require 'eglot)
(add-to-list 'eglot-server-programs
	     '((python-ts-mode python-mode) . ("~/miniconda3/bin/pyright-langserver" "--stdio")))
(add-to-list 'eglot-server-programs
             '((c-mode c-ts-mode c++-mode c++-ts-mode) . ("clangd")))
;; (add-to-list 'eglot-server-programs
;;              `((python-ts-mode python-mode)
;;                . (,(expand-file-name "~/miniconda3/bin/pyright-langserver") "--stdio")))

;; (use-package eldoc-box
;;   :after eglot
;;   :hook (prog-mode . eldoc-box-hover-at-point-mode)
;;   :custom
;;   (eldoc-idle-delay 1.5))


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

(use-package hdf5-viewer
  :mode ("\\.h5\\'" . hdf5-viewer-find-file-mode)
  :custom
  (hdf5-viewer-python-command "~/miniconda3/bin/python3"))

;; Markdown
(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)))
(use-package grip-mode ;; preview md
  :init
  (defvar grip-theme 'auto)

  ;; Compatibility patch: mdopen 0.5.x doesn't support `--theme=...`, while
  ;; some grip-mode versions still pass it.
  (defun my/grip--filter-mdopen-start-process-args (args)
    "Remove unsupported `--theme=' arg when launching mdopen."
    (let* ((program (nth 2 args))
           (is-mdopen (and (stringp program)
                           (string= (file-name-nondirectory program) "mdopen"))))
      (if (not is-mdopen)
          args
        (let ((prefix (seq-take args 3))
              (rest (nthcdr 3 args))
              (filtered nil))
          (dolist (arg rest)
            (unless (and (stringp arg)
                         (string-prefix-p "--theme=" arg))
              (push arg filtered)))
          (append prefix (nreverse filtered))))))
  :custom
  (grip-command 'mdopen)
  (grip-preview-in-webkit nil)
  :config
  (advice-add 'start-process :filter-args #'my/grip--filter-mdopen-start-process-args))

;; ;; Lisp
;; (use-package geiser
;;   :hook (scheme-mode . geiser-mode))
;; (use-package geiser-mit)

;; (use-package slime
;;   :custom
;;   (inferior-lisp-program "sbcl"))

;; Cmake
(use-package cmake-mode)

;; C/C++
(add-hook 'c-mode-hook #'eglot-ensure)
(add-hook 'c-ts-mode-hook #'eglot-ensure)
(add-hook 'c++-mode-hook #'eglot-ensure)
(add-hook 'c++-ts-mode-hook #'eglot-ensure)

;; ;; Lua
;; (use-package lua-mode)

;; Csv
(use-package csv-mode)

;; Yaml
(use-package yaml-mode)

(provide 'init-lsp)
;;; init-lsp.el ends here
