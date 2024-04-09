;; eglot
(require 'eglot)

(add-to-list 'eglot-server-programs
	     '((c-mode c++-mode) . "clangd"))
(add-to-list 'eglot-server-programs
             '((python-mode python-ts-mode) . ("/Users/grant/anaconda3/bin/pyright-langserver" "--stdio")))

(add-hook 'c-mode-hook 'eglot-ensure)
(add-hook 'c++-mode-hook 'eglot-ensure)
(add-hook 'python-mode-hook 'eglot-ensure)

(use-package cape
  :ensure t
  :init
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-keyword)
  (add-to-list 'completion-at-point-functions #'cape-history))

(use-package corfu
  :ensure t
  :hook ((go-mode . corfu-mode)
	 (c++-mode . corfu-mode)
         (python-mode . corfu-mode)
	 (rjsx-mode . corfu-mode)
	 (emacs-lisp-mode . corfu-mode))
  :bind (:map corfu-map
              ("M-n" . corfu-next)
              ("M-p" . corfu-previous))
  :config
  (setq corfu-auto t
        corfu-auto-prefix 1
        corfu-auto-delay 0.1
        corfu-quit-no-match t
        corfu-quit-at-boundary t)
  (add-hook 'multiple-cursors-mode-enabled-hook (lambda () (corfu-mode -1)))
  (add-hook 'multiple-cursors-mode-disabled-hook (lambda () (corfu-mode 1))))

(use-package doom-themes
  :init (load-theme 'doom-one-light t))

(use-package modus-themes
  :init (load-theme 'modus-operandi-tritanopia t))

;; 主题随时间变化
(require 'theme-changer)
(change-theme 'doom-one-light 'doom-one)

;;; Ebook
(use-package djvu)
(use-package nov
  :init
  (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode)))
(use-package calibredb
  :config
  (setq calibredb-root-dir "~/Calibre Library")
  (setq calibredb-db-dir (expand-file-name "metadata.db" calibredb-root-dir))
  (setq calibredb-library-alist '(("~/Calibre"))))

;;; Python Conda
(require 'conda)
(conda-env-initialize-interactive-shells)
(conda-env-autoactivate-mode t)
(add-hook 'conda-postactivate-hook
	  (lambda ()
	    (lsp-bridge-restart-process)))

(use-package auto-package-update ;; package 更新
  :config
  (setq auto-package-update-delete-old-versions t
	auto-package-update-hide-results t))

(use-package time ;; 显示时间
  :init
  (setq display-time-24hr-format t ;; 显示时间
	display-time-day-and-date t) ;; 显示日期
  :config
  (display-time-mode t))

(use-package dired-subtree)

;;; Chatgpt
(use-package chatgpt
  :quelpa (chatgpt :fetcher github :repo "joshcho/ChatGPT.el")
  :bind ("C-c q" . chatgpt-query)
  :config
  (setq chatgpt-code-query-map
      '(
        ;; ChatGPT.el defaults, string for each shortcut
        ("bug" . "There is a bug in the following, please help me fix it.")
        ("doc" . "Please write the documentation for the following.")
        ("improve" . "Please improve the following.")
        ("understand" . "What is the following?")
        ("refactor" . "Please refactor the following.")
        ("suggest" . "Please make suggestions for the following.")
        ;; your shortcut
        ("prompt-name" . "My custom prompt."))))

;;; Translate
(use-package immersive-translate
  :config
  (setq immersive-translate-backend 'chatgpt
	immersive-translate-chatgpt-host "api.openai.com"))

;;; A quelpa example
(use-package org-popup-posframe
  :quelpa (org-popup-posframe :fetcher github :repo "A7R7/org-popup-posframe")
  :defer nil
  :init (org-popup-posframe-mode 1))

(require 'quelpa)
(require 'quelpa-use-package) ;; 结合 use-package 与 quelpa, 可直接使用 github 上的 package
