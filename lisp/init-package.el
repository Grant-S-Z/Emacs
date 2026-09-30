;;; init-package.el --- for packages  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
;;; Basic
(which-key-mode 1) ;; which-key internally installed

(use-package restart-emacs ;; restart emacs
  :bind (("C-c r" . restart-emacs)))

(use-package drag-stuff ;; move selected region
  :bind (("M-p" . drag-stuff-up)
	 ("M-n" . drag-stuff-down)))

(use-package embark ;; act in minibuffer
  :bind ("C-." . embark-act))

(use-package consult ;; search
  :bind (("C-s" . consult-line)))

(use-package embark-consult) ;; act in consult

(use-package crux ;; crux bindings
  :bind (("C-a" . crux-move-beginning-of-line)
	 ("C-x ," . crux-find-user-init-file)
	 ("C-S-d" . crux-duplicate-current-line-or-region)
	 ("C-S-k" . crux-smart-kill-line)
	 ("C-c C-k" . crux-kill-other-buffers)
	 ("C-c C-d" . crux-delete-file-and-buffer))
  )

(use-package yasnippet ;; snippets
  :init (yas-global-mode t)
  :hook (prog-mode . yas-minor-mode))

(use-package yasnippet-snippets ;; regular snippets
  :after yasnippet)

(use-package avy ;; goto directly
  :bind
  (("C-;" . avy-goto-char-timer)))

(use-package saveplace
  :hook (after-init . save-place-mode))

;; Git
(use-package magit
  :bind ("C-x g" . magit))

(use-package openwith
  :init (openwith-mode t)
  :config
  (setq openwith-associations '(("\\.pdf\\'" "open" (file))
				("\\.epub\\'" "open" (file)))))

(use-package atomic-chrome
  :config
  (atomic-chrome-start-server)
  (setq atomic-chrome-buffer-open-style 'full)
  (setq atomic-chrome-url-major-mode-alist
	'(("overleaf\\.com" . latex-mode))))

;;; UI operation
(use-package ace-window ;; change window
  :bind (("M-o" . 'ace-window)))

(use-package ws-butler ;; remove space automatically
  :hook (prog-mode . ws-butler-mode))

;; super-save 已移除：用 Emacs 内置的 auto-save-visited-mode 替代
(setq auto-save-visited-interval 5 ;; 空闲 5 秒自动保存
      save-silently t)              ;; 保存时不在 echo area 提示
(auto-save-visited-mode 1)

(use-package pangu-spacing ;; comfortable space between English and Chinese
  :init (global-pangu-spacing-mode 1)
  :config
  (setq pangu-spacing-real-insert-separtor t))

(use-package helpful ;; help
  :bind
  ([remap describe-function] . #'helpful-callable)
  ([remap describe-variable] . #'helpful-variable))


;;; Daily packages
;; Calculator
;; (use-package literate-calc-mode
;;   :mode ("calc" . literate-calc-mode))

;; Calibre
(use-package calibredb
  :config
  (setq calibredb-root-dir "~/org/books/")
  (setq calibredb-db-dir (expand-file-name "metadata.db" calibredb-root-dir)))

(use-package nov)

;; Translator
(use-package gt
  :bind ("C-c g" . gt-translate)
  :config
  (setq gt-langs '(en zh))
  (setq gt-default-translator (gt-translator :engines (gt-youdao-dict-engine)))
  (setq gt-taker-pick 'paragraph))

(provide 'init-package)
;;; init-package.el ends here
