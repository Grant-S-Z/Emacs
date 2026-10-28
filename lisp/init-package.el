;;; init-package.el --- for packages  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
;;; Basic
(which-key-mode 1) ;; which-key internally installed

(use-package benchmark-init
  :init (benchmark-init/activate))

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

(use-package atomic-chrome
  :config
  (atomic-chrome-start-server)
  (setq atomic-chrome-buffer-open-style 'full)
  (setq atomic-chrome-url-major-mode-alist
	'(("overleaf\\.com" . latex-mode))))

;;; UI operation
(use-package ace-window ;; change window
  :bind (("M-o" . 'ace-window)))

(use-package expand-region
  :bind ("C-c e" . er/expand-region))

(use-package wgrep
  :commands (wgrep-change-to-wgrep-mode))

(use-package diff-hl
  :hook ((prog-mode . diff-hl-mode)
         (LaTeX-mode . diff-hl-mode)
         (org-mode . diff-hl-mode)))

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

;;; MaiMemo (墨墨背单词开放 API，Token 与官方 CLI 共享 ~/.config/memo-api.json)
(use-package maimemo
  :ensure nil ;; 不在 MELPA，从 site-lisp 手动加载
  :load-path "~/.emacs.d/site-lisp/maimemo"
  :commands (maimemo-add-word
             maimemo-add-word-to-notepad
             maimemo-study-progress
             maimemo-today-items)
  :bind (("C-c m a" . maimemo-add-word)            ; 加入学习规划（C-u 提前复习）
         ("C-c m n" . maimemo-add-word-to-notepad) ; 加入云词本
         ("C-c m p" . maimemo-study-progress)      ; 今日学习进度
         ("C-c m t" . maimemo-today-items))        ; 今日单词列表
  )


(provide 'init-package)
;;; init-package.el ends here
