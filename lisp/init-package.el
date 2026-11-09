;;; init-package.el --- package settings  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
;;; Basic
(which-key-mode 1) ; built-in

(use-package restart-emacs
  :bind (("C-c r" . restart-emacs)))

(use-package drag-stuff
  :bind (("M-p" . drag-stuff-up)
	 ("M-n" . drag-stuff-down)))

(use-package embark
  :bind ("C-." . embark-act))

(use-package consult
  :bind (("C-s" . consult-line)
         ("C-x b" . consult-buffer)
         ("C-c f" . consult-ripgrep)))

(use-package embark-consult)

(use-package wgrep
  :commands (wgrep-change-to-wgrep-mode))

(use-package consult-dir
  :bind (("C-x C-d" . consult-dir)
         :map minibuffer-local-completion-map
         ("C-x C-d" . consult-dir)
         ("C-x C-j" . consult-dir-jump-file)))

(use-package crux
  :bind (("C-a" . crux-move-beginning-of-line)
	 ("C-x ," . crux-find-user-init-file)
	 ("C-S-d" . crux-duplicate-current-line-or-region)
	 ("C-S-k" . crux-smart-kill-line)
	 ("C-c C-k" . crux-kill-other-buffers)
	 ("C-c C-d" . crux-delete-file-and-buffer)))

(use-package yasnippet
  ;; Loading yasnippet costs ~100 ms, so enable it on the first idle moment
  ;; instead of blocking startup; it still turns on in every existing buffer.
  :init (run-with-idle-timer 1 nil #'yas-global-mode))

(use-package yasnippet-snippets
  :after yasnippet)

(use-package avy
  :bind
  (("C-;" . avy-goto-char-timer)))

(use-package saveplace
  :hook (after-init . save-place-mode))

(use-package magit
  :bind ("C-x g" . magit))

(use-package atomic-chrome
  :config
  (atomic-chrome-start-server)
  (setq atomic-chrome-buffer-open-style 'full)
  (setq atomic-chrome-url-major-mode-alist
	'(("overleaf\\.com" . latex-mode))))

;;; UI operation
;; Switch window
(global-set-key (kbd "M-o") #'other-window)
(global-set-key (kbd "M-O") #'other-window-backward)

(use-package expand-region
  :bind ("M-=" . er/expand-region))

(use-package diff-hl
  :hook ((prog-mode . diff-hl-mode)
         (LaTeX-mode . diff-hl-mode)
         (org-mode . diff-hl-mode)))

(use-package ws-butler
  :hook (prog-mode . ws-butler-mode))

;; super-save was dropped in favour of the built-in auto-save-visited-mode.
(setq auto-save-visited-interval 5
      save-silently t)
(auto-save-visited-mode 1)

(use-package pangu-spacing ; spacing between CJK and Latin
  :init (global-pangu-spacing-mode 1)
  :config
  (setq pangu-spacing-real-insert-separtor t))

(use-package helpful
  :bind
  ([remap describe-function] . #'helpful-callable)
  ([remap describe-variable] . #'helpful-variable))


;;; Daily packages
;; Calibre
(use-package calibredb
  :config
  (setq calibredb-root-dir "~/org/books/")
  (setq calibredb-db-dir (expand-file-name "metadata.db" calibredb-root-dir)))

(use-package nov
  :mode ("\\.epub\\'" . nov-mode))

;; Appine is disabled for now; PDFs use pdf-tools instead.
;; (when *is-mac*
;;   (use-package appine
;;     :vc (:url "https://github.com/chaoswork/appine")
;;     :commands (appine appine-open-url appine-open-file)
;;     :bind (("C-c A a" . appine)
;;            ("C-c A u" . appine-open-url)
;;            ("C-c A o" . appine-open-file))))

;; Translator
(use-package gt
  :bind ("C-c g" . gt-translate)
  :config
  (setq gt-langs '(en zh))
  (setq gt-default-translator (gt-translator :engines (gt-youdao-dict-engine)))
  (setq gt-taker-pick 'paragraph))

;;; MaiMemo (open API; the token is shared with the official CLI
;;; through ~/.config/memo-api.json)
(use-package maimemo
  :ensure nil                            ; not on MELPA, loaded from site-lisp
  :load-path "~/.emacs.d/site-lisp/maimemo"
  :commands (maimemo-add-word
             maimemo-add-word-to-notepad
             maimemo-study-progress
             maimemo-today-items)
  :bind (("C-c m a" . maimemo-add-word)            ; add to study plan (C-u: review early)
         ("C-c m n" . maimemo-add-word-to-notepad) ; add to cloud notepad
         ("C-c m p" . maimemo-study-progress)      ; today's progress
         ("C-c m t" . maimemo-today-items))        ; today's word list
  )


(provide 'init-package)
;;; init-package.el ends here
