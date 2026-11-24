;;; init-package.el --- package settings  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
;;; Basic
(which-key-mode 1) ; built-in

;; Restart Emacs from inside Emacs.
(use-package restart-emacs
  :bind (("C-c r" . restart-emacs)))

;; Drag lines and regions with M-p and M-n.
(use-package drag-stuff
  :bind (("M-p" . drag-stuff-up)
	 ("M-n" . drag-stuff-down)))

;; Context actions on minibuffer candidates and other targets.
(use-package embark
  :bind ("C-." . embark-act))

;; Search and navigation commands built on completion.
(use-package consult
  :bind (("C-s" . consult-line)
         ("C-x b" . consult-buffer)
         ("C-c f" . consult-ripgrep)))

;; Embark actions for Consult results.
(use-package embark-consult)

;; Jump to directories and files from the minibuffer.
(use-package consult-dir
  :bind (("C-x C-d" . consult-dir)
         :map minibuffer-local-completion-map
         ("C-x C-d" . consult-dir)
         ("C-x C-j" . consult-dir-jump-file)))

;; Small editing, file, and buffer commands.
(use-package crux
  :bind (("C-a" . crux-move-beginning-of-line)
	 ("C-x ," . crux-find-user-init-file)
	 ("C-S-d" . crux-duplicate-current-line-or-region)
	 ("C-S-k" . crux-smart-kill-line)
	 ("C-c C-k" . crux-kill-other-buffers)
	 ("C-c C-d" . crux-delete-file-and-buffer)))

;; Snippet expansion engine.
(use-package yasnippet
  ;; Load yasnippet on idle instead of blocking startup.
  :init (run-with-idle-timer 1 nil #'yas-global-mode))

;; Ready-made snippets for many major modes.
(use-package yasnippet-snippets
  :after yasnippet)

;; Jump to a character or word by typing a key.
(use-package avy
  :bind
  (("C-;" . avy-goto-char-timer)))

;; Restore point when revisiting a file.
(use-package saveplace
  :hook (after-init . save-place-mode))

;; Git porcelain inside Emacs.
(use-package magit
  :bind ("C-x g" . magit))

;; Edit browser text fields in Emacs.
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

;; Expand the region semantically.
(use-package expand-region
  :bind ("M-=" . er/expand-region))

;; Show VCS changes in the fringe.
(use-package diff-hl
  :hook ((prog-mode . diff-hl-mode)
         (LaTeX-mode . diff-hl-mode)
         (org-mode . diff-hl-mode)))

;; Trim trailing whitespace only on edited lines.
(use-package ws-butler
  :hook (prog-mode . ws-butler-mode))

;; Save visited buffers automatically after a short idle interval.
(setq auto-save-visited-interval 5
      save-silently t)
(auto-save-visited-mode 1)

;; Insert spaces between CJK and Latin text.
(use-package pangu-spacing
  :init (global-pangu-spacing-mode 1)
  :config
  (setq pangu-spacing-real-insert-separtor t))

;; More informative help buffers.
(use-package helpful
  :bind
  ([remap describe-function] . #'helpful-callable)
  ([remap describe-variable] . #'helpful-variable))


;;; Daily packages
;; Browse and read a Calibre e-book library.
(use-package calibredb
  :config
  (setq calibredb-root-dir "~/org/books/")
  (setq calibredb-db-dir (expand-file-name "metadata.db" calibredb-root-dir)))

;; Read EPUB files in Emacs.
(use-package nov
  :mode ("\\.epub\\'" . nov-mode))

;; Translate text through online engines.
(use-package gt
  :bind ("C-c g" . gt-translate)
  :config
  (setq gt-langs '(en zh))
  (setq gt-default-translator (gt-translator :engines (gt-youdao-dict-engine)))
  (setq gt-taker-pick 'paragraph))

;;; Music
;; Play music and media with mpv.
(use-package empv
  :custom
  (empv-action-handler 'completing-read)
  (empv-audio-dir '("~/music/free/"))
  :config
  (with-eval-after-load 'embark
    (empv-embark-initialize-extra-actions)))

;; MaiMemo vocabulary service (token in ~/.config/memo-api.json).
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
