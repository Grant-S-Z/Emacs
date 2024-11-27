;;; init-package.el --- for packages
;;; Commentary:
;;; Code:
;;; Basic
(use-package restart-emacs ;; restart emacs
  :bind (("C-c r" . restart-emacs)))

(use-package drag-stuff ;; move selected region
  :bind (("M-p" . drag-stuff-up)
	 ("M-n" . drag-stuff-down)))

(use-package embark ;; act in minibuffer
  :bind
  (("C-." . embark-act)))

(use-package consult ;; search
  :bind (("C-s" . consult-line)))

(use-package embark-consult) ;; act in consult

(use-package crux ;; crux bindings
  :bind (("C-a" . crux-move-beginning-of-line)
	 ("C-c ^" . crux-top-join-line)
	 ("C-x ," . crux-find-user-init-file)
	 ("C-S-d" . crux-duplicate-current-line-or-region)
	 ("C-S-k" . crux-smart-kill-line)
	 ("C-c C-k" . crux-kill-other-buffers)
	 ("C-c C-d" . crux-delete-file-and-buffer)))

(use-package yasnippet ;; snippets
  :init (yas-global-mode t)
  :hook (prog-mode . yas-minor-mode))

(use-package yasnippet-snippets ;; regular snippets
  :after yasnippet)

(use-package which-key ;; key binding tips
  :defer nil
  :config (which-key-mode))

(use-package avy ;; goto directly
  :bind
  (("C-;" . avy-goto-char-timer)))

;; Git
(use-package magit)

;; Chinese calendar
(require 'cal-china-x)

(setq calendar-chinese-all-holidays-flag t)
(setq calendar-mark-holidays-flag t)
(setq cal-china-x-important-holidays cal-china-x-chinese-holidays)

(setq holiday-local-holidays
      '((holiday-lunar 1 10 "Father's birthday" 0)
	(holiday-lunar 2 20 "Mother's birthday" 0)))

(setq calendar-holidays
    (append cal-china-x-important-holidays
	    holiday-general-holidays
	    holiday-local-holidays))

;;; UI operation
(use-package ace-window ;; change window
  :bind (("M-o" . 'ace-window)))

(use-package dimmer ;; dimmer window unfocused
  :hook (prog-mode . dimmer-mode)
  :config
  (dimmer-configure-which-key)
  (dimmer-configure-posframe)
  (dimmer-configure-org))

(use-package ws-butler ;; remove space automatically
  :hook (prog-mode . ws-butler-mode))

(use-package super-save ;; save automatically
  :diminish
  :defer 0.5
  :config
  (super-save-mode 1)
  (setq super-save-auto-save-when-idle t)
  (setq save-silently t))

(use-package pangu-spacing ;; comfortable space between English and Chinese
  :init (global-pangu-spacing-mode 1)
  :config
  (setq pangu-spacing-real-insert-separtor t))

(use-package helpful ;; help
  :bind
  ([remap describe-function] . #'helpful-callable)
  ([remap describe-variable] . #'helpful-variable))

(use-package writeroom-mode ;;; center texts
  :hook ((org-mode . writeroom-mode)
	 (nov-mode . writeroom-mode))
  :custom
  (writeroom-maximize-window nil)
  (writeroom-mode-line t)
  (writeroom-global-effects '(writeroom-set-alpha
			      writeroom-set-menu-bar-lines
			      writeroom-set-tool-bar-lines
			      writeroom-set-vertical-scroll-bars
			      writeroom-set-bottom-divider-width)))

;;; Daily packages
;; Bongo, a music player
(use-package bongo
  :commands bongo-playlist
  :bind (("C-c m m" . bongo-playlist)
	 ("C-c m ," . bongo-pause/resume)
	 ("C-c m ." . bongo-start/stop)
	 ("C-c m n" . bongo-play-next)
	 ("C-c m p" . bongo-play-previous))
  :custom
  (bongo-enabled-backends '(mpv))
  (bongo-custom-backend-matchers '((mpv local-file "m4a" "opus")))
  (bongo-default-directory "~/Music/MusicFree/")
  (bongo-logo nil)
  (bongo-insert-album-covers nil)
  (bongo-album-cover-size 100)
  (bongo-mode-line-indicator-mode nil)
  (bongo-header-line-mode nil)
  )

;; Calculator
(use-package literate-calc-mode
  :mode ("calc" . literate-calc-mode))

;; Reader
(use-package nov
  :mode ("\\.epub\\'" . nov-mode)
  :config
  (setq nov-text-width (- writeroom-width 10))
  )
(defun my-nov-font-setup ()
  (face-remap-add-relative 'variable-pitch
			   :family "Alegreya"
			   :height 1.5
			   ))
(add-hook 'nov-mode-hook 'my-nov-font-setup)

;; Nov notes
(use-package org-remark
  :bind (("C-c n m" . org-remark-mark)
	 ("C-c n ]" . org-remark-view-next)
	 ("C-c n [" . org-remark-view-prev))
  :hook (nov-mode . org-remark-nov-mode))

;; Calibre
(use-package calibredb
  :config
  (setq calibredb-root-dir "~/org/books/")
  (setq calibredb-db-dir (expand-file-name "metadata.db" calibredb-root-dir)))

;; Translator
(use-package fanyi
  :bind ("C-c f" . fanyi-dwim)
  :custom
  (fanyi-providers '(fanyi-haici-provider ;; haici
                     fanyi-youdao-thesaurus-provider ;; youdao
                     fanyi-longman-provider ;; longman
		     ))
  (fanyi-verbose nil))

;; Rss
(use-package elfeed
  :config
  (setq elfeed-feeds
	'(("https://arxiv.org/rss/hep-ex" study physics)
	  ("https://arxiv.org/rss/hep-ph" study physics)
	  ("https://arxiv.org/rss/hep-th" study physics)
	  ("https://root-forum.cern.ch/posts.rss" root)
	  ("https://sachachua.com/blog/category/emacs-news/feed/" news emacs)
	  ("https://emacs-china.org/posts.rss" emacs)
	  ("https://news.ycombinator.com/rss" tech news)
	  ("https://v2ex.com/index.xml" tech news)
	  ))
  (setq elfeed-show-mode-hook
      (lambda ()
	(set-face-attribute 'variable-pitch (selected-frame) :font (font-spec :family "Iosevka" :size 18))
	(setq fill-column 100)))
  )

(use-package elfeed-summary
  :bind ("C-c e" . elfeed-summary)
  :config
  (setq elfeed-summary-other-window t)
  (setq elfeed-summary-settings
	'((group (:title . "Physics")
		 (:elements
		  (query . (study physics))))
	  (group (:title . "ROOT")
		 (:elements
		  (query . (root))))
	  (group (:title . "Emacs")
		 (:elements
		  (query . (and emacs (not '(news org))))
		  (group (:title . "News")
			 (:elements
			  (query . (and news emacs))))))
	  (group (:title . "News")
		 (:elements
		  (query . (and news (not '(tech eco emacs))))
		  (group (:title . "Tech")
			 (:elements
			  (query . (and tech news))))))
	  )))

;;; Chatgpt
(when *is-mac*
  (defun osx-get-keychain-password (account-name)
	"Gets ACCOUNT-NAME keychain password from OS X Keychain."
	(let ((cmd (concat "security 2>&1 >/dev/null find-generic-password -ga '" account-name "'")))
	  (let ((passwd (shell-command-to-string cmd)))
		(when (string-match (rx "\"" (group (0+ (or (1+ (not (any "\"" "\\"))) (seq "\\" anything)))) "\"") passwd)
		  (match-string 1 passwd)))))
  (use-package chatgpt-shell
    :load-path "~/.emacs.d/site-lisp/chatgpt-shell/"
    :bind
    (("C-c q" . chatgpt-shell)
     ("C-c d" . chatgpt-shell-explain-code)
     ("C-c p" . chatgpt-shell-prompt))
    :custom
    ((chatgpt-shell-api-url-base "https://api.gptsapi.net")
     (chatgpt-shell-openai-key
      (lambda ()
        ;; Here the openai-key should be the proxy service key.
	(osx-get-keychain-password "openai key"))))))

(provide 'init-package)
;;; init-package.el ends here
