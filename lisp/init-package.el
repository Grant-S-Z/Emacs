;;; init-package.el --- for packages
;;; Commentary:
;;; Code:
;;; Basic
(use-package benchmark-init ;; 统计 packages 耗时
  :init (benchmark-init/activate))

(use-package restart-emacs ;; 重启 emacs
  :bind (("C-c r" . restart-emacs)))

(use-package drag-stuff ;; 范围移动
  :bind (("M-p" . drag-stuff-up)
	 ("M-n" . drag-stuff-down)))

(use-package consult ;; 搜索
  :bind (("C-s" . consult-line)))

(use-package crux ;; 一些快捷键
  :bind (("C-a" . crux-move-beginning-of-line)
	 ("C-c ^" . crux-top-join-line)
	 ("C-x ," . crux-find-user-init-file)
	 ("C-S-d" . crux-duplicate-current-line-or-region)
	 ("C-S-k" . crux-smart-kill-line)
	 ("C-c C-k" . crux-kill-other-buffers)))

(use-package yasnippet ;; 设置 snippets
  :init (yas-global-mode t)
  :hook (prog-mode . yas-minor-mode))

(use-package yasnippet-snippets ;; 提供常用 snippets
  :after yasnippet)

(use-package which-key ;; 快捷键提示
  :defer nil
  :config (which-key-mode))

;; avy
(use-package avy
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
(use-package ace-window ;; 分屏切换
  :bind (("M-o" . 'ace-window)))

(use-package dimmer ;; 黯淡分屏
  :hook (prog-mode . dimmer-mode)
  :config
  (dimmer-configure-company-box)
  (dimmer-configure-which-key)
  (dimmer-configure-posframe)
  (dimmer-configure-org))

(use-package ws-butler ;; 自动清除空格
  :hook (prog-mode . ws-butler-mode))

(use-package super-save ;; 自动保存
  :diminish
  :defer 0.5
  :config
  (super-save-mode 1)
  (setq super-save-auto-save-when-idle t)
  (setq save-silently t))

(use-package pangu-spacing ;; comfortable space
  :init
  (global-pangu-spacing-mode 1)
  :config
  (setq pangu-spacing-real-insert-separtor t))

(use-package writeroom-mode ;;; center texts
  :defer nil
  :hook ((org-mode . writeroom-mode)
	 (nov-mode . writeroom-mode))
  :custom
  (writeroom-maximize-window nil)
  (writeroom-global-effects '(writeroom-set-alpha
			      writeroom-set-menu-bar-lines
			      writeroom-set-tool-bar-lines
			      writeroom-set-vertical-scroll-bars
			      writeroom-set-bottom-divider-width)))

(use-package keycast ;; key cast
  ;; :init (keycast-header-line-mode 1)
  :config
  (push '(org-self-insert-command nil nil) keycast-substitute-alist)
  (push '(self-insert-command nil nil) keycast-substitute-alist)
  (push '(mouse-drag-region nil nil) keycast-substitute-alist)
  (push '(mouse-set-point nil nil) keycast-substitute-alist)
  (push '(lsp-ui-doc--handle-mouse-movement nil nil) keycast-substitute-alist)
  (push '(mac-mwheel-scroll nil nil) keycast-substitute-alist))

(use-package helpful ;; help
  :bind
  ([remap describe-function] . #'helpful-callable)
  ([remap describe-variable] . #'helpful-variable))

;;; Daily packages
;; Hugo
(use-package easy-hugo
  :bind ("C-c b" . easy-hugo)
  :config
  (setq easy-hugo-basedir "~/Code/GrantSite/") ;; 网站本地文件根目录
  (setq easy-hugo-url "https://Grant-S-Z.github.io/GrantSite") ;; url 路径
  (setq easy-hugo-sshdomain "Grant-S-Z.github.io")
  (setq easy-hugo-previewtime "300")
  (setq easy-hugo-default-ext ".org"))

(use-package ox-hugo
  :config
  (setq org-hugo-base-dir "~/Code/GrantSite/")
  (setq org-hugo-section "post"))

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
  (bongo-mode-line-indicator-mode nil))

;; Mpv
(use-package mpv
  :config
  (defun org-mpv-complete-link (&optional arg)
    (replace-regexp-in-string
     "file:" "mpv:"
     (org-link-complete-file arg)
     t t))
  (org-link-set-parameters "mpv"
			   :follow #'mpv-play :complete #'org-mpv-complete-link)

  (defun org-metareturn-insert-playback-position ()
    (when-let ((item-beg (org-in-item-p)))
      (when (and (not (bound-and-true-p org-timer-start-time))
		 (mpv-live-p)
		 (save-excursion
                   (goto-char item-beg)
                   (and (not (org-invisible-p)) (org-at-item-timer-p))))
	(mpv-insert-playback-position t))))
  (add-hook 'org-metareturn-hook #'org-metareturn-insert-playback-position)

  (add-hook 'org-open-at-point-functions #'mpv-seek-to-position-at-point))

;; Calculator
(use-package literate-calc-mode
  :mode ("calc" . literate-calc-mode))

;; Reader
(use-package nov
  :mode ("\\.epub\\'" . nov-mode)
  :config
  (setq nov-text-width 80))
(defun my-nov-font-setup ()
  (face-remap-add-relative 'variable-pitch
			   :family "Alegreya"
			   :height 1.5))
(add-hook 'nov-mode-hook 'my-nov-font-setup)

;; Nov notes, combined with org-noter in place of pdf-tools to take notes
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
  (fanyi-providers '(;; 海词
                     fanyi-haici-provider
                     ;; 有道同义词词典，支持中文
                     fanyi-youdao-thesaurus-provider
                     ;; Etymonline
                     ;fanyi-etymon-provider
                     ;; Longman
                     fanyi-longman-provider))
  (fanyi-verbose nil))
(use-package sdcv)

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
	  ;("https://emacs.stackexchange.com/feeds" emacs)
	  ;("https://www.reddit.com/r/emacs/.rss" emacs)
	  ;("https://www.reddit.com/r/orgmode/.rss" org emacs)
	  ("https://news.ycombinator.com/rss" tech news)
	  ("https://www.economist.com/international/rss.xml" eco news)
	  ("https://www.reddit.com/r/leagueoflegends/.rss" lol games)
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
			  (query . (and news emacs))))
		  ;; (group (:title . "Org")
		  ;; 	 (:elements
		  ;; 	  (query . (and org emacs))))
		  ))
	  (group (:title . "News")
		 (:elements
		  (query . (and news (not '(tech eco emacs))))
		  (group (:title . "Tech")
			 (:elements
			  (query . (and tech news))))
		  (group (:title . "Eco")
			 (:elements
			  (query . (and eco news))))))
	  (group (:title . "Games")
		 (:elements
		  (query . (and Games (not '(lol))))
		  (group (:title . "LOL")
			 (:elements
			  (query . (and lol games))))))
	  )))

;; Pomm
(use-package pomm
  :commands (pomm)
  :config
  (bind-key* "C-c C-p" #'pomm)
  (setq pomm-audio-enabled nil)
  (setq pomm-work-period 45)
  (setq pomm-long-break-period 20)
  (setq pomm-number-of-periods 2))
(require 'pomm)
(pomm-mode-line-mode)

;;; Chatgpt
(when *is-mac*
  (defun osx-get-keychain-password (account-name)
	"Gets ACCOUNT-NAME keychain password from OS X Keychain."
	(let ((cmd (concat "security 2>&1 >/dev/null find-generic-password -ga '" account-name "'")))
	  (let ((passwd (shell-command-to-string cmd)))
		(when (string-match (rx "\"" (group (0+ (or (1+ (not (any "\"" "\\"))) (seq "\\" anything)))) "\"") passwd)
		  (match-string 1 passwd))))))
(when *is-mac*
  (use-package chatgpt-shell
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
