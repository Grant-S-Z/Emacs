;;; init-package.el --- for packages
;;; Commentary:
;;; Code:
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
	 ("C-S-k" . crux-smart-kill-line)))

(use-package yasnippet ;; 设置 snippets
  :init (yas-global-mode t)
  :hook (prog-mode . yas-minor-mode))

(use-package yasnippet-snippets ;; 提供常用 snippets
  :after yasnippet)

(use-package which-key ;; 快捷键提示
  :defer nil
  :config (which-key-mode))

(use-package rainbow-delimiters ;; 括号颜色
  :init (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

(use-package highlight-parentheses ;; 括号高亮
  :init (add-hook 'prog-mode-hook 'highlight-parentheses-mode))

;;; Git
(use-package magit)

;;; 界面
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

(use-package pangu-spacing ;; Comfortable space
  :init
  (global-pangu-spacing-mode 1)
  :config
  (setq pangu-spacing-real-insert-separtor t))

;; Key cast
(use-package keycast
  :config
  (push '(org-self-insert-command nil nil) keycast-substitute-alist)
  (push '(self-insert-command nil nil) keycast-substitute-alist)
  (push '(mouse-drag-region nil nil) keycast-substitute-alist)
  (push '(mouse-set-point nil nil) keycast-substitute-alist)
  (push '(lsp-ui-doc--handle-mouse-movement nil nil) keycast-substitute-alist)
  (push '(mac-mwheel-scroll nil nil) keycast-substitute-alist))

;;; 终端
(use-package vterm
  :init
  (setq vterm-shell "zsh"))

(use-package vterm-toggle
  :bind ("C-c s" . vterm-toggle)
  :config
  (setq vterm-toggle-fullscreen-p nil)
  (add-to-list 'display-buffer-alist
               '((lambda (buffer-or-name _)
                   (let ((buffer (get-buffer buffer-or-name)))
                     (with-current-buffer buffer
                       (or (equal major-mode 'vterm-mode)
                           (string-prefix-p vterm-buffer-name (buffer-name buffer))))))
                 (display-buffer-reuse-window display-buffer-at-bottom)
                 (reusable-frames . visible)
                 (window-height . 0.3))))

;;; hugo
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

;;; Bongo, a music player
(use-package bongo
  :commands bongo-playlist
  :bind ("C-c m" . bongo-playlist)
  :custom
  (bongo-enabled-backends '(mpv))
  (bongo-default-directory "~/Music/MusicFree/")
  (bongo-logo nil)
  (bongo-insert-album-covers nil)
  (bongo-album-cover-size 100)
  (bongo-mode-line-indicator-mode nil))

;;; Calculator
(use-package literate-calc-mode)

;;; MPV
(use-package mpv)

;;; Calibre
(use-package calibredb
  :config
  (setq calibredb-root-dir "~/Calibre Library")
  (setq calibredb-db-dir (expand-file-name "metadata.db" calibredb-root-dir))
  (setq calibredb-library-alist '(("~/Calibre"))))

;;; EPUB reader
(use-package nov
  :config
  (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))
  (setq nov-text-width 100))
(defun my-nov-font-setup ()
  (face-remap-add-relative 'variable-pitch
			   :family "Alegreya"
			   :height 1.1))
(add-hook 'nov-mode-hook 'my-nov-font-setup)

;;; Translator
(use-package fanyi
  :bind ("C-c f" . fanyi-dwim)
  :custom
  (fanyi-providers '(;; 海词
                     fanyi-haici-provider
                     ;; 有道同义词词典
                     fanyi-youdao-thesaurus-provider
                     ;; Etymonline
                     fanyi-etymon-provider
                     ;; Longman
                     fanyi-longman-provider))
  (fanyi-verbose nil))

(use-package go-translate
  :config
  (setq gts-translate-list '(("en" "zh")))
  (setq gts-default-translator
	(gts-translator
	 :picker (gts-prompt-picker)
	 :engines (list (gts-bing-engine))
	 :render
	 (gts-buffer-render))))

;;; RSS
(use-package elfeed
  :config
  (setq elfeed-feeds
	'(("https://arxiv.org/rss/hep-ex" study physics)
	  ("https://arxiv.org/rss/hep-ph" study physics)
	  ("http://www.reddit.com/r/emacs/.rss" discussion emacs)
	  ("https://planet.emacslife.com/atom.xml" discussion emacs)
	  ("https://rss.nytimes.com/services/xml/rss/nyt/World.xml" news))))

(provide 'init-package)
;;; init-package.el ends here
