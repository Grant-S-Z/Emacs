;;; init-unused.el --- Settings unused
;;; Commentary:
;;; Code:
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

(use-package ef-themes
  :init (load-theme 'ef-frost t))


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

;;; Videos
(use-package emms
  :bind (("C-c m m" . emms)
	 ("C-c m ," . emms-pause)
	 ("C-c m ." . emms-stop)
	 ("C-c m n" . emms-next)
	 ("C-c m r" . emms-previous))
  :config
  (require 'emms-setup)
  (emms-all)
  (setq emms-player-list '(emms-player-mpv))
  (setq emms-playlist-buffer-name "*Music*")
  (setq emms-source-file-default-directory "~/Music/MusicFree"))

(use-package mpvi)

(add-to-list 'load-path "~/.emacs.d/site-lisp/bilibili.el")
(require 'bilibili)
(setq bilibili-cookie-text "
buvid3=750BFB06-7E42-7B48-5A1A-BDF7ED2EAE6146387infoc; buvid4=BF81C9F4-8987-AFFB-1C54-1BC5BAF9ECB047529-023031311-JaQAkcnf0ookmGnscWdytw%3D%3D; DedeUserID=317050365; DedeUserID__ckMd5=2cdbd574d6e7b861; rpdid=|(J|)ul~kYk~0J'uY~)Y~)|)R; buvid_fp_plain=undefined; i-wanna-go-back=-1; b_ut=5; LIVE_BUVID=AUTO3016832895494307; hit-new-style-dyn=1; header_theme_version=CLOSE; enable_web_push=DISABLE; CURRENT_BLACKGAP=0; CURRENT_FNVAL=4048; b_nut=100; _uuid=101010F1027E-8E43-71F9-10355-E6D12DF1B9A675516infoc; hit-dyn-v2=1; SESSDATA=fa52b9d1%2C1726573794%2C0f39f%2A32CjBMiT1hDLzSyaCbcRmKOgruqLBHTTVHLg9DAI0SoJozpKddLLJEyVwWq7WKUNIDFf0SVnZrMEE4ZnBhS0tPZHpBMnRBbFlOSl9xRF9rck5kUUlXc1RDSzNSckVFdU5vZXlHSXBGX3FHTnhjXzVIQjhIY1AwbmhqY2JFYmN0VW5RNjlFMjhmMFNRIIEC; bili_jct=4ef9a3296047201c763d45727a4371af; fingerprint=b82c30b3ed1bbdbf5702b88ef4707a8f; buvid_fp=b82c30b3ed1bbdbf5702b88ef4707a8f; FEED_LIVE_VERSION=V_WATCHLATER_PIP_WINDOW3; CURRENT_QUALITY=116; bp_video_offset_317050365=918446376503738373; bili_ticket=eyJhbGciOiJIUzI1NiIsImtpZCI6InMwMyIsInR5cCI6IkpXVCJ9.eyJleHAiOjE3MTI5NDM1MjcsImlhdCI6MTcxMjY4NDI2NywicGx0IjotMX0.vmWeZAqeBoz_ocBwUFw6nEtwps4EbXAZCdvajHkiJeo; bili_ticket_expires=1712943467; PVID=2; home_feed_column=4; b_lsid=110951052A_18EC96C6F4E; browser_resolution=165-779")

(setq mouse-wheel-mode nil) ;; 滚轮
(setq mac-mouse-wheel-mode nil)
(setq pixel-scroll-mode nil) ;; 像素滚动，但在我这里不好用
(setq pixel-scroll-precision-mode nil)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time

(set-cursor-color "#8A2BE2") ;; 设置光标颜色 BlueViolet

;; 全屏启动，且可使用状态栏与程序坞
(set-frame-parameter nil 'fullscreen 'fullboth)

;; 忽略某些 message
(let ((inhibit-message t))
  (message "Wrong type argument: number-or-marker-p, nil"))

;;; 图片
;; 复制
(use-package org-download
  :config
  (add-hook 'dired-mode-hook 'org-download-enable)
  (setq-default org-download-image-dir "./img"))

;; graphviz
(use-package graphviz-dot-mode)

;;; 行号
;(setq display-line-numbers-type 'relative)
(setq display-line-numbers nil)
(setq display-line-numbers-width-start t) ;; 固定行号宽度
(global-display-line-numbers-mode nil)

;; 字体对齐
(use-package cnfonts
  :init (cnfonts-mode 1))

;; Useless translate
(use-package go-translate
  :config
  (setq gts-translate-list '(("en" "zh")))
  (setq gts-default-translator
	(gts-translator
	 :picker (gts-prompt-picker)
	 :engines (list (gts-bing-engine))
	 :render
	 (gts-buffer-render))))

;; 经常误操作，比如删除当前界面的内容
(use-package org-popup-posframe
  :load-path "~/.emacs.d/site-lisp/org-popup-posframe"
  :defer nil
  :init (org-popup-posframe-mode 1))

;;; Jupyter
(use-package ein)

;; Conda
(use-package conda
  :config
  (conda-mode-line-setup)
  (setq conda-anaconda-home (expand-file-name "~/miniconda3")
	conda-env-home-directory (expand-file-name "~/miniconda3/envs/")))

;;; 居中
(use-package writeroom-mode)

(use-package sly)

;;; init-unused.el ends here
