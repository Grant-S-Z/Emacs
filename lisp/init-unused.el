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

;;; Calibre
(use-package calibredb
  :config
  (setq calibredb-root-dir "~/Calibre Library")
  (setq calibredb-db-dir (expand-file-name "metadata.db" calibredb-root-dir))
  (setq calibredb-library-alist '(("~/Calibre"))))

;;; 默认 ssh
;(setq tramp-default-method "ssh")

;; (setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
;; 			 ("org" . "https://orgmode.org/elpa/")
;; 			 ("melpa" . "https://melpa.org/packages/")
;; 			 ("melpa-stable" . "https://stable.melpa.org/packages/"))
;;       package-archive-priorities '(("melpa-stable" . 1)))

(require 'use-package) ;; use-package 现已内置

;;; Set transparency
(set-frame-parameter (selected-frame) 'alpha '(100 100))
(add-to-list 'default-frame-alist '(alpha 100 100))

(add-to-list 'org-capture-templates ;; 密码模板
             '("k" "Passwords" entry (file "~/passwords.org")
             "* %U - %^{title} %^G\n\n  - 用户名: %^{用户名}\n  - 密码: %(get-or-create-password)"
               :empty-lines 1 :kill-buffer t))

(use-package geiser-mit
  :config
  (setq geiser-active-implementations '(mit))
  (setq geiser-default-implementation 'mit)
  (add-hook 'scheme-mode-hook 'geiser-mode)
  (add-to-list 'auto-mode-alist '("\\.scm\\'" . geiser-mode)))

(use-package djvu)

;;; all-the-icons
(use-package all-the-icons
  :if (display-graphic-p))

(add-to-list 'load-path "~/.emacs.d/site-lisp/welcome-dashboard")
(require 'welcome-dashboard)
(setq welcome-dashboard-latitude calendar-latitude
      welcome-dashboard-longitude calendar-longitude ;; latitude and longitude must be set to show weather information
      welcome-dashboard-use-nerd-icons t ;; Use nerd icons instead of all-the-icons
      welcome-dashboard-path-max-length 75
      welcome-dashboard-use-fahrenheit nil ;; show in celcius or fahrenheit.
      welcome-dashboard-min-left-padding 10
      welcome-dashboard-image-file "~/.emacs.d/img/Robin.png"
      welcome-dashboard-image-width 200
      welcome-dashboard-max-number-of-todos 5
      welcome-dashboard-image-height 169
      welcome-dashboard-title "Welcome Grant. Have a great day!")
(welcome-dashboard-create-welcome-hook)

(use-package amx
  :init (amx-mode))

;; lsp
(add-hook 'find-file-hook #'lsp-bridge-restart-process) ;; 每进入一次其他文件，重启 lsp

;;; LeetCode
(use-package leetcode
  :config
  (setq leetcode-prefer-language "cpp")
  (setq leetcode-save-solutions t)
  (setq leetcode-directory "~/leetcode")
  (add-hook 'leetcode-solution-mode-hook
            (lambda() (flycheck-mode -1))))

;; Fit org modern indent
(use-package org-modern-indent
  :load-path "~/.emacs.d/site-lisp/org-modern-indent"
  :defer t
  :after org
  :hook (org-mode . org-modern-indent-mode)
  :config
  (add-hook 'org-mode-hook #'org-modern-indent-mode 90))

;; highlight-symbol
(use-package highlight-symbol
  :init (highlight-symbol-mode))

(use-package c++-mode
  :functions 			; suppress warnings
  c-toggle-hungry-state
  :hook
  (c-mode . lsp-deferred)
  (c++-mode . lsp-deferred)
  (c++-mode . c-toggle-hungry-state))

;;; lsp-mode
(use-package company
  :init (global-company-mode)
  :config
  (setq company-minimum-prefix-length 1) ; 1 个字母开始进行自动补全
  (setq company-tooltip-align-annotations t)
  (setq company-idle-delay 0.0)
  (setq company-show-numbers t) ;; 给选项编号 (按快捷键 M-1、M-2 等等来进行选择).
  (setq company-selection-wrap-around t)
  (setq company-transformers '(company-sort-by-occurrence))) ; 根据选择的频率进行排序

(use-package company-box
  :after company
  :hook (company-mode . company-box-mode))

(use-package lsp-mode
  :ensure t
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-x C-l"
	lsp-file-watch-threshold 500)
  :hook
  (lsp-mode . lsp-enable-which-key-integration) ; which-key integration
  :commands (lsp lsp-deferred)
  :config
  (setq lsp-completion-provider :none) ;; 阻止 lsp 重新设置 company-backend 而覆盖我们 yasnippet 的设置
  (setq lsp-headerline-breadcrumb-enable t)
  :bind
  ;("C-c l s" . lsp-ivy-workspace-symbol)
  ) ;; 可快速搜索工作区内的符号（类名、函数名、变量名等）

(use-package lsp-ui
  :ensure t
  :config
  (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
  (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references)
  (setq lsp-ui-doc-position 'top))

;;; Dap
(use-package dap-mode
  :after hydra lsp-mode
  :commands dap-debug
  :custom
  (dap-auto-configure-mode t)
  :config
  (dap-ui-mode 1)
  :hydra
  (hydra-dap-mode
   (:color pink :hint nil :foreign-keys run)
   "
^Stepping^          ^Switch^                 ^Breakpoints^         ^Debug^                     ^Eval
^^^^^^^^----------------------------------------------------------------------------------------------------------------
_n_: Next           _ss_: Session            _bb_: Toggle          _dd_: Debug                 _ee_: Eval
_i_: Step in        _st_: Thread             _bd_: Delete          _dr_: Debug recent          _er_: Eval region
_o_: Step out       _sf_: Stack frame        _ba_: Add             _dl_: Debug last            _es_: Eval thing at point
_c_: Continue       _su_: Up stack frame     _bc_: Set condition   _de_: Edit debug template   _ea_: Add expression.
_r_: Restart frame  _sd_: Down stack frame   _bh_: Set hit count   _ds_: Debug restart
_Q_: Disconnect     _sl_: List locals        _bl_: Set log message
                  _sb_: List breakpoints
                  _sS_: List sessions
"
   ("n" dap-next)
   ("i" dap-step-in)
   ("o" dap-step-out)
   ("c" dap-continue)
   ("r" dap-restart-frame)
   ("ss" dap-switch-session)
   ("st" dap-switch-thread)
   ("sf" dap-switch-stack-frame)
   ("su" dap-up-stack-frame)
   ("sd" dap-down-stack-frame)
   ("sl" dap-ui-locals)
   ("sb" dap-ui-breakpoints)
   ("sS" dap-ui-sessions)
   ("bb" dap-breakpoint-toggle)
   ("ba" dap-breakpoint-add)
   ("bd" dap-breakpoint-delete)
   ("bc" dap-breakpoint-condition)
   ("bh" dap-breakpoint-hit-condition)
   ("bl" dap-breakpoint-log-message)
   ("dd" dap-debug)
   ("dr" dap-debug-recent)
   ("ds" dap-debug-restart)
   ("dl" dap-debug-last)
   ("de" dap-debug-edit-template)
   ("ee" dap-eval)
   ("ea" dap-ui-expressions-add)
   ("er" dap-eval-region)
   ("es" dap-eval-thing-at-point)
   ("q" nil "quit" :color blue)
   ("Q" dap-disconnect :color red)))
(use-package dap-lldb
  :after dap-mode
  :custom
  (dap-lldb-debug-program '("/usr/bin/lldb"))
  ;; ask user for executable to debug if not specified explicitly (c++)
  (dap-lldb-debugged-program-function
   (lambda () (read-file-name "Select file to debug: "))))

;; hydra
(require 'hydra)
(require 'use-package-hydra)

;; undo-tree
(use-package undo-tree
  :ensure t
  :init (global-undo-tree-mode)
  :after hydra
  :config
  (bind-key* "C-x u" #'hydra-undo-tree/body)
  :hydra (hydra-undo-tree (:hint nil)
  "
  _p_: undo  _n_: redo _s_: save _l_: load   "
  ("p"   undo-tree-undo)
  ("n"   undo-tree-redo)
  ("s"   undo-tree-save-history)
  ("l"   undo-tree-load-history)
  ("u"   undo-tree-visualize "visualize" :color blue)
  ("q"   nil "quit" :color blue)))

;; org-modern-indent
(add-to-list 'load-path "~/.emacs.d/lisp")
(require 'org-modern-indent)
(add-hook 'org-mode-hook #'org-modern-indent-mode 90)

(use-package projectile
  :bind (("C-c p" . projectile-command-map))
  :config
  (setq projectile-mode-line "Projectile")
  (setq projectile-track-known-projects-automatically nil))
;;; init-unused.el ends here
