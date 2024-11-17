;;; init-unused.el --- Settings unused
;;; Commentary:
;;; Code:
(use-package cape
  :ensure t
  :init
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-keyword)
  (add-to-list 'completion-at-point-functions #'cape-history))

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

;; (use-package lsp-mode
;;   :init
;;   (defun grant/lsp-mode-setup-completion ()
;;      (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
;; 	   '(orderless)))
;;   :commands (lsp lsp-deferred)
;;   :hook
;;   ((c++-mode . lsp-deferred)
;;    (c-mode . lsp-deferred)
;;    (rust-mode . lsp-deferred)
;;    (lsp-mode . lsp-enable-which-key-integration)
;;    (lsp-completion-mode . grant/lsp-mode-setup-completion))
;;   :custom
;;   (lsp-keymap-prefix "C-x C-l")
;;   (lsp-file-watch-threshold 500)
;;   ;; Completion provider
;;   (lsp-completion-provider :none)
;;   ;; Python ruff
;;   (lsp-ruff-python-path "~/miniconda3/bin/python3")
;;   (lsp-ruff-server-command '("~/.local/bin/ruff" "server"))
;;   )

;; (use-package lsp-pyright
;;   :custom (lsp-pyright-langserver-command "~/.local/bin/pyright")
;;   :hook (python-mode . (lambda ()
;;                           (require 'lsp-pyright)
;;                           (lsp-deferred))))

;; (use-package lsp-ui
;;   :custom
;;   (lsp-ui-sideline-enable nil)
;;   (lsp-ui-peek-enable t)
;;   (lsp-ui-doc-enable t))

;; (use-package lsp-treemacs)

;; (use-package dap-mode
;;   :config
;;   (dap-mode 1)
;;   (dap-ui-mode 1)
;;   (dap-tooltip-mode 1)
;;   (tooltip-mode 1)
;;   (dap-ui-controls-mode 1)
;;   ;; Python
;;   (require 'dap-python)
;;   (setq dap-python-debugger 'debugpy)
;;   ;; C
;;   (require 'dap-gdb-lldb)
;;   ;(setq dap-lldb-debug-program "/usr/bin/lldb")
;;   )

;; hydra
(require 'hydra)
(require 'use-package-hydra)

;; undo-tree
(use-package undo-tree ;; 重量级使用体验，强烈不推荐
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

;; for sdcv and fanyi
;; (defun kimim/sdcv-translate-result-advice (word dictionary-list)
;;   (let* ((arguments
;;           (cons word
;;                 (mapcan
;;                  (lambda (d) (list "-u" d)) dictionary-list)))
;;        (result (mapconcat
;;                 (lambda (result)
;;                   (let-alist result
;;                     (format
;;                      "## %s\n%s\n\n" .dict .definition)))
;;                 (apply #'sdcv-call-process arguments)
;;                 "")))
;;   (if (string-empty-p result)
;;       sdcv-fail-notify-string
;;     result)))

;; (advice-add 'sdcv-translate-result
;;             :override
;;             #'kimim/sdcv-translate-result-advice)

;; (defun kimim/fanyi-dwim-add-sdcv (word)
;;   (let ((buf (get-buffer fanyi-buffer-name)))
;;   (with-current-buffer buf
;;     (let ((inhibit-read-only t)
;;           (inhibit-point-motion-hooks t))
;;       ;; Clear the previous search result.
;;       (point-max)
;;       (insert "# SDCV\n\n")
;;       (insert
;;        (sdcv-search-with-dictionary-args
;;         word sdcv-dictionary-complete-list))
;;       (insert "\n\n")
;;       (beginning-of-buffer)))))

;; (advice-add 'fanyi-dwim :after
;;             #'kimim/fanyi-dwim-add-sdcv)

;; Translate variable name when coding
(add-to-list 'load-path "~/.emacs.d/site-lisp/insert-translated-name/")
(require 'insert-translated-name)
(setq insert-translated-name-program "ollama")
(setq insert-translated-name-ollama-model-name "zephyr")

;;; Ollama client
(use-package ellama
  :init
  (setopt ellama-language "English")
  (require 'llm-ollama)
  (setopt ellama-provider
		  (make-llm-ollama
		   :chat-model "zephyr" :embedding-model "zephyr"))
  :bind (("C-c t" . ellama-translate)))

(defun pdf-open (pdf-path)
  "Open a PDF file with Skim's displayline on macOS.
Argument PDF-PATH The path to the PDF file."
  (interactive "fPath to PDF: ") ;; Prompt user for PDF file path
  (let ((skim-path "/opt/homebrew/bin/displayline"))
    (if (file-exists-p pdf-path) ;; Check the path existence
        (start-process "pdf-open" nil skim-path "1" pdf-path)
      (message "PDF file does not exist: %s" pdf-path))))

(require 'theme-changer)
(change-theme 'doom-one-light 'doom-shades-of-purple)

(defun grant/disable-keycast-header-line-mode-in-writeroom ()
  "Disable keycast-header-line-mode when writeroom-mode is enabled."
  (when (bound-and-true-p keycast-header-line-mode)
    (keycast-header-line-mode -1)))

(add-hook 'writeroom-mode-hook #'grant/disable-keycast-header-line-mode-in-writeroom)

(add-to-list 'load-path "~/.emacs.d/site-lisp/holo-layer/")
(require 'holo-layer)
(setq holo-layer-python-command "~/miniconda3/envs/hep/bin/python")
(setq holo-layer-enable-cursor-animation t)
(setq holo-layer-enable-type-animation t)
(setq holo-layer-enable-indent-rainbow t)
(holo-layer-enable)

;;; eglot
;; (require 'eglot)

;; (add-to-list 'eglot-server-programs
;; 	     '((c-mode c++-mode) . ("clangd")))
;; (add-to-list 'eglot-server-programs
;;              '((python-mode python-ts-mode) . ("/Users/grant/miniconda3/envs/hep/bin/pyright")))

;; (add-hook 'c-mode-hook 'eglot-ensure)
;; (add-hook 'c++-mode-hook 'eglot-ensure)
;; (add-hook 'python-mode-hook 'eglot-ensure)

;;; company
;; (use-package company
;;   :init (global-company-mode)
;;   :config
;;   (setq company-minimum-prefix-length 1) ; 1 个字母开始进行自动补全
;;   (setq company-tooltip-align-annotations t)
;;   (setq company-idle-delay 0.0)
;;   (setq company-show-numbers t) ;; 给选项编号 (按快捷键 M-1、M-2 等等来进行选择).
;;   (setq company-selection-wrap-around t)
;;   (setq company-transformers '(company-sort-by-occurrence))) ; 根据选择的频率进行排序

;; (use-package company-box
;;   :after company
;;   :hook (company-mode . company-box-mode))

(use-package emacs ;; 史前配置
  :init
  ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Emacs 28: Hide commands in M-x which do not work in the current mode.
  ;; Vertico commands are hidden in normal buffers.
  (setq read-extended-command-predicate
        #'command-completion-default-include-p)

  ;; Enable recursive minibuffers
  (setq enable-recursive-minibuffers t)

  (defvar eh-space "  ")

  ;; 设置 mode-line
  ;; 在 mode-line 最后追加一个半角空格，一个全角空格，防止因为字体高度原因导致 mode-line 抖动。
  (setq mode-line-end-spaces
	'(:eval (if (display-graphic-p) eh-space "-%-")))

  (defun eh-tab-line-format (orig_func)
    "在 tab-line 的最后添加一个全角空格，防止 tab-line 抖动。"
    (list (funcall orig_func) eh-space))

  (advice-add 'tab-line-format :around #'eh-tab-line-format)

  ;; (progn
  ;;   (set-face-attribute 'default nil    ;:font "Fantasque Sans Mono"
  ;; 					;:font "DejaVu Sans Mono"
  ;; 			                ;:font "IBM Plex Mono"
  ;; 					:font "Ligconsolata"
  ;; 					:height 160)
  ;;   (dolist (charset '(kana han symbol cjk-misc bopomofo)) ;; Chinese fonts
  ;;     (set-fontset-font (frame-parameter nil 'font)
  ;; 			charset (font-spec :family "LXGW WenKai Mono"))))
  )

;; (use-package whitespace # 太繁复了
;;   :hook (after-init . global-whitespace-mode))

;; (use-package olivetti
;;   :after org
;;   :hook ((org-mode . olivetti-mode)
;; 	 (text-mode . olivetti-mode)
;; 	 (markdown-mode . olivetti-mode))
;;   :custom
;;   (olivetti-body-width 100))
;; (defun grant/toggle-olivetti-according-window ()
;;     "Toggle olivetti mode according to window width."
;;     (if (and (eq (buffer-local-value 'major-mode (current-buffer)) 'org-mode)
;; 	     (or (eq (length (window-list nil nil nil)) 1)
;; 		 (window-at-side-p (frame-first-window) 'right))) ;; frame-first-window 的 mode 是 org-mode 并且没有右边 window
;; 	(olivetti-mode 1)
;;       (olivetti-mode 0)
;;       (when (eq (buffer-local-value 'major-mode (current-buffer)) 'org-mode)
;; 	(visual-line-mode 1))))
;;   (add-hook 'org-mode-hook #'grant/toggle-olivetti-according-window)
;;   (add-hook 'window-configuration-change-hook #'grant/toggle-olivetti-according-window)

;; unsuccessful
;; (setq org-format-latex-options '(:foreground "Black"
;; 					     :background "Transparent"
;; 					     :scale 1.1))
;;
;; (defun grant/update-org-latex-preview-scale ()
;;   (let ((org-latex-preview-scale
;; 	   (cond
;; 	    ((= (display-pixel-height) 900) 1.1)
;; 	    ((= (display-pixel-height) 1080) 1.7)
;; 	    (t, 1.1)))
;;     (plist-put org-format-latex-options :scale org-latex-preview-scale))))
;; (advice-add 'org--make-preview-overlay
;; 	    :after #'grant/update-org-latex-preview-scale)

;; Vertically align LaTeX preview in org mode
;; (defun grant/org-latex-preview-advice (beg end &rest _args)
;;   (let* ((ov (car (overlays-at (/ (+ beg end) 2) t)))
;;          (img (cdr (overlay-get ov 'display)))
;;          (new-img (plist-put img :ascent 95)))
;;     (overlay-put ov 'display (cons 'image new-img))))
;; (advice-add 'org--make-preview-overlay
;;             :after #'grant/org-latex-preview-advice)

;; 全屏启动，且可使用状态栏与程序坞
;(set-frame-parameter nil 'fullscreen 'fullboth)

;; Inhibit resizing Puremacs frame
(setq frame-inhibit-implied-resize t)

;; To suppress flashing at startup
(setq-default inhibit-redisplay t
              inhibit-message t)
(defun reset-inhibit-vars ()
  "Reset inhibit vars."
  (setq-default inhibit-redisplay nil
                inhibit-message nil)
  (redraw-frame))
(add-hook 'window-setup-hook #'reset-inhibit-vars)
(define-advice startup--load-user-init-file (:after (&rest _) reset-inhibit-vars)
  (and init-file-had-error (reset-inhibit-vars)))

;;; 位置
(setq calendar-location-name "Beijing, CN")
(setq calendar-latitude 39.9042)
(setq calendar-longitude 116.4074)

;;; Functions
(defun grant/make-in-parent-directory ()
  "Run `make` in the parent directory of the current buffer's file."
  (interactive)
  (let ((default-directory (file-name-directory (directory-file-name (file-name-directory (or (buffer-file-name) ""))))))
    (compile "make")))

(defun grant/indent-all ()
  "Indent for all code."
  (interactive)
  (indent-region (point-min) (point-max))
  (message "format successfully"))

(defun grant/clear-messages-buffer ()
  "Clear Message buffer."
  (interactive)
  (let ((inhibit-read-only t))
    (with-current-buffer "*Messages*"
      (erase-buffer))))

(defun grant/kill-unused-buffers () ;; use crux-kill-other-buffers
  "Kill unused buffers."
  (interactive)
  (ignore-errors
    (save-excursion
      (dolist (buf (buffer-list))
	(set-buffer buf)
	(when (and (string-prefix-p "*" (buffer-name)) (string-suffix-p "*" (buffer-name)))
	  (kill-buffer buf))))))

;;; 待办事项关键词
;; (setq org-todo-keywords '((sequence "TODO" "DONE" "CANCELED")))
;; (setq org-todo-keyword-faces '(("TODO" . "red") ("DONE" . "green") ("CANCELED" . "blue")))

;;; 表格
(use-package valign
  :after org
  :hook (org-mode . valign-mode)
  :custom
  (valign-fancy-bar nil)) ;; 确保性能

(setq package-native-compile t)

;; (add-to-list 'exec-path "~/miniconda3/bin")
;; (setenv "PATH" "~/miniconda3/bin:$PATH" '("PATH"))

;; (setq python-shell-exec-path "python")

;; (use-package highlight-parentheses ;; highlight parentheses
;;   :init (add-hook 'prog-mode-hook 'highlight-parentheses-mode))

;; (use-package color-identifiers-mode
;;   :init (add-hook 'prog-mode-hook 'color-identifiers-mode))

;; multiple-cursors
(use-package multiple-cursors
  :bind
  ("C-S-<mouse-1>" . mc/toggle-cursor-on-click))

;; Lsp bridge
;; (use-package lsp-bridge
;;   :defer nil
;;   :load-path "~/.emacs.d/site-lisp/lsp-bridge"
;;   :bind (("C-x C-l" . lsp-bridge-mode)
;; 	 ("C-x C-p" . lsp-bridge-peek) ;; 8 -> lsp-bridge-peek-jump
;; 	 ("C-x C-8" . lsp-bridge-peek-jump-back)
;; 	 ("C-c <return>" . lsp-bridge-code-format))
;;   :config
;;   (setq lsp-bridge-peek-file-content-height 14)
;;   (setq lsp-bridge-peek-file-content-scroll-margin 2)
;;   (setq-default
;;    acm-enable-icon t
;;    acm-enable-doc t
;;    acm-enable-yas nil
;;    acm-enable-tempel nil
;;    acm-enable-quick-access t
;;    acm-enable-search-file-words nil
;;    acm-enable-telega nil
;;    acm-enable-tabnine nil
;;    acm-enable-doc-markdown-render 'async
;;    acm-candidate-match-function 'orderless-flex
;;    lsp-bridge-enable-log nil
;;    lsp-bridge-enable-signature-help nil
;;    lsp-bridge-enable-diagnostics nil
;;    lsp-bridge-complete-manually nil
;;    lsp-bridge-enable-search-words nil
;;    lsp-bridge-enable-auto-format-code nil)
;;   ;; languages
;;   (setq lsp-bridge-default-mode-hooks '(c-mode-hook c++-mode-hook python-mode-hook bash-mode-hook sh-mode-hook emacs-lisp-mode-hook lisp-interaction-mode-hook LaTeX-mode-hook bibtex-mode-hook))
;;   (setq lsp-bridge-c-lsp-server "clangd")
;;   (setq lsp-bridge-python-command "~/miniconda3/envs/hep/bin/python")
;;   (setq lsp-bridge-python-lsp-server "~/miniconda3/envs/hep/bin/pyright")
;;   (setq lsp-bridge-python-multi-lsp-server "pyright_ruff")
;;   (setq lsp-bridge-enable-org-babel t)
;;   ;; start
;;   (add-hook 'find-file-hook #'lsp-bridge-restart-process) ;; restart lsp-bridge when entering a new file
;;   (global-lsp-bridge-mode))

;; Debug
;; (use-package realgud)

(setq blink-cursor-mode t) ;; no blinking cursor, which is very distracting

;; (use-package ultra-scroll-mac
;;   :if (eq window-system 'mac)
;;   :load-path "~/.emacs.d/site-lisp/ultra-scroll-mac"
;;   :init
;;   (setq scroll-conservatively 101 ; important!
;;         scroll-margin 0)
;;   :config
;;   (ultra-scroll-mac-mode 1))

;; Eglot
;; (use-package eglot
;;   :bind (("C-x C-l" . eglot)
;; 	 ("C-c <RET>" . eglot-format)
;; 	 ("C-x C-p" . eglot-find-declaration))
;;   :hook ((c-mode-hook . eglot-ensure)
;; 	 (c++-mode-hook .eglot-ensure)
;; 	 (python-mode . eglot-ensure))
;;   :config
;;   (add-to-list 'eglot-server-programs
;; 	     '((c-mode c++-mode) . ("clangd")))
;;   (add-to-list 'eglot-server-programs
;; 		 '((python-mode) . ("~/.local/bin/pyright-langserver" "--stdio")))) ;; use langserver

;; Grammer
;; (setq flymake-start-on-save-buffer nil)
;; (setq flymake-start-on-flymake-mode nil)
;; (use-package flycheck
;;   :config
;;   (setq truncate-lines nil) ; 如果单行信息很长会自动换行
;;   (setq flycheck-python-pycompile-executable "~/miniconda3/bin/python3")
;;   (setq flycheck-python-ruff-executable "~/.local/bin/ruff")
;;   :hook
;;   (prog-mode . flycheck-mode))

;; Tree-sitter
;; (setq treesit-extra-load-path '("~/.emacs.d/tree-sitter/"))

(use-package listen
  :custom
  (listen-library "~/Music/MusicFree/"))

;; Flymake
(require 'flymake)

(setq flymake-cc-command `("clang" "-fsyntax-only" "-Weverything" "-x" "-std=c++11"))

;; LSP bridge
(add-to-list 'load-path "~/.emacs.d/site-lisp/lsp-bridge/")
(require 'lsp-bridge)
(use-package lsp-bridge
  :load-path "~/.emacs.d/site-lisp/lsp-bridge"
  :bind (("C-x C-l" . lsp-bridge-mode)
	 ("C-x C-p" . lsp-bridge-peek) ;; 8 -> lsp-bridge-peek-jump
	 ("C-x C-8" . lsp-bridge-peek-jump-back)
	 ("C-c <return>" . lsp-bridge-code-format))
  :config
  (setq lsp-bridge-peek-file-content-height 14)
  (setq lsp-bridge-peek-file-content-scroll-margin 2)
  (setq-default
   acm-enable-icon t
   acm-enable-doc t
   acm-enable-yas nil
   acm-enable-tempel nil
   acm-enable-quick-access t
   acm-enable-search-file-words nil
   acm-enable-telega nil
   acm-enable-tabnine nil
   acm-enable-doc-markdown-render 'async
   acm-candidate-match-function 'orderless-flex
   lsp-bridge-enable-log nil
   lsp-bridge-enable-signature-help nil
   lsp-bridge-enable-diagnostics nil
   lsp-bridge-complete-manually nil
   lsp-bridge-enable-search-words nil
   lsp-bridge-enable-auto-format-code nil)
  ;; languages
  (setq lsp-bridge-default-mode-hooks '(c-mode-hook c++-mode-hook python-mode-hook bash-mode-hook sh-mode-hook emacs-lisp-mode-hook lisp-interaction-mode-hook LaTeX-mode-hook bibtex-mode-hook))
  (setq lsp-bridge-c-lsp-server "clangd")
  (setq lsp-bridge-python-command "~/miniconda3/bin/python")
  (setq lsp-bridge-python-lsp-server "~/.local/bin/pyright")
  (setq lsp-bridge-python-multi-lsp-server "pyright_ruff")
  (setq lsp-bridge-enable-org-babel t)
  ;; start
  (add-hook 'find-file-hook #'lsp-bridge-restart-process) ;; restart lsp-bridge when entering a new file
  )
(global-lsp-bridge-mode)


;;; Meaningful, but to load less
(use-package symbol-overlay ;; highlight the variable at the point, lsp-mode and even eglot own this
  :hook (prog-mode . symbol-overlay-mode))

;; (use-package keycast ;; key cast
;;   ;; :init (keycast-header-line-mode 1)
;;   :config
;;   (push '(org-self-insert-command nil nil) keycast-substitute-alist)
;;   (push '(self-insert-command nil nil) keycast-substitute-alist)
;;   (push '(mouse-drag-region nil nil) keycast-substitute-alist)
;;   (push '(mouse-set-point nil nil) keycast-substitute-alist)
;;   (push '(lsp-ui-doc--handle-mouse-movement nil nil) keycast-substitute-alist)
;;   (push '(mac-mwheel-scroll nil nil) keycast-substitute-alist))

;; Lua
(use-package lua-mode)

;; Json
(use-package json-mode)

;; Csv
(use-package csv-mode)

;; Yaml
(use-package yaml-mode)

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

  (add-hook 'org-open-at-point-functions #'mpv-seek-to-position-at-point));; Mpv

;; (defun my-denote--split-luhman-sig (signature)
;;   "Split numbers and letters in Luhmann-style SIGNATURE string."
;;   (replace-regexp-in-string
;;    "\\([a-zA-Z]+?\\)\\([0-9]\\)" "\\1=\\2"
;;    (replace-regexp-in-string
;;     "\\([0-9]+?\\)\\([a-zA-Z]\\)" "\\1=\\2"
;;     signature)))

;; (defun my-denote--pad-sig (signature)
;;   "Create a new signature with padded spaces for all components"
;;   (combine-and-quote-strings
;;    (mapcar
;;     (lambda (x)
;;       (string-pad x 5 32 t))
;;     (split-string (my-denote--split-luhman-sig signature) "=" t))
;;    "="))

;; (defun my-denote-sort-for-signatures (sig1 sig2)
;;   "Return non-nil if SIG1 is smaller that SIG2.
;; Perform the comparison with `string<'."
;;   (string< (my-denote--pad-sig sig1) (my-denote--pad-sig sig2)))

;; ;; Change the sorting function only when we sort by signature.
;; (setq denote-sort-signature-comparison-function #'my-denote-sort-for-signatures)

;; Dired
(when (string= system-type "darwin")
  (setq dired-use-ls-dired t
        insert-directory-program "/opt/homebrew/bin/gls" ;; replace ls with gls
        dired-listing-switches "-aBhl --group-directories-first"))
;; Dirvish
(use-package dirvish
  :defer nil
  :bind ("C-c l" . dirvish-side)
  :custom
  (dirvish-quick-access-entries
   '(("h" "~/" "Home")
     ("d" "~/Downloads" "Downloads")
     ("s" "~/Senior" "Senior")))
  (dirvish-default-layout '(0 0.2 0.8))
  (dirvish-attributes '(subtree-state
		        nerd-icons
		        collapse
		        file-size))
  :config
  (dirvish-override-dired-mode) ;; replace dired ui with dirvish
  (dirvish-side-follow-mode))

(use-package sdcv) ;; sdcv local dict

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

(use-package rainbow-mode) ;; show corresponding colors

;; R
(use-package ess)

;; ox-reveal
(use-package ox-reveal
  :config
  (setq org-reveal-root "file:///Users/grant/Code/js/reveal.js-5.0.5"))

(use-package cern-root-mode ;; root mode
  :config
  (setq cern-root-filepath "/opt/homebrew/bin/root"))

(use-package embark
  :bind (("C-." . embark-act))
  :config
  (setq prefix-help-command 'embark-prefix-help-command))

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;; ;; org-roam-ui
;; (use-package org-roam-ui
;;   :after org-roam
;;   :custom
;;   (org-roam-ui-sync-theme t)
;;   (org-roam-ui-follow t) ;; node following
;;   (org-roam-ui-update-on-save t))

;;(setq show-paren-style 'expression) ;; show expression between parens

;; (setq mac-command-modifier 'meta
;;       mac-option-modifier 'super)

;; (use-package ebib
;;   :custom
;;   (ebib-preload-bib-files zot_bib))

;; (defun grant/choose-theme ()
;;   "Prompt user to choose a theme for coding or note-taking."
;;   (let ((choice (read-char-choice
;; 		 "Choose a theme: (c)oding or (n)ote-taking? " '(?c ?n))))
;;     (cond
;;      ((eq choice ?c)
;;       (load-theme 'night-owl t)
;;       (message "Loaded programming theme."))
;;      ((eq choice ?n)
;;       (add-to-list 'load-path "~/.emacs.d/site-lisp/moe-theme.el")
;;       (require 'moe-theme)
;;       (load-theme 'moe-light t)
;;       (message "Loaded note-taking theme."))
;;      )))

;; (grant/choose-theme)

;; (let ((org-latex-preview-scale
;;        (cond
;; 	((= (display-pixel-height) 900) 1.0) ;; Mac: 1440*900 Displayer: 1980*1080
;; 	((= (display-pixel-height) 1080) 1.5)
;; 	(t 1.0))))
;;   (setq org-format-latex-options
;;       `(:foreground "Black"
;; 		    :background "Transparent"
;; 		    :scale ,org-latex-preview-scale)))

;; (setq org-preview-latex-default-process 'imagemagick)
;; (plist-put org-format-latex-options :foreground nil)
;; (plist-put org-format-latex-options :background nil)

(defun xenops-aio-subprocess (command &optional _ __)
  "Start asynchronous subprocess; return a promise.

COMMAND is the command to run as an asynchronous subprocess.

Resolve the promise when the process exits. The value function
does nothing if the exit is successful, but if the process exits
with an error status, then the value function signals the error."
  (let* ((promise (aio-promise))
	 (name (format "xenops-aio-subprocess-%s"
		       (sha1 (prin1-to-string command))))
	 (output-buffer (generate-new-buffer name))
	 (sentinel
	  (lambda (process event)
	    (unless (process-live-p process)
	      (aio-resolve
	       promise
	       (lambda ()
		 (if (or (eq 0 (process-exit-status process))
			 (and (eq 1 (process-exit-status process))
			      (not (string-match-p
				    "^! [^P]"
				    (with-current-buffer output-buffer
				      (buffer-string))))))
		     (kill-buffer output-buffer)
		   (signal 'error
			   (prog1 (list :xenops-aio-subprocess-error-data
					(list (s-join " " command)
					      event
					      (with-current-buffer output-buffer
						(buffer-string))))
			     (kill-buffer output-buffer))))))))))
    (prog1 promise
      (make-process
       :name name
       :buffer output-buffer
       :command command
       :sentinel sentinel))))

(defun eli/xenops-preview-align-baseline (element &rest _args)
  "Redisplay SVG image resulting from successful LaTeX compilation of ELEMENT.

Use the data in log file (e.g. \"! Preview: Snippet 1 ended.(368640+1505299x1347810).\")
to calculate the decent value of `:ascent'. "
  (let* ((inline-p (eq 'inline-math (plist-get element :type)))
	 (ov-beg (plist-get element :begin))
	 (ov-end (plist-get element :end))
	 (colors (xenops-math-latex-get-colors))
	 (latex (buffer-substring-no-properties ov-beg
						ov-end))
	 (cache-svg (xenops-math-compute-file-name latex colors))
	 (cache-log (file-name-with-extension cache-svg "log"))
	 (cache-log-exist-p (file-exists-p cache-log))
	 (tmp-log (f-join temporary-file-directory "xenops"
			  (concat (f-base cache-svg) ".log")))
	 (ov (car (overlays-at (/ (+ ov-beg ov-end) 2) t)))
	 (regexp-string "^! Preview:.*\(\\([0-9]*?\\)\\+\\([0-9]*?\\)x\\([0-9]*\\))")
	 img new-img ascent bbox log-text log)
    (when (and ov inline-p)
      (if cache-log-exist-p
	  (let ((text (f-read-text cache-log)))
	    (string-match regexp-string text)
	    (setq log (match-string 0 text))
	    (setq bbox (mapcar #'(lambda (x)
				   (* (preview-get-magnification)
				      (string-to-number x)))
			       (list
				(match-string 1 text)
				(match-string 2 text)
				(match-string 3 text)))))
	(with-temp-file cache-log
	  (insert-file-contents-literally tmp-log)
	  (goto-char (point-max))
	  (if (re-search-backward regexp-string nil t)
	      (progn
		(setq log (match-string 0))
		(setq bbox (mapcar #'(lambda (x)
				       (* (preview-get-magnification)
					  (string-to-number x)))
				   (list
				    (match-string 1)
				    (match-string 2)
				    (match-string 3))))))
	  (erase-buffer)
	  (insert log)))
      (setq ascent (preview-ascent-from-bb (preview-TeX-bb bbox)))
      (setq img (cdr (overlay-get ov 'display)))
      (setq new-img (plist-put img :ascent ascent))
      (overlay-put ov 'display (cons 'image new-img)))))
(advice-add 'xenops-math-display-image :after
	    #'eli/xenops-preview-align-baseline)

;; (setq xenops-math-latex-process-alist
;;         '((dvisvgm :programs
;;                    ("latex" "dvisvgm")
;;                    :description "dvi > svg" :message "you need to install the programs: latex and dvisvgm." :image-input-type "dvi" :image-output-type "svg" :image-size-adjust
;;                    (1.7 . 1.5)
;;                    :latex-compiler
;;                    ("latex -interaction nonstopmode -shell-escape -output-format dvi -output-directory %o \"\\nonstopmode\\nofiles\\PassOptionsToPackage{active,tightpage,auctex}{preview}\\AtBeginDocument{\\ifx\\ifPreview\\undefined\\RequirePackage[displaymath,floats,graphics,textmath,footnotes]{preview}[2004/11/05]\\fi}\\input\\detokenize{\"%f\"}\" %f")
;;                    :image-converter
;;                    ("dvisvgm %f -n -b %B -c %S -o %O"))))

(use-package xenops
  :hook (org-mode . xenops-mode)
  :custom
  (xenops-math-latex-process 'dvisvgm)
  (xenops-math-image-scale-factor 0.9))

(plist-put org-format-latex-options :justify 'center)
(defun eli/xenops-justify-fragment-overlay (element &rest _args)
    (let* ((ov-beg (plist-get element :begin))
           (ov-end (plist-get element :end))
           (ov (car (overlays-at (/ (+ ov-beg ov-end) 2) t)))
           (position (plist-get org-format-latex-options :justify))
           (inline-p (eq 'inline-math (plist-get element :type)))
           width offset)
      (when (and ov
                 (imagep (overlay-get ov 'display)))
        (setq width (car (image-display-size (overlay-get ov 'display))))
        (cond
         ((and (eq 'center position)
               (not inline-p))
          (setq offset (floor (- (/ fill-column 2)
                                 (/ width 2))))
          (if (< offset 0)
              (setq offset 0))
          (overlay-put ov 'before-string (make-string offset ? )))
         ((and (eq 'right position)
               (not inline-p))
          (setq offset (floor (- fill-column
                                 width)))
          (if (< offset 0)
              (setq offset 0))
          (overlay-put ov 'before-string (make-string offset ? )))))))
(advice-add 'xenops-math-display-image :after
	    #'eli/xenops-justify-fragment-overlay)

(defun eli/xenops-renumber-environment (orig-func element latex colors
                                                    cache-file display-image)
    (let ((results '())
          (counter -1)
          (numberp))
      (setq results (cl-loop for (begin .  env) in
                             (org-element-map (org-element-parse-buffer)
                                 'latex-environment
                               (lambda (env)
                                 (cons
                                  (org-element-property :begin env)
                                  (org-element-property :value env))))
                             collect
                             (cond
                              ((and (string-match "\\\\begin{equation}" env)
                                    (not (string-match "\\\\tag{" env)))
                               (cl-incf counter)
                               (cons begin counter))
                              ((and (string-match "\\\\begin{align}" env)
                                    (string-match "\\\\notag" env))
                               (cl-incf counter)
                               (cons begin counter))
                              ((string-match "\\\\begin{align}" env)
                               (prog2
                                   (cl-incf counter)
                                   (cons begin counter)
                                 (with-temp-buffer
                                   (insert env)
                                   (goto-char (point-min))
                                   ;; \\ is used for a new line. Each one leads
                                   ;; to a number
                                   (cl-incf counter (count-matches "\\\\$"))
                                   ;; unless there are nonumbers.
                                   (goto-char (point-min))
                                   (cl-decf counter
                                            (count-matches "\\nonumber")))))
                              (t
                               (cons begin nil)))))
      (when (setq numberp (cdr (assoc (plist-get element :begin) results)))
        (setq latex
              (concat
               (format "\\setcounter{equation}{%s}\n" numberp)
               latex))))
    (funcall orig-func element latex colors cache-file display-image))
(advice-add 'xenops-math-latex-create-image :around #'eli/xenops-renumber-environment)

;;; init-unused.el ends here
