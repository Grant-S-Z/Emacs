;;; init-tex.el --- for tex
;;; Commentary:
;;; Code:
;;; TeX settings
(use-package tex
  :ensure auctex
  :config
  (setq TeX-auto-save t) ;; 自动保存
  (setq TeX-parse-self t)
  (setq-default TeX-master t) ;; 编译时不询问主文件名称
  (setq-default TeX-engine 'xetex) ;; 使用 xelatex 作为 TeX-engine
  (add-hook 'LaTeX-mode-hook (lambda ()
	      (add-to-list 'TeX-command-list '("XeLaTeX" "%'xelatex --synctex=1 --shell-escape%(mode)%' %t" TeX-run-TeX nil t)) ;; xelatex 编译
	      ;(setq prettify-symbols-mode t) ;; 加载 prettify-symbols-mode, 不适合 LaTeX 编辑
	      (setq TeX-command-default "LatexMk") ;; 默认使用 latexmk
	      (setq TeX-show-compilation nil) ;; 不展示编译过程
	      (turn-on-cdlatex) ;; 加载 cdlatex
	      (turn-on-reftex) ;; 加载 reftex
	      (auctex-latexmk-setup) ;; 使用 latexmk
n	      (outline-minor-mode) ;; 加载 outline-minor-mode
	      (outline-hide-body) ;; 只显示标题

	      ;; pdf 预览
	      (setq TeX-PDF-mode t)
	      (setq TeX-source-correlate-mode t) ;; 编译后开启正反向搜索
	      (setq TeX-source-correlate-method 'syntax) ;; 搜索执行方式
	      (setq TeX-view-program-list '(("Skim" "/Applications/Skim.app/Contents/SharedSupport/displayline -b -g %n %o %b") ;; Skim
					    ("PDF Tools" "TeX-pdf-tools-sync-view"))) ;; pdf-tools
	      (setq TeX-view-program-selection '((output-pdf "PDF Tools")))
	      (add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer) ;; 完成编译后刷新 pdf
	      (add-hook 'pdf-view-mode-hook 'pdf-view-fit-width-to-window))))

;; latexmk
(use-package auctex-latexmk
  :after tex
  :config
  (setq auctex-latexmk-inherit-TeX-PDF-mode t))

;; cdlatex
(use-package cdlatex
  :after tex
  :hook ((org-mode . org-cdlatex-mode)
	 (tex-mode . cdlatex-mode))
  :config
  (add-to-list 'cdlatex-command-alist
	       '(("qt" "Insert \\qty{}{}" "\\qty{?}{}" cdlatex-position-cursor nil t nil)
		 ))
  ;;; LaTeX environment
  )

;;; PDF view
;; pdf-tools
(use-package pdf-tools
  :init (pdf-loader-install)
  ;:hook (pdf-view-mode . pdf-view-midnight-minor-mode)
  :custom
  (pdf-view-incompatible-modes '(linum-mode linum-relative-mode helm-linum-relative-mode nlinum-mode nlinum-hl-mode nlinum-relative-mode yalinum-mode))
  :config
  (define-key pdf-view-mode-map
	      "d" 'pdf-view-next-page-command) ;; 向后翻页
  (define-key pdf-view-mode-map
	      "a" 'pdf-view-previous-page-command) ;; 向前翻页
  (define-key pdf-view-mode-map
	      "s" 'pdf-view-scroll-up-or-next-page) ;; 向下滑动
  (define-key pdf-view-mode-map
	      "w" 'pdf-view-scroll-down-or-previous-page) ;; 向上滑动
  (require 'pdf-annot)
  (define-key pdf-annot-minor-mode-map (kbd "C-a a") 'pdf-annot-add-highlight-markup-annotation)
  (define-key pdf-annot-minor-mode-map (kbd "C-a s") 'pdf-annot-add-squiggly-markup-annotation)
  (define-key pdf-annot-minor-mode-map (kbd "C-a f") 'pdf-annot-add-underline-markup-annotation)
  (define-key pdf-annot-minor-mode-map (kbd "C-a d") 'pdf-annot-delete)
  (require 'saveplace-pdf-view)
  (save-place-mode 1))

(provide 'init-tex)
;;; init-tex.el ends here
