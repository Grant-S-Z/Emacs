;;; init-tex.el --- for tex
;;; Commentary:
;;; Code:

;;; TeX
(use-package tex
  :defer t
  :ensure auctex
  :config
  (setq TeX-auto-save t) ;; 自动保存
  (setq TeX-parse-self t)
  (setq-default TeX-master t) ;; 编译时不询问主文件名称
  (setq-default TeX-engine 'xetex) ;; 使用 xelatex 作为 TeX-engine

  (add-hook 'LaTeX-mode-hook (lambda ()
	      (add-to-list 'TeX-command-list '("XeLaTeX" "%'xelatex --synctex=1 --shell-escape%(mode)%' %t" TeX-run-TeX nil t)) ;; xelatex 编译
	      ;; (setq prettify-symbols-mode t) ;; 加载 prettify-symbols-mode
	      (setq TeX-command-default "LatexMk") ;; 默认使用 latexmk
	      (setq TeX-show-compilation nil) ;; 不展示编译过程
	      (turn-on-cdlatex) ;; 加载 cdlatex
	      (turn-on-reftex) ;; 加载 reftex
	      (auctex-latexmk-setup) ;; 使用 latexmk
	      (outline-minor-mode) ;; 加载 outline-minor-mode
	      (outline-hide-body) ;; 只显示标题

	      ;; pdf 预览
	      (setq TeX-PDF-mode t)
	      (setq TeX-source-correlate-mode t) ;; 编译后开启正反向搜索
	      (setq TeX-source-correlate-method 'syntax) ;; 搜索执行方式

	      (setq TeX-view-program-list '(("PDF Tools" "TeX-pdf-tools-sync-view")))
	      (setq TeX-view-program-selection '((output-pdf "PDF Tools"))) ;; 用 pdf-tools 预览
	      (add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer) ;; 完成编译后刷新 pdf

	      (add-hook 'pdf-view-mode-hook 'pdf-view-fit-width-to-window))))

;; latexmk
(use-package auctex-latexmk
  :after tex
  :config
  (setq auctex-latexmk-inherit-TeX-PDF-mode t))

;; cdlatex
(use-package cdlatex
  :after tex)

;;; PDF preview
;; pdf-tools
(use-package pdf-tools
  :init (pdf-loader-install))

(provide 'init-tex)
;;; init-tex.el ends here
