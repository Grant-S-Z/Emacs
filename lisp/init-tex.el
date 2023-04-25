;;; init-tex.el --- for tex
;;; Commentary:
;;; Code:

;; 基本配置
(use-package tex
  :ensure auctex

  :init
  (pdf-tools-install) ;; 使用pdf-tools替代docview

  :config
  (setq TeX-auto-save t) ;; 自动保存
  (setq TeX-parse-self t)
  (setq-default TeX-master t) ;; 编译时不询问主文件名称

  (add-hook 'LaTeX-mode-hook (lambda ()
	      (add-to-list 'TeX-command-list '("XeLaTeX" "%'xelatex%(mode) --synctax=1%' %t" TeX-run-TeX nil t)) ;; xelatex编译
	      (setq prettify-symbols-mode t) ;; 加载prettify-symbols-mode
	      (setq TeX-command-default "LatexMk") ;; 默认使用latexmk
	      (setq TeX-show-compilation nil) ;; 不展示编译过程
	      (turn-on-cdlatex) ;; 加载cdlatex
	      (turn-on-reftex) ;; 加载reftex
	      (auctex-latexmk-setup) ;; 使用latexmk
	      (outline-minor-mode) ;; 加载outline-minor-mode
	      (outline-hide-body) ;; 只显示标题

	      ;; pdf预览
	      (setq TeX-PDF-mode t)
	      (setq TeX-source-correlate-mode t)
	      (setq TeX-source-correlate-method 'syntax)

	      (setq TeX-view-command-raw "pdf-tools")
	      (setq TeX-view-program-list '(("PDF Tools" "TeX-pdf-tools-sync-view")))
	      (setq TeX-view-program-selection '((output-pdf "PDF Tools"))) ;; 用pdf-tools预览
	      (add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer) ;; 完成编译后刷新pdf

	      (add-hook 'pdf-view-mode-hook 'pdf-view-fit-width-to-window)

	      ;; (setq TeX-view-program-list
	      ;; 	    '(("PDF Viewer" "/Applications/Skim.app/Contents/SharedSupport/displayline -b -g %n %o %b"))) ;; -b highlights the current line, and -g oepns skim in the background
	      ;; (setq TeX-view-program-selection '((output-pdf "PDF Viewer")))
	      ;; (server-start) ;; make sure that skim can talk to emacs
	      )))

;; latexmk
(use-package auctex-latexmk
  :after tex
  :config
  (setq auctex-latexmk-inherit-TeX-PDF-mode t))

;; cdlatex
(use-package cdlatex
  :after tex)

;; pdf-tools
(use-package pdf-tools)

(provide 'init-tex)
;;; init-tex.el ends here
