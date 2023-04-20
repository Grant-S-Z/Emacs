;;; init-tex.el --- for tex
;;; Commentary:
;;; Code:

;; 基本配置
(use-package tex
  :ensure auctex

  :config
  (setq TeX-auto-save t) ;; 自动保存
  (setq TeX-parse-self t)
  (setq-default TeX-master nil)
  (setq TeX-PDF-mode t)

  (add-hook 'LaTeX-mode-hook (lambda ()
	      (add-to-list 'TeX-command-list '("XeLaTeX" "%'xelatex%(mode)%' %t" TeX-run-TeX nil t)) ;; xelatex编译
	      (setq prettify-symbols-mode t) ;; 加载prettify-symbols-mode
	      (setq TeX-command-default "XeLaTeX") ;; 默认使用xelatex
	      (setq TeX-show-compilation t) ;; 展示编译过程
	      (turn-on-cdlatex) ;; 加载cdlatex
	      (turn-on-reftex) ;; 加载reftex
	      (auctex-latexmk-setup) ;; 使用latexmk
	      (outline-minor-mode) ;; 加载outline-minor-mode
	      (outline-hide-body) ;; 只显示标题



	      ;; pdf预览
	      (setq TeX-view-program-list
		    '(("PDF Viewer" "/Applications/Skim.app/Contents/SharedSupport/displayline -b -g %n %o %b"))) ;; -b highlights the current line, and -g oepns skim in the background
	      (setq TeX-view-program-selection '((output-pdf "PDF Viewer")))
	      (server-start) ;; make sure that skim can talk to emacs
	      )))

;; latexmk
(use-package auctex-latexmk
  :after tex
  :config
  (setq auctex-latexmk-inherit-TeX-PDF-mode t))

;; cdlatex
(use-package cdlatex
  :after tex)

(add-hook 'tex-mode-hook (lambda ()
			   (local-set-key (kbd "C-c &") 'yas-insert-snippet)))

(provide 'init-tex)
;;; init-tex.el ends here
