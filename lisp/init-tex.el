;;; init-tex.el --- for tex  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
;;; TeX settings
(use-package tex
  :ensure auctex
  :config
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq-default TeX-master t) ;; make emacs aware of multi-file projects
  (add-hook 'LaTeX-mode-hook (lambda ()
			       (setq TeX-command-default "LatexMk")
			       (setq TeX-show-compilation nil)
			       (turn-on-cdlatex)
			       (turn-on-reftex)
			       (auctex-latexmk-setup)
			       (outline-minor-mode)
			       (outline-hide-body) ;; show title only
			       )))

;; pdf 预览
(setq TeX-PDF-mode t)
(setq TeX-source-correlate-mode t) ;; 编译后开启正反向搜索
(setq TeX-source-correlate-method 'syntax) ;; 搜索执行方式
(setq TeX-source-correlate-start-server t)
(setq TeX-view-program-list '(("Skim" "/Applications/Skim.app/Contents/SharedSupport/displayline -b -g %n %o %b"))) ;; Skim
(setq TeX-view-program-selection '((output-pdf "Skim")))
(add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer) ;; refresh pdf after compilation
(add-hook 'pdf-view-mode-hook 'pdf-view-fit-width-to-window) ;; auto fit width

;; Latexmk
(use-package auctex-latexmk
  :after tex
  :config
  (setq auctex-latexmk-inherit-TeX-PDF-mode t))

;; Cdlatex
(use-package cdlatex
  :after tex
  :hook ((org-mode . org-cdlatex-mode)
	 (tex-mode . cdlatex-mode))
  :config
  (add-to-list 'cdlatex-command-alist
	       '(("qt" "Insert \\qty{}{}" "\\qty{?}{}" cdlatex-position-cursor nil t nil))))

(provide 'init-tex)
;;; init-tex.el ends here
