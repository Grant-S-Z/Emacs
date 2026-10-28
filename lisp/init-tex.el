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
;; (setq TeX-view-program-list '(("Skim" "/Applications/Skim.app/Contents/SharedSupport/displayline -b -g %n %o %b")))
;; (setq TeX-view-program-selection '((output-pdf "Skim")))
;; 用 pdf-tools 作为 AUCTeX 的 PDF 查看器（否则 macOS 默认回退到 open → Skim）
(setq TeX-view-program-selection '((output-pdf "PDF Tools")))
(add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer) ;; refresh pdf after compilation
(add-hook 'pdf-view-mode-hook 'pdf-view-fit-width-to-window) ;; auto fit width

;; Latexmk
(use-package auctex-latexmk
  :after tex
  :config
  (setq auctex-latexmk-inherit-TeX-PDF-mode t))

;; Cdlatex
(use-package cdlatex
  :hook ((org-mode . org-cdlatex-mode)
	 (tex-mode . cdlatex-mode))
  :config
  (add-to-list 'cdlatex-command-alist
	       '(("qt" "Insert \\qty{}{}" "\\qty{?}{}" cdlatex-position-cursor nil t nil))))

;; PDF tools
(use-package pdf-tools
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :config (pdf-tools-install))

;; PDF 演示模式（pdf-tools 之上）：全屏 + 整页适配 + 隐藏干扰元素
;; 用法：在 PDF buffer 中按 P 或 M-x grant-pdf-present-mode；q 退出，m 反色
(defvar-local grant-pdf-present--saved nil
  "State saved before entering `grant-pdf-present-mode'.")

(defvar grant-pdf-present-mode nil
  "Non-nil when `grant-pdf-present-mode' is active (forward declaration).")

(defun grant-pdf-present--refit ()
  "Refit current page to window after page changes."
  (when (and (derived-mode-p 'pdf-view-mode) grant-pdf-present-mode)
    (pdf-view-fit-page-to-window)))

(define-minor-mode grant-pdf-present-mode
  "Present the current PDF full-screen via pdf-tools."
  :lighter " Present"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "q") #'grant-pdf-present-mode)
            (define-key map (kbd "m") #'pdf-view-midnight-minor-mode)
            map)
  (if grant-pdf-present-mode
      (progn
        (unless (derived-mode-p 'pdf-view-mode)
          (setq grant-pdf-present-mode nil)
          (user-error "grant-pdf-present-mode 只适用于 pdf-view 缓冲区"))
        (setq grant-pdf-present--saved
              (list :fullscreen (frame-parameter nil 'fullscreen)
                    :mode-line mode-line-format
                    :cursor cursor-type
                    :tab-bar (bound-and-true-p tab-bar-mode)))
        (unless (frame-parameter nil 'fullscreen)
          (toggle-frame-fullscreen))
        (when (bound-and-true-p tab-bar-mode)
          (tab-bar-mode -1))
        (setq-local mode-line-format nil
                    cursor-type nil)
        (display-line-numbers-mode -1)
        (pdf-view-fit-page-to-window)
        (add-hook 'pdf-view-after-change-page-hook
                  #'grant-pdf-present--refit nil t)
        (message "演示模式：n/p/SPC/DEL/方向键翻页，m 反色，q 退出"))
    (remove-hook 'pdf-view-after-change-page-hook
                 #'grant-pdf-present--refit t)
    (when-let* ((state grant-pdf-present--saved))
      (unless (eq (frame-parameter nil 'fullscreen)
                  (plist-get state :fullscreen))
        (toggle-frame-fullscreen))
      (when (plist-get state :tab-bar)
        (tab-bar-mode 1))
      (setq-local mode-line-format (plist-get state :mode-line)
                  cursor-type (plist-get state :cursor)))
    (setq grant-pdf-present--saved nil)))

(with-eval-after-load 'pdf-view
  (define-key pdf-view-mode-map (kbd "P") #'grant-pdf-present-mode))

(provide 'init-tex)
;;; init-tex.el ends here
