;;; init-elpa.el -- archives of emacs  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
(setq package-check-signature nil) ; verify package signatures
(setq package-install-upgrade-built-in nil) ; no auto upgrade

;; Sources
(require 'package)
;; Make Org main visible before activating packages that may load Org.
(let ((org-main-lisp (expand-file-name "elpa/org/lisp" user-emacs-directory)))
  (when (file-directory-p org-main-lisp)
    (add-to-list 'load-path org-main-lisp)))
(package-initialize)

(setq package-archives '(("gnu" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
                         ("melpa" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
                         ("nongnu" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/nongnu/")))

;; Use-package settings
(eval-and-compile
  (setq use-package-always-ensure t ;; 自动确保安装
	use-package-always-defer t ;; 延迟加载
	use-package-always-demand nil ;; demand 可覆盖触发器，强制立即加载
	use-package-expand-minimally t
	use-package-verbose nil))

(provide 'init-elpa)
;;; init-elpa.el ends here
