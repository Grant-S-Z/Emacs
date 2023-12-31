;;; init-elpa.el -- archives of emacs
;;; Commentary:
;;; Code:
(setq package-check-signature nil) ; No checking signature

;;; 配置 package 源
(require 'package)

;; (setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
;; 			 ("org" . "https://orgmode.org/elpa/")
;; 			 ("melpa" . "https://melpa.org/packages/")
;; 			 ("melpa-stable" . "https://stable.melpa.org/packages/"))
;;       package-archive-priorities '(("melpa-stable" . 1)))

;; thu
(setq package-archives '(("gnu" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
                         ("melpa" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
                         ("nongnu" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/nongnu/")))

;;; Use-package 配置
(eval-and-compile
  (setq use-package-always-ensure t ;; 自动确保安装
	use-package-always-defer t ;; 默认延迟加载
	use-package-always-demand nil
	use-package-expand-minimally t
	use-package-verbose t))

(require 'use-package) ;; use-package 现已内置

(provide 'init-elpa)
;;; init-elpa.el ends here
