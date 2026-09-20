;;; init-elpa.el -- archives of emacs  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
(setq package-check-signature nil) ; no checking signature
(setq package-install-upgrade-built-in nil) ; no auto upgrade

;; Sources
(require 'package)

;; (setq package-archives '(("gnu"   . "https://elpa.gnu.org/packages/")
;; 			 ("melpa" . "https://melpa.org/packages/")
;; 			 ("nongnu" . "https://elpa.nongnu.org/nongnu/")))

(setq package-archives '(("gnu" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
                         ("melpa" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
                         ("nongnu" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/nongnu/")))

;; Use-package settings
(eval-and-compile
  (setq use-package-always-ensure t ;; 自动确保安装
	use-package-always-defer t ;; 延迟加载
	use-package-always-demand nil ;; demand 可覆盖触发器，强制立即加载
	use-package-expand-minimally t
	use-package-verbose t))

;; Async
;; (use-package async
;;   :ensure t
;;   :init
;;   (autoload 'dired-async-mode "dired-async.el" nil t)
;;   (dired-async-mode 1)
;;   (async-bytecomp-package-mode 1))

(provide 'init-elpa)
;;; init-elpa.el ends here
