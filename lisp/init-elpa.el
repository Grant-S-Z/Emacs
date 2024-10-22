;;; init-elpa.el -- archives of emacs
;;; Commentary:
;;; Code:
(setq package-check-signature nil) ; No checking signature
(setq package-install-upgrade-built-in t) ; 内嵌 package 自动更新

;; Sources
(require 'package)

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

;; GC
(require 'gcmh)
(gcmh-mode 1)

;; Async
(require 'async)
(autoload 'dired-async-mode "dired-async.el" nil t)
(dired-async-mode 1)
(async-bytecomp-package-mode 1)


(provide 'init-elpa)
;;; init-elpa.el ends here
