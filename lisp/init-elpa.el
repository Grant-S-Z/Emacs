;;; init-elpa.el -- archives of emacs
;;; Commentary:
;;; Code:
(setq package-check-signature nil) ; No checking signature
(setq package-install-upgrade-built-in t) ; 内嵌 package 自动更新

;;; 配置 package 源
(require 'package)

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

(require 'async) ;; 异步
(autoload 'dired-async-mode "dired-async.el" nil t)
(dired-async-mode 1)
(async-bytecomp-package-mode 1)

(provide 'init-elpa)
;;; init-elpa.el ends here
