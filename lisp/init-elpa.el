;;; init-elpa.el -- archives of emacs
;;; Commentary:
;;; Code:
(setq package-check-signature nil) ; No checking signature

;; 配置package源
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

;; ustc
;; (setq package-archives '(("gnu" . "https://mirrors.ustc.edu.cn/elpa/gnu/")
;;                          ("melpa" . "https://mirrors.ustc.edu.cn/elpa/melpa/")
;;                          ("nongnu" . "https://mirrors.ustc.edu.cn/elpa/nongnu/")))

;; 初始化与自动更新
(unless (bound-and-true-p package--initialized)
  (package-initialize))

(unless package-archive-contents
  (package-refresh-contents))

;; Use-package配置
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(setq use-package-always-ensure t
      use-package-always-defer t
      use-package-always-demand nil
      use-package-expand-minimally t
      use-package-verbose t)

(require 'use-package)

(provide 'init-elpa)
;;; init-elpa.el ends here
