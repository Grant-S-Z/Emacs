;;; init-elpa.el -- package archives  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
(setq package-check-signature nil)          ; no signature checks
(setq package-install-upgrade-built-in nil) ; never upgrade built-in packages

;; Sources
(require 'package)
;; Put the Org main branch ahead of the built-in Org before packages load it.
(let ((org-main-lisp (expand-file-name "elpa/org/lisp" user-emacs-directory)))
  (when (file-directory-p org-main-lisp)
    (add-to-list 'load-path org-main-lisp)))
(package-initialize)

(setq package-archives '(("gnu" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
                         ("melpa" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
                         ("nongnu" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/nongnu/")))

;; Use-package defaults
(eval-and-compile
  (setq use-package-always-ensure t
        use-package-always-defer t
        use-package-always-demand nil ; :demand overrides deferral per package
        use-package-expand-minimally t
        use-package-verbose nil))

(provide 'init-elpa)
;;; init-elpa.el ends here
