;;; init-lisp.el --- for lisp
;;; Commentary:
;;; Code:
(use-package geiser-mit
  :config
  (setq geiser-active-implementations '(mit))
  (setq geiser-default-implementation 'mit)
  (add-hook 'scheme-mode-hook 'geiser-mode)
  (add-to-list 'auto-mode-alist '("\\.scm\\'" . geiser-mode)))

(provide 'init-lisp)
;;; init-lisp.el ends here
