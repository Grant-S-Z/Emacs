;;; init-const.el --- for some const
;;; Commentary:
;;; Code:

;; 判断操作系统
(defconst *is-mac* (eq system-type 'darwin))
(defconst *is-linux* (eq system-type 'gnu/linux))
(defconst *is-windows* (or (eq system-type 'ms-dos) (eq system-type 'windows-nt)))

(provide 'init-const)
;;; init-const.el ends here
