;;; init-const.el --- for some const
;;; Commentary:
;;; Code:
;;; Operation system
(defconst *is-mac* (eq system-type 'darwin))
(defconst *is-linux* (eq system-type 'gnu/linux))
;; No one will use windows Emacs
(defconst *is-windows* (or (eq system-type 'ms-dos) (eq system-type 'windows-nt)))

;;; Alias
(defalias 'yes-or-no-p 'y-or-n-p) ;; yes-or-no

(provide 'init-const)
;;; init-const.el ends here
