;;; init-const.el --- for some const
;;; Commentary:
;;; Code:

;; 判断操作系统
(defconst *is-mac* (eq system-type 'darwin))
(defconst *is-linux* (eq system-type 'gnu/linux))
(defconst *is-windows* (or (eq system-type 'ms-dos) (eq system-type 'windows-nt)))

;; 位置
(setq calendar-location-name "Beijing, CN")
(setq calendar-latitude 39.9042)
(setq calendar-longitude 116.4074)

(provide 'init-const)
;;; init-const.el ends here
