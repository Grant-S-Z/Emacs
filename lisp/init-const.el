;;; init-const.el --- for some const  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
;;; Operation system
(defconst *is-mac* (eq system-type 'darwin))
(defconst *is-linux* (eq system-type 'gnu/linux))
(defconst *is-wsl*
  (and *is-linux*
       (or (getenv "WSL_DISTRO_NAME")
           (getenv "WSL_INTEROP")
           (ignore-errors
             (with-temp-buffer
               (insert-file-contents "/proc/sys/kernel/osrelease")
               (string-match-p "[Mm]icrosoft" (buffer-string))))
           (file-exists-p "/mnt/c/Windows"))
       t))
(defconst *is-windows* (or (eq system-type 'ms-dos) (eq system-type 'windows-nt)))

;;; Alias
(defalias 'yes-or-no-p 'y-or-n-p)

(provide 'init-const)
;;; init-const.el ends here
