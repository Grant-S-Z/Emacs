;;; init-tramp.el --- for remote editing
;;; Commentary:
;;; Code:
(require 'tramp)

(setq tramp-default-method "ssh")

(add-to-list 'tramp-remote-path "/opt/gentoo/usr/lib/llvm/19/bin")

(provide 'init-tramp)
