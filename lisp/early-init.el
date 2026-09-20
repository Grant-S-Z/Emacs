;;; early-init.el --- early init settings  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
;; (setq frame-inhibit-implied-resize t)
(setq window-resize-pixelwise t
      frame-resize-pixelwise t)
(add-to-list 'default-frame-alist '(undecorated-round . t))

(provide 'early-init)
