;;; init-etaf.el --- ETAF and the Claude-chan pet  -*- lexical-binding: t; -*-
;;; Commentary:
;; The pet lives in site-lisp as a small standalone project.  Its artwork is
;; private fan art: do not redistribute anything under assets/.
;;; Code:

(use-package etaf-pet
  :ensure nil                            ; not on MELPA, loaded from site-lisp
  :load-path "~/.emacs.d/site-lisp/etaf-pet"
  :commands (etaf-pet-open
             etaf-pet-quit
             etaf-pet-toggle-transparency)
  :bind (("C-c p" . etaf-pet-open)                  ; toggle the floating pet
         ("C-c P" . etaf-pet-toggle-transparency))) ; opaque panel <-> transparent window

(provide 'init-etaf)
;;; init-etaf.el ends here
