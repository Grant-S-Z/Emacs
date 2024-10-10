;;; init-cpp.el --- for c++
;;; Commentary:
;;; Code:

;; LSP
(setq lsp-bridge-c-lsp-server "clangd")

;; Quickrun
(quickrun-add-command "c++/c1z"
  '((:command . "clang++")
    (:exec    . ("%c -std=c++1z %o -o %e %s"
		 "%e %a"))
    (:remove  . ("%e")))
  :default "c++")

;; ROOT
(use-package cern-root-mode
  :config
  (setq cern-root-filepath "~/miniconda3/envs/hep/bin/root"))

(provide 'init-cpp)
;;; init-cpp.el ends here
