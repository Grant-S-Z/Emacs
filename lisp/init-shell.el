;;; init-shell.el --- shell setup  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
;;; Shell environment
(use-package exec-path-from-shell
  :demand t
  :if (memq window-system '(mac ns x neo))
  :config
  ;; Run a non-interactive login zsh so that only ~/.zprofile is read;
  ;; this avoids loading Oh My Zsh, Powerlevel10k and the conda hook.
  (setq exec-path-from-shell-shell-name "/bin/zsh"
        exec-path-from-shell-arguments '("-l"))
  (exec-path-from-shell-initialize))

;;; Shell vterm
;; Use the macOS system compiler for native modules.
(setenv "CC" "/usr/bin/clang")
(setenv "CXX" "/usr/bin/clang++")

(use-package vterm
  :init (setq vterm-shell "zsh"))

(use-package vterm-toggle
  :bind ("C-c s" . vterm-toggle)
  :config (setq vterm-toggle-fullscreen-p nil)
  (add-to-list
   'display-buffer-alist
   '((lambda (buffer-or-name _)
       (let ((buffer (get-buffer buffer-or-name)))
         (with-current-buffer buffer
           (or (equal major-mode 'vterm-mode)
               (string-prefix-p
                vterm-buffer-name (buffer-name buffer))))))
     (display-buffer-reuse-window display-buffer-at-bottom)
     (reusable-frames . visible)
     (window-height . 0.3))))

(provide 'init-shell)
;;; init-shell.el ends here
