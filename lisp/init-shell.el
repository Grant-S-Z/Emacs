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
  ;; A broken shell profile should not abort startup; fall back to the
  ;; PATH Emacs inherited from the launcher.
  (condition-case err
      (progn
        (exec-path-from-shell-initialize)
        (setq exec-path (delete-dups exec-path))
        (setenv "PATH" (mapconcat #'identity exec-path ":")))
    (error (message "exec-path-from-shell failed: %S" err))))

;;; Shell vterm
;; Compiler for native modules: clang on macOS, gcc on Linux/WSL.
(if *is-wsl*
    (progn
      (setenv "CC" "/usr/bin/gcc")
      (setenv "CXX" "/usr/bin/g++"))
  (setenv "CC" "/usr/bin/clang")
  (setenv "CXX" "/usr/bin/clang++"))

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
