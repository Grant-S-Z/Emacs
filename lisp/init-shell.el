;;; init-shell.el --- for shell  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
;;; Get commands work in shell and know system variables
(use-package exec-path-from-shell
  :demand t
  :if (memq window-system '(mac ns x neo))
  :config
  ;; 只启动非交互式登录 zsh：读取 ~/.zprofile 中的 PATH，
  ;; 避免加载 Oh My Zsh、Powerlevel10k、Conda hook 等交互式配置。
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
