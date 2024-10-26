;;; init-shell.el --- for shell
;;; Commentary:
;;; Code:
;;; Get commands work in shell and know system variables
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

;;; Shell vterm
(use-package vterm :init (setq vterm-shell "zsh"))

(use-package
 vterm-toggle
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
