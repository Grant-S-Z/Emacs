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

;;; Ghostel terminal
;; Compiler for native modules: clang on macOS, gcc on Linux/WSL.
(if *is-wsl*
    (progn
      (setenv "CC" "/usr/bin/gcc")
      (setenv "CXX" "/usr/bin/g++"))
  (setenv "CC" "/usr/bin/clang")
  (setenv "CXX" "/usr/bin/clang++"))

(defun grant/ghostel-toggle ()
  "Toggle a Ghostel terminal in a bottom window."
  (interactive)
  (let* ((buffer (seq-find
                  (lambda (buf)
                    (with-current-buffer buf
                      (derived-mode-p 'ghostel-mode)))
                  (buffer-list)))
         (window (and buffer (get-buffer-window buffer t))))
    (cond
     (window
      (condition-case nil
          (delete-window window)
        (error (bury-buffer buffer))))
     ((and buffer (buffer-local-value 'ghostel--term buffer))
      (pop-to-buffer buffer))
     (buffer
      (kill-buffer buffer)
      (ghostel))
     (t
      (ghostel)))))

(use-package ghostel
  :ensure t
  :init
  ;; Keep the native module outside the package tree so upgrades do not
  ;; remove or overwrite it.
  (setq ghostel-module-directory (expand-file-name "~/.cache/ghostel/"))
  :bind ("C-c s" . grant/ghostel-toggle)
  :config
  (setq ghostel-shell "zsh")
  (add-to-list
   'display-buffer-alist
   '((lambda (buffer-or-name _)
       (let ((buffer (get-buffer buffer-or-name)))
         (and buffer
              (with-current-buffer buffer
                (derived-mode-p 'ghostel-mode)))))
     (display-buffer-reuse-window display-buffer-at-bottom)
     (reusable-frames . visible)
     (window-height . 0.3))))

;;; SSH
(with-eval-after-load 'tramp
  (setq tramp-completion-use-cache nil))

(provide 'init-shell)
;;; init-shell.el ends here
