;;; init-ai.el --- LLM clients and AI agents  -*- lexical-binding: t; -*-
;;; Commentary:
;; agent-shell / agent-shell-math-renderer
;;; Code:

;;; agent-shell: a CLI coding agent over ACP, inside Emacs
(use-package agent-shell
  :bind (("C-c k" . agent-shell)
         ("C-c K" . agent-shell-toggle))
  :custom
  ;; Open the shell in a right side window instead of replacing the buffer
  ;; you are reading.
  (agent-shell-display-action
   '((display-buffer-in-side-window)
     (side . right) (slot . 0) (window-width . 0.45)))
  ;; Files the agent opens must not take over the shell window.
  (agent-shell-file-display-action
   '((display-buffer-reuse-window display-buffer-pop-up-window)
     (inhibit-same-window . t)))
  ;; Kimi matches model IDs case-sensitively against ~/.kimi-code/config.toml.
  ;; The WSL config spells this model in lower case; the mac config uses the
  ;; capitalized key.
  (agent-shell-kimi-default-model-id
   (if *is-wsl* "deepseek/deepseek-flash" "DeepSeek/deepseek-flash"))
  ;; WSL has no logind session, so the D-Bus inhibit call is denied; disable
  ;; it there.  macOS can still block idle sleep through native APIs.
  (agent-shell-inhibit-system-sleep (not *is-wsl*))
  :config
  ;; (setq agent-shell-preferred-agent-config 'pi)
  ;; (setq agent-shell-preferred-agent-config 'omp)
  (setq agent-shell-preferred-agent-config 'kimi)
  ;; GUI Emacs does not inherit the terminal PATH; use an absolute path.
  (setq agent-shell-kimi-acp-command
        (if *is-mac*
            '("/opt/homebrew/bin/kimi" "acp")
          '("~/.kimi-code/bin/kimi" "acp")))
  (setq agent-shell-kimi-default-session-mode-id "auto")
  ;; Replay the whole history when restoring a session via session/load.
  ;; The default 'minimal shows only the session title; last / first-last / full
  ;; are also available.
  (setq agent-shell-session-restore-verbosity 'last)
  ;; Ask whether to restore a session or start a new one.
  (setq agent-shell-session-strategy 'prompt)
  ;; M-x grant/agent-shell-here starts in the current buffer's directory and
  ;; leaves the global cwd detection alone.
  (defun grant/agent-shell-here (dir)
    "Start agent-shell in DIR without resolving the project root.
Interactive use defaults to the current buffer's directory."
    (interactive
     (list (read-directory-name "agent-shell 工作目录: " default-directory)))
    (let ((agent-shell-cwd-function (lambda () dir)))
      (agent-shell))))

;;; agent-shell-math-renderer: render LaTeX in replies as SVG
(use-package agent-shell-math-renderer
  :after agent-shell
  :hook (agent-shell-mode . agent-shell-math-renderer-mode)
  :config
  ;; Recolour and rescale formulas when the theme or font size changes.
  (add-hook 'enable-theme-functions
            #'agent-shell-math-renderer-on-appearance-change)
  (add-hook 'after-setting-font-hook
            #'agent-shell-math-renderer-on-appearance-change)
  (setq agent-shell-math-renderer-rescale-inline 0.9)
  (setq agent-shell-math-renderer-rescale-display 0.9)
  ;; Allow Chinese in formulas.
  (setq latex-to-svg-backend-appended-preamble
        "\\usepackage[UTF8,fontset=fandol]{ctex}")
  )

(provide 'init-ai)
;;; init-ai.el ends here
