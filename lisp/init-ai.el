;;; init-ai.el --- LLM clients and AI agents  -*- lexical-binding: t; -*-
;;; Commentary:
;; gptel / gptel-agent / agent-shell / agent-shell-math-renderer
;;; Code:

;;; gptel & gptel-agent
(when *is-mac*
  (defun osx-get-keychain-password (account-name)
    "Gets ACCOUNT-NAME keychain password from OS X Keychain."
    (let ((cmd (concat "security 2>&1 >/dev/null find-generic-password -ga '" account-name "'")))
      (let ((passwd (shell-command-to-string cmd)))
	(when (string-match (rx "\"" (group (0+ (or (1+ (not (any "\"" "\\"))) (seq "\\" anything)))) "\"") passwd)
	  (match-string 1 passwd)))))
  (use-package gptel
    :bind
    (("C-c q" . gptel)
     ("C-c d" . gptel-add-file)
     ("C-c p" . gptel-add))
    :config
    (setq gptel-default-mode 'org-mode)
    (setq gptel-model 'deepseek-flash
	  gptel-backend (gptel-make-deepseek "DeepSeek"
			  :stream t
			  :models '(deepseek-flash)
			  :key (lambda () (osx-get-keychain-password "deepseek key"))))))

;;; agent-shell: a CLI coding agent over ACP, inside Emacs
(use-package agent-shell
  :config
  ;; (setq agent-shell-preferred-agent-config 'pi)
  ;; (setq agent-shell-preferred-agent-config 'omp)
  (setq agent-shell-preferred-agent-config 'kimi)
  ;; GUI Emacs does not inherit the terminal PATH; use an absolute path.
  (setq agent-shell-kimi-acp-command '("/opt/homebrew/bin/kimi" "acp"))
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
  (setq agent-shell-math-renderer-rescale-inline 1.2)
  (setq agent-shell-math-renderer-rescale-display 1.0)
  ;; Allow Chinese in formulas.
  (setq latex-to-svg-backend-appended-preamble
        "\\usepackage[UTF8,fontset=fandol]{ctex}")
  )

(provide 'init-ai)
;;; init-ai.el ends here
