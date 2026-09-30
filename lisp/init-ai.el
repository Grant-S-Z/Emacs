;;; init-ai.el --- LLM clients and AI agents  -*- lexical-binding: t; -*-
;;; Commentary:
;; AI 相关配置：gptel / gptel-agent / agent-shell / agent-shell-math-renderer
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
    ;; 2026-09: deepseek-chat / deepseek-reasoner 别名已于 2026-07-24 退役，
    ;; 当前 API ID 为 deepseek-flash（V4.1 Flash）和 deepseek-v4-pro
    (setq gptel-model 'deepseek-flash
	  gptel-backend (gptel-make-deepseek "DeepSeek"
			  :stream t
			  :models '(deepseek-flash deepseek-v4-pro)
			  :key (lambda () (osx-get-keychain-password "deepseek key")))))

  ;; gptel-agent: agent 模式（文件读写 / Bash / elisp / 联网，子 agent 委派）
  ;; 需先 M-x package-install RET gptel-agent（MELPA）
  ;; 用法：gptel 菜单中应用 gptel-agent 预设，或在 prompt 中加 @gptel-agent
  (use-package gptel-agent
    :after gptel
    :config (gptel-agent-update)))

;;; agent-shell: 通过 ACP 在 Emacs 里使用 CLI coding agent（MELPA）
;; 需先 M-x package-install RET agent-shell
;; M-x agent-shell 启动；默认用 Kimi Code
;; 前置：终端里运行过 kimi 并完成 /login（ACP 复用其登录态）
(use-package agent-shell
  :config
  (setq agent-shell-preferred-agent-config 'kimi)
  ;; GUI 启动的 Emacs 不继承终端 PATH，用绝对路径稳妥
  (setq agent-shell-kimi-acp-command '("/opt/homebrew/bin/kimi" "acp"))
  ;; 恢复历史会话时完整回放全部消息（走 session/load，Kimi ACP 已支持）；
  ;; 默认 'minimal 只显示会话标题、不回放历史。可选 last / first-last / full
  (setq agent-shell-session-restore-verbosity 'last)
  ;; 新建 shell 时询问：恢复历史会话还是开新会话（agent-shell 默认即为 prompt）
  ;; 用法：C-u M-x agent-shell（或 M-x agent-shell-new-shell）后挑选会话
  (setq agent-shell-session-strategy 'prompt))

;;; agent-shell-math-renderer: 把 agent 回复中的 LaTeX 公式渲染成 SVG（MELPA）
;; 依赖本机 latex + dvisvgm（TeX Live / MacTeX 自带）；公式原文仍保留在 buffer 中
;; 注意：行内公式只渲染 \(...\)，不渲染 $...$
(use-package agent-shell-math-renderer
  :after agent-shell
  :hook (agent-shell-mode . agent-shell-math-renderer-mode)
  :config
  ;; 切换主题 / 调整字号时，公式即时重新着色、缩放
  (add-hook 'enable-theme-functions
            #'agent-shell-math-renderer-on-appearance-change)
  (add-hook 'after-setting-font-hook
            #'agent-shell-math-renderer-on-appearance-change)
  (setq agent-shell-math-renderer-rescale-inline 1.2)
  (setq agent-shell-math-renderer-rescale-display 1.2)
  )

(provide 'init-ai)
;;; init-ai.el ends here
