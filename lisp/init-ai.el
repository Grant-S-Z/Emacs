;;; init-ai.el --- LLM clients and AI agents  -*- lexical-binding: t; -*-
;;; Commentary:
;; agent-shell / agent-shell-math-renderer
;;; Code:

;; Agentero is an external CLI called by the ACP agent, not an ACP server.
(let ((bin (expand-file-name "~/.local/bin")))
  (add-to-list 'exec-path bin)
  (unless (member bin (split-string (or (getenv "PATH") "") path-separator t))
    (setenv "PATH" (concat bin path-separator (getenv "PATH")))))

(defun grant/agent-shell-file-directory ()
  "Return the current file's directory, or `default-directory'."
  (or (and buffer-file-name
           (file-name-directory (expand-file-name buffer-file-name)))
      default-directory))

;;; agent-shell: a CLI coding agent over ACP, inside Emacs
(use-package agent-shell
  :bind (("C-c k" . grant/agent-shell-dwim)
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
  ;; xclip exists on macOS but needs X11; drop it so text yanks fall through
  ;; to the normal `yank' instead of starting XQuartz.
  (when *is-mac*
    (setq agent-shell-clipboard-image-handlers
          (seq-filter (lambda (handler)
                        (not (equal (map-elt handler :command) "xclip")))
                      agent-shell-clipboard-image-handlers)))
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
     (list (read-directory-name "agent-shell 工作目录: "
                                (grant/agent-shell-file-directory))))
    (let ((agent-shell-cwd-function (lambda () dir)))
      (agent-shell))))

(defun grant/agent-shell-dwim (&optional arg)
  "Start or reuse an agent shell.
With one prefix argument, force a new shell in the current file's
directory and skip the session prompt, instead of starting from the
project root.  With two prefix arguments, prompt for an existing
shell.  With three, force a new shell in the project root as plain
`agent-shell' would."
  (interactive "P")
  (cond
   ((equal arg '(64))
    (agent-shell '(4)))
   ((equal arg '(16))
    (agent-shell '(16)))
   ((equal arg '(4))
    (require 'agent-shell)
    (let ((agent-shell-cwd-function #'grant/agent-shell-file-directory)
          (agent-shell-session-strategy 'new))
      (agent-shell '(4))))
   (t
    (agent-shell))))

(defun grant/agent-shell-project-name ()
  "Return the shell's cwd relative to its project.
Used for the agent-shell header and buffer name, so a shell started in
a project subdirectory shows that subdirectory instead of the project
root."
  (let* ((dir (directory-file-name (expand-file-name default-directory)))
         (root (ignore-errors
                 (when-let* ((project (project-current nil dir)))
                   (directory-file-name
                    (expand-file-name (project-root project)))))))
    (if (and root (file-in-directory-p dir root))
        (let ((rel (file-relative-name dir root)))
          (if (equal rel ".")
              (file-name-nondirectory root)
            (format "%s/%s" (file-name-nondirectory root) rel)))
      (file-name-nondirectory dir))))

(defun grant/agent-shell-delete-session ()
  "Delete a saved agent-shell session via ACP session/delete.
Run from an agent-shell buffer.  The command lists sessions for the
current shell's working directory and asks for confirmation."
  (interactive)
  (unless (derived-mode-p 'agent-shell-mode)
    (user-error "Run this from an agent-shell buffer"))
  (let* ((state (agent-shell--state))
         (client (map-elt state :client))
         (buffer (current-buffer))
         (current-id (agent-shell-session-id)))
    (unless client
      (user-error "Agent shell is not connected"))
    (unless (map-elt state :supports-session-list)
      (user-error "This agent does not support listing sessions"))
    (agent-shell--list-sessions
     :state state
     :cwd (agent-shell--resolve-path (agent-shell-cwd))
     :buffer buffer
     :on-success
     (lambda (sessions)
       (if (null sessions)
           (message "No saved sessions")
         (let* ((choices
                 (mapcar (lambda (session)
                           (cons (format "%s  %s  %s"
                                         (agent-shell--session-title session)
                                         (agent-shell--session-column-value
                                          'date session)
                                         (map-elt session 'sessionId))
                                 session))
                         sessions))
                (label (completing-read "Delete session: " choices nil t))
                (session (cdr (assoc label choices)))
                (session-id (map-elt session 'sessionId)))
           (unless session-id
             (user-error "Selected session has no ID"))
           (when (yes-or-no-p
                  (format "Delete session %s%s? "
                          session-id
                          (if (equal session-id current-id)
                              " (current session)"
                            "")))
             (agent-shell--send-request
              :state state
              :client client
              :request (acp-make-session-delete-request
                        :session-id session-id)
              :buffer buffer
              :on-success
              (lambda (_response)
                (message "Deleted session %s%s"
                         session-id
                         (if (equal session-id current-id)
                             "; kill or restart this shell"
                           "")))
              :on-failure
              (lambda (err raw)
                (message "Delete failed: %s"
                         (or (map-elt err 'message) raw))))))))
     :on-failure
     (lambda (err raw)
       (message "Could not list sessions: %s"
                (or (map-elt err 'message) raw))))))

(with-eval-after-load 'agent-shell
  (advice-add 'agent-shell--project-name :override
              #'grant/agent-shell-project-name)
  (define-key agent-shell-mode-map (kbd "C-c C-d")
              #'grant/agent-shell-delete-session))

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
