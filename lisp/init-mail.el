;;; Package -- summary  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
;;; Basic settings
;; Send mails
(require 'auth-source)
(setq auth-sources '("~/.authinfo"))

(setq message-send-mail-function 'smtpmail-send-it)
(setq send-mail-function 'smtpmail-send-it)

(setq smtpmail-smtp-service 465
      smtpmail-stream-type 'ssl)

(setq user-mail-address "zhu-yt24@mails.tsinghua.edu.cn")
(setq user-full-name "Yutao Zhu")
(setq smtpmail-smtp-user "zhu-yt24@mails.tsinghua.edu.cn"
      smtpmail-smtp-server "mails.tsinghua.edu.cn")

;;Debug
(setq smtpmail-debug-info nil)
(setq smtpmail-debug-verb nil)

;;; Mu4e
(use-package mu4e
  :ensure nil
  :commands (mu4e)
  :custom
  (mu4e-maildir "~/thumail")
  (mu4e-get-mail-command "mbsync -a")
  (mu4e-update-interval 300)

  (mu4e-sent-folder "/Sent Items")
  (mu4e-drafts-folder "/Drafts")
  (mu4e-trash-folder "/Trash")

  (mu4e-view-show-images t)
  (mu4e-html2text-command "w3m -T text/html")

  (mu4e-modeline-mode t)

  (mu4e-attachment-dir "~/Downloads/")

  (mail-user-agent 'mu4e-user-agent)

  :config
  (add-hook 'mu4e-compose-mode-hook
          (lambda ()
            (setq-local message-signature
			(format "Best regards,\n\nYutao Zhu")
			)))
  )

(provide 'init-mail)
;;; init-mail.el ends here
