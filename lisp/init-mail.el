;;; Package -- summary
;;; Commentary:
;;; Code:

;; Sending email
(require 'auth-source)
(setq auth-source '("~/.authinfo" "~/.authinfo.gpg"))

(setq message-send-mail-function 'smtpmail-send-it)
(setq user-mail-address "2206627826@qq.com")
(setq user-full-name "Yutao Zhu")

(setq smtpmail-smtp-user "2206627826@qq.com"
      smtpmail-smtp-server "smtp.qq.com"
      smtpmail-smtp-service 465
      smtpmail-stream-type 'ssl)

;;Debug
(setq smtpmail-debug-info t)
(setq smtpmail-debug-verb t)

(provide 'init-mail)
;;; init-mail.el ends here
