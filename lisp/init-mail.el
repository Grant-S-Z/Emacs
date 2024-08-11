;;; Package -- summary
;;; Commentary:
;;; Code:

;; Send mails
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

;;; Gnus
(gnus-delay-initialize)
(setq gnus-asynchronous t)

(setq gnus-select-method
      '(nnimap "qq.com"
               (nnimap-address "imap.qq.com")
               (nnimap-inbox "INBOX")
               (nnimap-expunge t)
               (nnimap-server-port 993)
               (nnimap-stream ssl)))

(setq send-mail-function 'smtpmail-send-it
      smtpmail-smtp-server "smtp.qq.com"
      smtpmail-smtp-service 465
      smtpmail-stream-type 'ssl
      gnus-ignored-newsgroups "^to\\.\\|^[0-9. ]+\\( \\|$\\)\\|^[\"]\"[#'()]")

(setq gnus-use-full-window nil)
(setq gnus-message-archive-group nil)

;; color
(cond (window-system
       (setq custom-background-mode 'light)
       (defface my-group-face-1
         '((t (:foreground "Red" :bold t))) "First group face")
       (defface my-group-face-2
         '((t (:foreground "DarkSeaGreen4" :bold t)))
         "Second group face")
       (defface my-group-face-3
         '((t (:foreground "Green4" :bold t))) "Third group face")
       (defface my-group-face-4
         '((t (:foreground "SteelBlue" :bold t))) "Fourth group face")
       (defface my-group-face-5
         '((t (:foreground "Blue" :bold t))) "Fifth group face")))

(setq gnus-group-highlight
      '(((> unread 200) . my-group-face-1)
        ((and (< level 3) (zerop unread)) . my-group-face-2)
        ((< level 3) . my-group-face-3)
        ((zerop unread) . my-group-face-4)
        (t . my-group-face-5)))

;; block
(setq gnus-blocked-images "ads")

;; timestamp
(add-hook 'gnus-select-group-hook 'gnus-group-set-timestamp)

;; delete
(setq nnmail-expiry-wait 'never)
(setq nnmail-expiry-target "Deleted Messages")

(provide 'init-mail)
;;; init-mail.el ends here
