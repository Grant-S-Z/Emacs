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

;; (setq user-mail-address "2206627826@qq.com")
;; (setq user-full-name "Yutao Zhu")
;; (setq smtpmail-smtp-user "2206627826@qq.com"
;;       smtpmail-smtp-server "smtp.qq.com")

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
  ;; :load-path "/opt/homebrew/share/emacs/site-lisp/mu/mu4e/"
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

  :config
  (add-hook 'mu4e-compose-mode-hook
          (lambda ()
            (setq-local message-signature
                        ;; (format "Best regards,\n\nYutao Zhu\n%s" (format-time-string "%Y-%m-%d"))
			(format "Best regards,\n\nYutao Zhu")
			)))
  )


;;; Gnus
;; (gnus-delay-initialize)
;; (setq gnus-asynchronous t)

;; ;; (setq gnus-select-method
;; ;;       '(nnimap "qq.com"
;; ;;                (nnimap-address "imap.qq.com")
;; ;;                (nnimap-inbox "INBOX")
;; ;;                (nnimap-expunge t)
;; ;;                (nnimap-server-port 993)
;; ;;                (nnimap-stream ssl)))

;; (setq gnus-select-method
;;       '(nnimap "thu"
;;                (nnimap-address "mails.tsinghua.edu.cn")
;;                (nnimap-inbox "INBOX")
;;                (nnimap-expunge t)
;;                (nnimap-server-port 993)
;;                (nnimap-stream ssl)))

;; ;; (setq gnus-ignored-newsgroups "^to\\.\\|^[0-9. ]+\\( \\|$\\)\\|^[\"]\"[#'()]")

;; (setq gnus-use-full-window nil)
;; ;; (setq gnus-message-archive-group nil)

;; ;; color
;; (cond (window-system
;;        (setq custom-background-mode 'light)
;;        (defface my-group-face-1
;;          '((t (:foreground "Red" :bold t))) "First group face")
;;        (defface my-group-face-2
;;          '((t (:foreground "DarkSeaGreen4" :bold t)))
;;          "Second group face")
;;        (defface my-group-face-3
;;          '((t (:foreground "Green4" :bold t))) "Third group face")
;;        (defface my-group-face-4
;;          '((t (:foreground "SteelBlue" :bold t))) "Fourth group face")
;;        (defface my-group-face-5
;;          '((t (:foreground "Blue" :bold t))) "Fifth group face")))

;; (setq gnus-group-highlight
;;       '(((> unread 200) . my-group-face-1)
;;         ((and (< level 3) (zerop unread)) . my-group-face-2)
;;         ((< level 3) . my-group-face-3)
;;         ((zerop unread) . my-group-face-4)
;;         (t . my-group-face-5)))

;; ;; block
;; (setq gnus-blocked-images "ads")

;; ;; timestamp
;; (add-hook 'gnus-select-group-hook 'gnus-group-set-timestamp)

;; ;; delete / expiry
;; ;; Your IMAP server exposes these folders (from Group buffer):
;; ;;   Deleted Messages, Drafts, INBOX, Junk E-mail, Sent Items, Sent Messages, Trash, Virus Items
;; ;; Use "Trash" as the delete target.
;; (setq nnmail-expiry-wait 'never)
;; (setq nnmail-expiry-target "Trash")

(provide 'init-mail)
;;; init-mail.el ends here
