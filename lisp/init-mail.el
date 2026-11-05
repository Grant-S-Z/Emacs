;;; init-mail.el --- mail setup  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
;;; Basic settings
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

;;; Debug
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

  (mu4e-attachment-dir "~/Downloads/")
  (mail-user-agent 'mu4e-user-agent)
  (mu4e-search-threads t)

  ;; Compact, fixed-width headers with fancy flag characters.
  (mu4e-headers-fields
   '((:human-date . 11)
     (:flags . 5)
     (:from-or-to . 24)
     (:subject)))
  (mu4e-headers-date-format "%Y-%m-%d")
  (mu4e-headers-time-format "%H:%M")
  (mu4e-use-fancy-chars t)
  (mu4e-headers-visible-lines 15)
  (mu4e-search-results-limit 2000)

  ;; mbsync renames on move, so let mu4e follow suit.
  (mu4e-change-filenames-when-moving t)
  (mu4e-confirm-quit nil)
  (mu4e-hide-index-messages t)

  (mu4e-maildir-shortcuts
   '(("/INBOX" . ?i)
     ("/Sent Items" . ?s)
     ("/Drafts" . ?d)
     ("/Trash" . ?t)))
  (mu4e-view-fields '(:from :to :subject :date :tags))

  :config
  (mu4e-modeline-mode 1)
  (add-to-list 'mu4e-bookmarks
               '(:name "Flagged" :query "flag:flagged" :key ?f) t)
  (with-eval-after-load 'org
    (require 'mu4e-org))

  ;; Thread folding in the headers list: TAB toggles the current thread.
  (setq mu4e-thread-fold-unread nil
        mu4e-thread-fold-single-children t)
  (add-hook 'mu4e-thread-mode-hook #'mu4e-thread-fold-all)

  ;; Hide quoted history while reading, newest reply first.
  (add-hook 'mu4e-view-rendered-hook #'gnus-article-hide-citation)

  (add-hook 'mu4e-compose-mode-hook
            (lambda ()
              (setq-local message-signature
                          (format "Best regards,\n\nYutao Zhu")))))

(provide 'init-mail)
;;; init-mail.el ends here
