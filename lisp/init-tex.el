;;; init-tex.el --- LaTeX setup  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
;;; TeX settings
(use-package tex
  :ensure auctex
  :config
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq-default TeX-master t) ; make emacs aware of multi-file projects
  (add-hook 'LaTeX-mode-hook (lambda ()
			       (setq TeX-command-default "LatexMk")
			       (setq TeX-show-compilation nil)
			       (turn-on-cdlatex)
			       (turn-on-reftex)
			       (auctex-latexmk-setup)
			       (outline-minor-mode)
			       (outline-hide-body) ; show title only
			       )))

;; PDF preview
(setq TeX-PDF-mode t)
(setq TeX-source-correlate-mode t) ; forward/inverse search after compiling
(setq TeX-source-correlate-method 'syntax)
(setq TeX-source-correlate-start-server t)
;; (setq TeX-view-program-list '(("Skim" "/Applications/Skim.app/Contents/SharedSupport/displayline -b -g %n %o %b")))

;; Appine is the macOS PDF viewer; other platforms keep pdf-tools.
(declare-function appine-open-file "appine" (path))
(defun grant-tex-view-appine ()
  "Open the current AUCTeX output PDF in Appine."
  (appine-open-file (TeX-active-master (TeX-output-extension))))

(setq TeX-view-program-selection
      (if *is-mac* '((output-pdf "Appine")) '((output-pdf "PDF Tools"))))
(with-eval-after-load 'tex
  (when *is-mac*
    (add-to-list 'TeX-view-program-list '("Appine" grant-tex-view-appine))))

;; On macOS, `find-file' on a local PDF opens it in Appine instead of
;; pdf-tools; other files and platforms keep the normal behavior.
(defun grant/pdf-file-p (filename)
  "Return non-nil if FILENAME is a local regular PDF file."
  (let ((case-fold-search t))
    (and (string-match-p "\\.pdf\\'" filename)
         (not (file-remote-p filename))
         (file-regular-p filename))))

(defun grant/find-file (orig filename &optional wildcards)
  "Open local PDFs in Appine on macOS, otherwise call ORIG."
  (if (and *is-mac* (not wildcards) (grant/pdf-file-p filename))
      (progn
        (appine-open-file filename)
        (current-buffer))
    (funcall orig filename wildcards)))

(when *is-mac*
  (dolist (command '(find-file find-file-other-window))
    (advice-add command :around #'grant/find-file)))
(add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer) ; refresh pdf after compiling

(use-package auctex-latexmk
  :after tex
  :config
  (setq auctex-latexmk-inherit-TeX-PDF-mode t))

(use-package cdlatex
  :hook ((org-mode . org-cdlatex-mode)
	 (tex-mode . cdlatex-mode))
  :config
  (add-to-list 'cdlatex-command-alist
	       '(("qt" "Insert \\qty{}{}" "\\qty{?}{}" cdlatex-position-cursor nil t nil))))

;; pdf-tools is only used on WSL; macOS opens PDFs in Appine.
(when *is-wsl*
  (add-hook 'pdf-view-mode-hook #'pdf-view-fit-width-to-window)
  (add-hook 'pdf-view-mode-hook #'pdf-view-roll-minor-mode) ; continuous scroll
  (use-package pdf-tools
    :mode ("\\.pdf\\'" . pdf-view-mode)
    :config (pdf-tools-install)))

;; PDF presentation mode on top of pdf-tools (WSL only): full screen, page
;; fit, no clutter.  In a PDF buffer press P or M-x grant-pdf-present-mode;
;; q quits, m inverts.
(defvar-local grant-pdf-present--saved nil
  "State saved before entering `grant-pdf-present-mode'.")

(defvar grant-pdf-present-mode nil
  "Non-nil when `grant-pdf-present-mode' is active (forward declaration).")

(defun grant-pdf-present--refit ()
  "Refit current page to window after page changes."
  (when (and (derived-mode-p 'pdf-view-mode) grant-pdf-present-mode)
    (pdf-view-fit-page-to-window)))

(define-minor-mode grant-pdf-present-mode
  "Present the current PDF full-screen via pdf-tools."
  :lighter " Present"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "q") #'grant-pdf-present-mode)
            (define-key map (kbd "m") #'pdf-view-midnight-minor-mode)
            map)
  (if grant-pdf-present-mode
      (progn
        (unless (derived-mode-p 'pdf-view-mode)
          (setq grant-pdf-present-mode nil)
          (user-error "grant-pdf-present-mode 只适用于 pdf-view 缓冲区"))
        (setq grant-pdf-present--saved
              (list :fullscreen (frame-parameter nil 'fullscreen)
                    :mode-line mode-line-format
                    :cursor cursor-type
                    :tab-bar (bound-and-true-p tab-bar-mode)))
        (unless (frame-parameter nil 'fullscreen)
          (toggle-frame-fullscreen))
        (when (bound-and-true-p tab-bar-mode)
          (tab-bar-mode -1))
        (setq-local mode-line-format nil
                    cursor-type nil)
        (display-line-numbers-mode -1)
        (pdf-view-fit-page-to-window)
        (add-hook 'pdf-view-after-change-page-hook
                  #'grant-pdf-present--refit nil t)
        (message "演示模式：n/p/SPC/DEL/方向键翻页，m 反色，q 退出"))
    (remove-hook 'pdf-view-after-change-page-hook
                 #'grant-pdf-present--refit t)
    (when-let* ((state grant-pdf-present--saved))
      (unless (eq (frame-parameter nil 'fullscreen)
                  (plist-get state :fullscreen))
        (toggle-frame-fullscreen))
      (when (plist-get state :tab-bar)
        (tab-bar-mode 1))
      (setq-local mode-line-format (plist-get state :mode-line)
                  cursor-type (plist-get state :cursor)))
    (setq grant-pdf-present--saved nil)))

(when *is-wsl*
  (with-eval-after-load 'pdf-view
    (define-key pdf-view-mode-map (kbd "P") #'grant-pdf-present-mode)))

(provide 'init-tex)
;;; init-tex.el ends here
