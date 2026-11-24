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

;; AUCTeX View uses pdf-tools on both macOS and WSL.
(setq TeX-view-program-selection '((output-pdf "PDF Tools")))
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

;;; PDF Tools (default PDF viewer on both macOS and WSL)
(setq pdf-view-continuous t)
(add-hook 'pdf-view-mode-hook #'pdf-view-fit-width-to-window)
(add-hook 'pdf-view-mode-hook #'pdf-view-roll-minor-mode) ; continuous scroll

(use-package pdf-tools
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :config (pdf-tools-install))

;; pdf-view-roll-minor-mode remaps the standard scroll commands to one-line
;; steps.  Use multi-line, screenful, and multi-column steps instead.
(defvar grant/pdf-scroll-lines 3
  "Lines to scroll per pdf-view line command.")
(defvar grant/pdf-hscroll-columns 5
  "Columns to scroll per pdf-view horizontal command.")

(defun grant/pdf-scroll-down (&optional arg)
  "Scroll the PDF down ARG times `grant/pdf-scroll-lines'."
  (interactive "p")
  (let ((n (* (or arg 1) grant/pdf-scroll-lines)))
    (if (bound-and-true-p pdf-view-roll-minor-mode)
        (pdf-roll-scroll-forward n)
      (pdf-view-next-line-or-next-page n))))

(defun grant/pdf-scroll-up (&optional arg)
  "Scroll the PDF up ARG times `grant/pdf-scroll-lines'."
  (interactive "p")
  (let ((n (* (or arg 1) grant/pdf-scroll-lines)))
    (if (bound-and-true-p pdf-view-roll-minor-mode)
        (pdf-roll-scroll-backward n)
      (pdf-view-previous-line-or-previous-page n))))

(defun grant/pdf-scroll-screen-down (&optional arg)
  "Scroll the PDF down ARG screens."
  (interactive "p")
  (if (bound-and-true-p pdf-view-roll-minor-mode)
      (pdf-roll-scroll-screen-forward (or arg 1))
    (pdf-view-scroll-up-or-next-page)))

(defun grant/pdf-scroll-screen-up (&optional arg)
  "Scroll the PDF up ARG screens."
  (interactive "p")
  (if (bound-and-true-p pdf-view-roll-minor-mode)
      (pdf-roll-scroll-screen-backward (or arg 1))
    (pdf-view-scroll-down-or-previous-page)))

(defun grant/pdf-hscroll-left (&optional arg)
  "Scroll the PDF left ARG times `grant/pdf-hscroll-columns'."
  (interactive "p")
  (image-forward-hscroll (- (* (or arg 1) grant/pdf-hscroll-columns))))

(defun grant/pdf-hscroll-right (&optional arg)
  "Scroll the PDF right ARG times `grant/pdf-hscroll-columns'."
  (interactive "p")
  (image-forward-hscroll (* (or arg 1) grant/pdf-hscroll-columns)))

(with-eval-after-load 'pdf-view
  (define-key pdf-view-mode-map (kbd "SPC") #'grant/pdf-scroll-screen-down)
  (define-key pdf-view-mode-map (kbd "S-SPC") #'grant/pdf-scroll-screen-up)
  (define-key pdf-view-mode-map (kbd "DEL") #'grant/pdf-scroll-screen-up)
  (define-key pdf-view-mode-map (kbd "C-n") #'grant/pdf-scroll-down)
  (define-key pdf-view-mode-map (kbd "C-p") #'grant/pdf-scroll-up)
  (define-key pdf-view-mode-map (kbd "<down>") #'grant/pdf-scroll-down)
  (define-key pdf-view-mode-map (kbd "<up>") #'grant/pdf-scroll-up)
  (define-key pdf-view-mode-map (kbd "C-b") #'grant/pdf-hscroll-left)
  (define-key pdf-view-mode-map (kbd "C-f") #'grant/pdf-hscroll-right)
  (define-key pdf-view-mode-map (kbd "<left>") #'grant/pdf-hscroll-left)
  (define-key pdf-view-mode-map (kbd "<right>") #'grant/pdf-hscroll-right))

;; PDF presentation mode on top of pdf-tools: full screen, page fit, no
;; clutter.  In a PDF buffer press P or M-x grant-pdf-present-mode; q quits,
;; m inverts.
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
          (user-error "Only available in pdf-view buffers"))
        (setq grant-pdf-present--saved
              (list :fullscreen (frame-parameter nil 'fullscreen)
                    :mode-line mode-line-format
                    :header-line header-line-format
                    :cursor cursor-type
                    :tab-bar (bound-and-true-p tab-bar-mode)
                    :line-numbers (bound-and-true-p display-line-numbers-mode)))
        (unless (frame-parameter nil 'fullscreen)
          (toggle-frame-fullscreen))
        (when (bound-and-true-p tab-bar-mode)
          (tab-bar-mode -1))
        (setq-local mode-line-format nil
                    header-line-format nil
                    cursor-type nil)
        (display-line-numbers-mode -1)
        (pdf-view-fit-page-to-window)
        (add-hook 'pdf-view-after-change-page-hook
                  #'grant-pdf-present--refit nil t)
        (message "Presentation: n/p next/previous, m invert, q quit"))
    (remove-hook 'pdf-view-after-change-page-hook
                 #'grant-pdf-present--refit t)
    (when-let* ((state grant-pdf-present--saved))
      (unless (eq (frame-parameter nil 'fullscreen)
                  (plist-get state :fullscreen))
        (toggle-frame-fullscreen))
      (when (plist-get state :tab-bar)
        (tab-bar-mode 1))
      (setq-local mode-line-format (plist-get state :mode-line)
                  header-line-format (plist-get state :header-line)
                  cursor-type (plist-get state :cursor))
      (when (plist-get state :line-numbers)
        (display-line-numbers-mode 1)))
    (setq grant-pdf-present--saved nil)))

(with-eval-after-load 'pdf-view
  (define-key pdf-view-mode-map (kbd "C-s") #'isearch-forward)
  (define-key pdf-view-mode-map (kbd "P") #'grant-pdf-present-mode))

;; Single-key annotation commands in PDF buffers.  Displaced commands keep
;; working on their upper-case counterparts.
(with-eval-after-load 'pdf-annot
  (let ((slice-map (lookup-key pdf-view-mode-map (kbd "s"))))
    (when (keymapp slice-map)
      (define-key pdf-view-mode-map (kbd "S") slice-map)))
  (let ((register (lookup-key pdf-view-mode-map (kbd "m"))))
    (define-key pdf-view-mode-map (kbd "M") register))
  (define-key pdf-view-mode-map (kbd "h")
    #'pdf-annot-add-highlight-markup-annotation)
  (define-key pdf-view-mode-map (kbd "s")
    #'pdf-annot-add-squiggly-markup-annotation)
  (define-key pdf-view-mode-map (kbd "u")
    #'pdf-annot-add-underline-markup-annotation)
  (define-key pdf-view-mode-map (kbd "o")
    #'pdf-annot-add-strikeout-markup-annotation)
  (define-key pdf-view-mode-map (kbd "t")
    #'pdf-annot-add-text-annotation)
  (define-key pdf-view-mode-map (kbd "m")
    #'pdf-annot-add-markup-annotation)
  (define-key pdf-view-mode-map (kbd "d") #'pdf-annot-delete)
  (define-key pdf-view-mode-map (kbd "e") #'pdf-annot-edit)
  (define-key pdf-view-mode-map (kbd "l") #'pdf-annot-list-annotations))

(with-eval-after-load 'pdf-outline
  (define-key pdf-outline-minor-mode-map (kbd "O") #'pdf-outline)
  (define-key pdf-outline-minor-mode-map (kbd "o")
    #'pdf-annot-add-strikeout-markup-annotation))

(with-eval-after-load 'pdf-history
  (define-key pdf-history-minor-mode-map (kbd "l")
    #'pdf-annot-list-annotations))

(provide 'init-tex)
;;; init-tex.el ends here
