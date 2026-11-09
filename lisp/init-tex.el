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

;;; Doc View PDF presentation (shared by macOS and WSL)
(with-eval-after-load 'doc-view
  (add-hook 'doc-view-mode-hook #'doc-view-fit-width-to-window)

  (defvar-local grant-docview-present--saved nil
    "State saved before entering `grant-docview-present-mode'.")
  (defvar-local grant-docview-present--page nil
    "Page shown when presentation mode last refit.")

  (defvar grant-docview-present-mode-map
    (let ((map (make-sparse-keymap)))
      (set-keymap-parent map doc-view-mode-map)
      (define-key map (kbd "q") #'grant-docview-present-mode)
      (define-key map (kbd "n") #'doc-view-next-page)
      (define-key map (kbd "SPC") #'doc-view-next-page)
      (define-key map (kbd "p") #'doc-view-previous-page)
      (define-key map (kbd "DEL") #'doc-view-previous-page)
      map)
    "Keymap for `grant-docview-present-mode'.")

  (defun grant-docview-present--refit ()
    "Refit the page after a page change while presenting."
    (when (and grant-docview-present-mode
               (derived-mode-p 'doc-view-mode)
               grant-docview-present--page
               (/= (doc-view-current-page) grant-docview-present--page))
      (setq grant-docview-present--page (doc-view-current-page))
      (doc-view-fit-page-to-window)))

  (define-minor-mode grant-docview-present-mode
    "Present the current PDF full-screen with Doc View."
    :lighter " Present"
    :keymap grant-docview-present-mode-map
    (if grant-docview-present-mode
        (progn
          (unless (derived-mode-p 'doc-view-mode)
            (setq grant-docview-present-mode nil)
            (user-error "Only available in Doc View buffers"))
          (setq grant-docview-present--saved
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
          (doc-view-fit-page-to-window)
          (setq grant-docview-present--page (doc-view-current-page))
          (add-hook 'post-command-hook #'grant-docview-present--refit nil t)
          (message "Presentation: n/SPC next, p/DEL previous, q quit"))
      (remove-hook 'post-command-hook #'grant-docview-present--refit t)
      (when-let* ((state grant-docview-present--saved))
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
      (setq grant-docview-present--saved nil
            grant-docview-present--page nil)))

  (define-key doc-view-mode-map (kbd "P") #'grant-docview-present-mode))

(provide 'init-tex)
;;; init-tex.el ends here
