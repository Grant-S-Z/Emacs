;;; init-ui.el --- theme, fonts and UI chrome  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;;; Reading and scrolling
(pixel-scroll-precision-mode 1)
(setq fill-column 68)
(add-hook 'text-mode-hook #'visual-line-mode)

;;; Line number
(defun grant/enable-line-numbers ()
  "Enable line numbers except in specific modes."
  (unless (or (derived-mode-p 'org-mode)
              (derived-mode-p 'latex-mode)
              (derived-mode-p 'doc-view-mode))
    (display-line-numbers-mode 1)))
(add-hook 'prog-mode-hook 'grant/enable-line-numbers)

;;; Dashboard
;; Recent files and a banner on the startup screen.
(use-package dashboard
  :init
  (add-hook 'after-init-hook 'dashboard-open)
  :config
  (setq initial-buffer-choice (lambda () (get-buffer-create "*dashboard*")))

  (setq dashboard-items '((recents . 8)))
  (setq dashboard-item-shortcuts '((recents . "r")
				   ))

  (setq dashboard-display-icons-p t)
  (setq dashboard-icon-type 'nerd-icons)
  :custom
  (dashboard-banner-logo-title "Welcome Grant. Have a good time!")
  (dashboard-center-content t)
  (dashboard-startup-banner "~/.emacs.d/img/celeste.png")
  (dashboard-footer-messages '
   ("True mastery of any skill takes a lifetime."))
  (dashboard-set-heading-icons t)
  (dashboard-set-file-icons t))

;;; Modeline
;; Doom-style mode line.
(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :config
  (setq doom-modeline-height 20)
  (display-time)
  (setq doom-modeline-time t)
  (setq doom-modeline-icon t)
  (setq doom-modeline-github nil)
  (setq doom-modeline-battery nil)
  (setq doom-modeline-buffer-file-name-style 'buffer-name)
  (setq doom-modeline-enable-word-count nil)
  (setq doom-modeline-env-python-executable "~/miniconda3/bin/python3"))

;;; Subtle fun and orientation
;; Nyan Cat progress indicator in the mode line.
(use-package nyan-mode
  :init (nyan-mode 1)
  :config
  (setq nyan-animate-nyancat t
        nyan-animation-frame-interval 0.3
        nyan-bar-length 20))

;;; Minibuffer
;; Vertical completion UI in the minibuffer.
(use-package vertico
  :init (vertico-mode)
  :custom
  (vertico-count 15))

;; Persist minibuffer history across restarts.
(use-package savehist
  :init (savehist-mode 1))

;; Space-separated, order-independent completion matching.
(use-package orderless
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles basic partial-completion))))
  )

;; Show docs and metadata beside completion candidates.
(use-package marginalia
  :init (marginalia-mode 1))

;;; Buffer management
;; Manage buffers in a tabulated list.
(use-package ibuffer
  :ensure nil
  :bind ("C-x C-b" . ibuffer)
  :hook (ibuffer-mode . ibuffer-auto-mode)
  :config
  (setq ibuffer-show-empty-filter-groups nil))

;; Clean up old buffers periodically.
(use-package midnight
  :ensure nil
  :init (midnight-mode 1))

;;; Useful highlights and colors
;; Highlight matching parentheses.
(use-package paren
  :config
  (setq show-paren-when-point-inside-paren t
        show-paren-when-point-in-periphery t
        show-paren-context-when-offscreen t
        show-paren-delay 0.1)
  )

;; Color nested delimiters by depth.
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; Highlight the current line.
(use-package hl-line
  :hook (after-init . global-hl-line-mode)
  :config
  (setq hl-line-sticky-flag nil)
  ;; Highlight starts from EOL, to avoid conflicts with other overlays
  (setq hl-line-range-function (lambda () (cons (line-end-position)
						(line-beginning-position 2))))
  )

;; Draw indentation guides with characters.
(use-package indent-bars
  :init
  (setq indent-bars-no-descend-lists t
	indent-bars-prefer-character t)
  :hook (prog-mode . indent-bars-mode))

;; Outli: org-style folding in code buffers.
(use-package outli
  :vc (:url "https://github.com/jdtsmith/outli")
  :hook (prog-mode . outli-mode))

;; Highlight TODO, FIXME, and similar keywords.
(use-package hl-todo
  :init (global-hl-todo-mode))

;;; Spell checking
;; Spell checking through Jinx/enchant; skip non-ASCII words because the
;; dictionary is English-only.
(use-package jinx
  :hook ((org-mode . jinx-mode)
         (LaTeX-mode . jinx-mode)
         (message-mode . jinx-mode))
  :bind (:map jinx-mode-map
              ("M-$" . jinx-correct))
  :custom
  (jinx-languages "en_US")
  :config
  (let ((cell (assq t jinx-exclude-regexps)))
    (when cell
      (setcdr cell (cons "[^[:ascii:]]" (cdr cell))))))

;;; Fonts and input method
;; Use one font family for Latin, CJK, code and proportional text.
(defun grant-setup-fonts (&optional frame)
  "Use Maple Mono NF CN for all Latin and CJK text."
  (when (display-graphic-p frame)
    (set-face-attribute 'default frame :family "Maple Mono NF CN" :height 170)
    (set-face-attribute 'fixed-pitch frame :family "Maple Mono NF CN")
    (set-face-attribute 'variable-pitch frame :family "Maple Mono NF CN")
    (dolist (charset '(kana han cjk-misc bopomofo hangul))
      (set-fontset-font "fontset-default" charset
                        (font-spec :family "Maple Mono NF CN" :size 17.0)))))

(grant-setup-fonts)
(add-hook 'after-make-frame-functions #'grant-setup-fonts)

(global-set-key (kbd "C--") #'text-scale-decrease)
(global-set-key (kbd "C-=") #'text-scale-increase)

;; Rime is disabled; the system input method is used (see archive/).

;; Switch macOS input sources around minibuffer/prefix use (needs macism).
(use-package sis
  :if *is-mac*
  :hook (after-init . sis-global-respect-mode)
  :config
  (sis-ism-lazyman-config
   "com.apple.keylayout.ABC"
   "com.tencent.inputmethod.wetype.pinyin")
  ;; Do not change the input source just because Emacs starts.
  (setq sis-respect-start nil))

;;; Themes
(load-theme 'ef-reverie :no-confirm)

;;; Dired
(when *is-mac*
  (setq dired-use-ls-dired t
        insert-directory-program "/opt/homebrew/bin/gls" ; GNU ls for --group-directories-first
        dired-listing-switches "-aBhl --group-directories-first"))

;; Drag files out of Dired/Dirvish and drop them into other applications.
(setq dired-mouse-drag-files t
      mouse-drag-and-drop-region-cross-program t)

;;; Dirvish: file manager in Dired, plus a side panel
(use-package dirvish
  :bind (("C-x d" . dirvish)
         ("C-c l" . dirvish-side)
         :map dirvish-mode-map
         ("?" . dirvish-dispatch)       ; cheatsheet
         ("C-c o" . dirvish-quick-access) ; `o' keeps find-file-other-window
         ("s" . dirvish-quicksort)      ; superset of dired-sort-toggle
         ("r" . dirvish-history-jump)   ; unbound in dired
         ("y" . dirvish-yank-menu)
         ("N" . dirvish-narrow)
         ("TAB" . dirvish-subtree-toggle)
         ("M-f" . dirvish-history-go-forward)
         ("M-b" . dirvish-history-go-backward)
         ("M-e" . dirvish-emerge-menu))
  :custom
  (dirvish-attributes '(subtree-state nerd-icons collapse git-msg file-size))
  ;; The side panel is narrow: no git-msg there, or the file name vanishes.
  (dirvish-side-attributes '(vc-state nerd-icons collapse file-size))
  (dirvish-subtree-state-style 'nerd)
  (dirvish-side-width 32)
  ;; List huge directories asynchronously with fd instead of blocking.
  (dirvish-large-directory-threshold 5000)
  ;; Keep preview/thumbnail caches out of the config directory.
  (dirvish-cache-dir (expand-file-name "dirvish/"
                                       (expand-file-name ".cache" user-emacs-directory)))
  ;; No epub preview (`epub-thumbnailer' is missing); .epub opens in nov.
  (dirvish-preview-dispatchers '(video image gif audio archive font pdf))
  (dirvish-quick-access-entries
   '(("h" "~/" "Home")
     ("d" "~/Downloads" "Downloads")
     ("o" "~/org/" "Org")
     ("r" "~/research/" "Research")))
  :config
  (dirvish-override-dired-mode)  ; the Dired UI and previews
  (dirvish-side-follow-mode)
)

;;; Ligature
;; Maple Mono NF CN's `calt' ligature table.  ssXX/cvXX forms (>>=, -<<, ~=,
;; =~, !~ ...) need OpenType features Emacs cannot enable, so they are absent.
(defconst grant-ligatures
  '("!==" "!!" "!="
    "#######" "#_(" "#__" "#!" "#(" "#:" "#=" "#?" "#[" "#_" "#{"
    "&&&" "&&"
    "*/" "*>"
    "+++" "++" "+>"
    "-------" "--}}" "---" "--<" "-->" "--" "->" "-~"
    "..." "..<" ".." ".=" ".?"
    "/**" "///" "/*" "//" "/>"
    ":::" ":<:" ":=:" ":>:" ":?>" "::" ":<" ":=" ":>" ":?"
    ";;;" ";;"
    "<!---->" "<!--" "<#--" "<-->" "<==>" "<|||" "<*>" "<+>" "<--" "<-<"
    "<->" "<-|" "</>" "<:<" "<<<" "<=<" "<==" "<=>" "<=|" "<|>" "<||"
    "<~>" "<~~" "<*" "<+" "<-" "</" "<:" "<<" "<=" "<>" "<|" "<~"
    "=======" "=!=" "=/=" "=:=" "=<=" "===" "==>" "=>=" "=:" "==" "=>"
    ">--" ">->" ">:>" ">=<" ">=>" ">>>" ">:" ">=" ">>"
    "???" "?." "?:" "??"
    "]#" "_|_" "__"
    "{{!--" "{{--" "{{" "{|" "|}" "}}"
    "|||>" "|->" "|=>" "||>" "|=" "|>" "|]" "||"
    "~~~~~~~" "~~>" "~-" "~>" "~@" "~~")
  "Fixed-length ligatures of Maple Mono NF CN, grouped by first character.")

;; Match - = # ~ and arrows at any length with a regexp rather than
;; enumerating; the regexp starts after the first char (the rule matches `.`).
(defconst grant-ligature-regexps
  '((?- . "-+\\(?:[<>]\\|}}\\|~\\)?")
    (?= . "=+\\(?:[<>]=\\|[!/:]=\\|[<>:]\\)?")
    (?# . "#+\\(?:_[(_]\\|[_(:?!=]\\|\\[\\|{\\)?")
    (?~ . "~+\\(?:[>@-]\\)?")
    (?< . "-+[<>|]?\\|=+[<>|]?\\||+>?\\|<+\\|/>?\\|!-+>?\\|#-+\\|:<?\\|~+>?\\|[+*]>?\\|>")
    (?> . ">+\\(?:[-=:][-=<>]?\\)?\\|-+[<>]?\\|[-=:][-=<>]?"))
  "Regexps for the leading character, covering ligatures of any length.
They are tried before the fixed-length strings that share a first character.")

(let ((groups nil))
  (dolist (ligature grant-ligatures)
    (let ((group (assq (aref ligature 0) groups)))
      (if group
          (setcdr group (cons (substring ligature 1) (cdr group)))
        (push (cons (aref ligature 0) (list (substring ligature 1))) groups))))
  (dolist (group groups)
    ;; Regexp alternation takes the first match, so longer strings go first.
    (let* ((char (car group))
           (regexp (cdr (assq char grant-ligature-regexps)))
           (rests (sort (cdr group)
                        (lambda (a b)
                          (or (> (length a) (length b))
                              (and (= (length a) (length b)) (string< a b))))))
           (alternatives (mapcar #'regexp-quote rests)))
      (when regexp
        (setq alternatives (cons regexp alternatives)))
      (set-char-table-range composition-function-table char
                            `([,(concat ".\\(?:" (mapconcat #'identity alternatives "\\|") "\\)")
                               0 font-shape-gstring])))))

;;; Frame borders and window dividers
(modify-all-frames-parameters
 '((right-divider-width . 1)
   (internal-border-width . 1)))
(dolist (face '(window-divider
                window-divider-first-pixel
                window-divider-last-pixel))
  (face-spec-reset-face face)
  (set-face-foreground face (face-attribute 'default :background)))
(set-face-background 'fringe (face-attribute 'default :background))

(provide 'init-ui)
;;; init-ui.el ends here
