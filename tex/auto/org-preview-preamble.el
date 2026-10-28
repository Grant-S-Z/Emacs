;; -*- lexical-binding: t; -*-

(TeX-add-style-hook
 "org-preview-preamble"
 (lambda ()
   (TeX-add-to-alist 'LaTeX-provided-class-options
                     '(("article" "")))
   (TeX-add-to-alist 'LaTeX-provided-package-options
                     '(("xcolor" "") ("amsmath" "") ("amsfonts" "") ("unicode-math" "") ("siunitx" "") ("mhchem" "version=4") ("tikz" "") ("tikz-feynman" "")))
   (TeX-run-style-hooks
    "latex2e"
    "article"
    "art10"
    "xcolor"
    "amsmath"
    "amsfonts"
    "unicode-math"
    "siunitx"
    "mhchem"
    "tikz"
    "tikz-feynman")
   (TeX-add-symbols
    '("bm" 1)))
 :latex)

