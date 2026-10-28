;;; init-org-publish.el --- Org publishing and Hugo  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package easy-hugo
  :bind ("C-c h" . easy-hugo) ;; 原 C-c b 与 open-blog-dir 冲突，改绑 C-c h
  :config
  (setq easy-hugo-basedir "~/research/code/Grant/")
  (setq easy-hugo-postdir "content/org/")
  (setq easy-hugo-url "https://Grant-S-Z.github.io/Grant")
  (setq easy-hugo-sshdomain "grant-s-z.github.io")
  (setq easy-hugo-previewtime "300")
  (setq easy-hugo-default-ext ".md"))

(use-package ox-hugo
  :config
  (setq org-hugo-base-dir "~/research/code/Grant/")
  (setq org-hugo-section "post")
  (setq org-hugo-headline-anchor nil))

(provide 'init-org-publish)
;;; init-org-publish.el ends here
