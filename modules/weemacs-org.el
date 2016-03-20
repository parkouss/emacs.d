(weemacs-require-package 'org)
(weemacs-require-package 'ox-reveal)
(weemacs-require-package 'htmlize) ;; code coloring for org-reveal

;; Make windmove work in org-mode (do this before loading org):
(setq org-replace-disputed-keys t)

(require 'ox-reveal)
(require 'ob-dot)

(setq org-default-notes-file (expand-file-name "notes.org" weemacs-dir))

(setq org-agenda-files (list (expand-file-name "agenda" weemacs-dir)))

(setq org-tag-alist '((:startgroup . nil)
                      ("@work" . ?w) ("@home" . ?h)
                      (:endgroup . nil)))

(add-hook 'org-mode-hook
          (lambda ()
            (turn-on-auto-fill)))
(provide 'weemacs-org)
