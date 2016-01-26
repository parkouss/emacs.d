(weemacs-require-package 'org)
(weemacs-require-package 'ox-reveal)
(weemacs-require-package 'htmlize) ;; code coloring for org-reveal

(require 'ox-reveal)
(require 'ob-dot)

;; Make windmove work in org-mode:
(add-hook 'org-shiftup-final-hook 'windmove-up)
(add-hook 'org-shiftleft-final-hook 'windmove-left)
(add-hook 'org-shiftdown-final-hook 'windmove-down)
(add-hook 'org-shiftright-final-hook 'windmove-right)

(setq org-default-notes-file (expand-file-name "notes.org" weemacs-dir))

(provide 'weemacs-org)
