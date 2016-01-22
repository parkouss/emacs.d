(weemacs-require-package 'pdf-tools)

(pdf-tools-install)

(define-key pdf-view-mode-map (kbd "M-v") 'pdf-view-scroll-down-or-previous-page)
(define-key pdf-view-mode-map (kbd "C-v") 'pdf-view-scroll-up-or-next-page)

(provide 'weemacs-pdf)
