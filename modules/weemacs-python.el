
;; python specific code
(weemacs-require-package 'anaconda-mode)
(weemacs-require-package 'company-anaconda)

;; first Add company-anaconda to allowed company-mode backends list
(eval-after-load "company"
 '(progn
   (add-to-list 'company-backends 'company-anaconda)))

(add-hook 'python-mode-hook 'anaconda-mode)
;;(add-hook 'python-mode-hook 'eldoc-mode) ;; provide ElDoc

(provide 'weemacs-python)
