
;; python specific code
(weemacs-require-package 'anaconda-mode)
(weemacs-require-package 'company-anaconda)
(weemacs-require-package 'pyvenv)
(weemacs-require-package 'pytest)

(require 'pytest)
(add-hook 'python-mode-hook
          (lambda ()
            (local-set-key "\C-ca" 'pytest-all)
            (local-set-key "\C-cm" 'pytest-module)
            (local-set-key "\C-c." 'pytest-one)
            (local-set-key "\C-cd" 'pytest-directory)
            (local-set-key "\C-cpa" 'pytest-pdb-all)
            (local-set-key "\C-cpm" 'pytest-pdb-module)
            (local-set-key "\C-cp." 'pytest-pdb-one)))

;; first Add company-anaconda to allowed company-mode backends list
(eval-after-load "company"
 '(progn
   (add-to-list 'company-backends 'company-anaconda)))

(add-hook 'python-mode-hook 'anaconda-mode)
;;(add-hook 'python-mode-hook 'eldoc-mode) ;; provide ElDoc

(provide 'weemacs-python)
