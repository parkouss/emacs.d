(weemacs-require-package 'rust-mode)
(weemacs-require-package 'racer)
(weemacs-require-package 'company-racer)
(weemacs-require-package 'flycheck-rust)

;; Use flycheck-rust in rust-mode
(add-hook 'flycheck-mode-hook #'flycheck-rust-setup)

;; Setting up configurations when you load rust-mode
(add-hook 'rust-mode-hook

     '(lambda ()
     ;; Enable racer
     (racer-activate)
  
	 ;; Hook in racer with eldoc to provide documentation
     (racer-turn-on-eldoc)
	 
	 ;; Use company-racer in rust mode
     (set (make-local-variable 'company-backends) '(company-racer))
	 
	 ;; Key binding to jump to method definition
     (local-set-key (kbd "M-.") #'racer-find-definition)
	 
	 ;; Key binding to auto complete and indent
     (local-set-key (kbd "TAB") #'racer-complete-or-indent)))

(provide 'weemacs-rust)
