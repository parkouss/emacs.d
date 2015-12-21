(weemacs-require-package 'rust-mode)
(weemacs-require-package 'racer)
(weemacs-require-package 'company-racer)
(weemacs-require-package 'flycheck-rust)

;; Use flycheck-rust in rust-mode
(add-hook 'flycheck-mode-hook #'flycheck-rust-setup)

;; Setting up configurations when you load rust-mode
(add-hook 'rust-mode-hook #'racer-mode)
(add-hook 'rust-mode-hook #'eldoc-mode)

(provide 'weemacs-rust)
