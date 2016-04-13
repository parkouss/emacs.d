(weemacs-require-packages '(js2-mode tern company-tern coffee-mode))

(defun my-key-binding ()
  "Change key binding for tern"
  (local-set-key (kbd "M-.") 'tern-find-definition)
  )

(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

(setq-default js2-basic-offset 4)
;; no warning for trailing commas
(setq-default js2-strict-trailing-comma-warning nil)

(add-hook 'js2-mode-hook (lambda ()
                           (tern-mode t)
                           (local-set-key (kbd "M-*") 'tern-pop-find-definition)
                           (local-set-key (kbd "M-?") 'tern-get-docs)
                           ))

(add-hook 'coffee-mode-hook (lambda ()
                              (setq coffee-tab-width 2)))

(provide 'weemacs-javascript)
