(weemacs-require-package 'yasnippet)

(setq yas-prompt-functions '(yas-ido-prompt))

(global-set-key (kbd "C-c s") 'yas-insert-snippet)

(yas-global-mode 1)

(provide 'weemacs-yasnippet)
