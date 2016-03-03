(weemacs-require-package 'yasnippet)

(require 'yasnippet)

(setq yas-prompt-functions '(yas-ido-prompt))

(yas-reload-all)
(add-hook 'python-mode-hook #'yas-minor-mode)

(provide 'weemacs-yasnippet)
