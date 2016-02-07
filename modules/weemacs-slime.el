(weemacs-require-packages '(slime slime-company))

(require 'slime-autoloads)

(setq inferior-lisp-program "/usr/bin/sbcl")
(setq slime-contribs '(slime-fancy))
(slime-setup '(slime-company))
(add-hook 'slime-repl-mode-hook (lambda () (paredit-mode +1)))

(provide 'weemacs-slime)
