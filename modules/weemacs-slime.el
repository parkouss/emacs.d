(weemacs-require-package 'slime)

(require 'slime-autoloads)

(setq inferior-lisp-program "/usr/bin/clisp")
(setq slime-contribs '(slime-fancy))

(provide 'weemacs-slime)
