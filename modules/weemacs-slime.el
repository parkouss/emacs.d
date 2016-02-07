(weemacs-require-packages '(slime slime-company))

(require 'slime-autoloads)

(setq inferior-lisp-program "/usr/bin/sbcl")
(setq slime-contribs '(slime-fancy))
(slime-setup '(slime-company))

(provide 'weemacs-slime)
