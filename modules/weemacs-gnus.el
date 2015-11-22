(weemacs-require-package 'bbdb)
(setq gnus-init-file (expand-file-name ".gnus" weemacs-dir ))

(require 'bbdb)

(bbdb-mua-auto-update-init 'message) ;; use 'gnus for incoming messages too
(setq bbdb-mua-auto-update-p 'query) ;; or 'create to create without asking

(setq bbdb-north-american-phone-numbers-p nil)
(setq bbdb-complete-name-full-completion t)
(setq bbdb-completion-type 'primary-or-name)
(setq bbdb-complete-name-allow-cycling t)

(bbdb-initialize 'gnus 'message 'sendmail)
(add-hook 'gnus-startup-hook 'bbdb-insinuate-gnus)

(provide 'weemacs-gnus)
