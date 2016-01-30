(weemacs-require-package 'bbdb)
(setq gnus-init-file (expand-file-name ".gnus" weemacs-dir ))

(setq user-mail-address	"j.parkouss@gmail.com"
      user-full-name "Julien Pagès"
      send-mail-function 'smtpmail-send-it
      message-send-mail-function 'smtpmail-send-it
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587
      compose-mail-user-agent-warnings nil
      message-signature user-full-name)

(setq bbdb-file (expand-file-name "bbdb" weemacs-private-dir))
(require 'bbdb)

(bbdb-mua-auto-update-init 'message) ;; use 'gnus for incoming messages too
(setq bbdb-mua-auto-update-p 'query) ;; or 'create to create without asking

(setq bbdb-north-american-phone-numbers-p nil
      bbdb-complete-name-full-completion t
      bbdb-completion-type 'primary-or-name
      bbdb-complete-name-allow-cycling t
      bbdb-completion-display-record nil)

(bbdb-initialize 'gnus 'message 'sendmail)
(add-hook 'gnus-startup-hook 'bbdb-insinuate-gnus)

(defun gnus-grace-exit-before-kill-emacs ()
  (if (and (fboundp 'gnus-alive-p)
           (gnus-alive-p))
      (let ((noninteractive t))
        (gnus-group-exit))))

(add-hook 'kill-emacs-hook 'gnus-grace-exit-before-kill-emacs)

(provide 'weemacs-gnus)
