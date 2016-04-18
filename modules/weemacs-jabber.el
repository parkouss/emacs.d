(weemacs-require-package 'jabber)

(setq jabber-account-list `(
                            ;; steery.io account
                            ("213996_3664926@chat.hipchat.com"
                             (:password . ,jabber_steery_password))))

(setq jabber-auto-reconnect t)

(eval-after-load 'jabber
  `(progn
     ;; Message alert hooks
     (define-jabber-alert echo "Show a message in the echo area"
       (lambda (msg)
         (unless (minibuffer-prompt)
           (message "%s" msg))))))

(provide 'weemacs-jabber)
