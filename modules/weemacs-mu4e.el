;; mu4e must be installed from outside emacs.
;; it is in AUR for archlinux, easy to compile/install.

(require 'mu4e)
(require 'smtpmail)

(require 'org-mu4e) ;; allow to capture links to mails

;; sending mails
(setq user-mail-address	"j.parkouss@gmail.com"
      user-full-name "Julien Pagès"
      send-mail-function 'smtpmail-send-it
      message-send-mail-function 'smtpmail-send-it
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587
      compose-mail-user-agent-warnings nil
      message-signature user-full-name)

;; mu4e
(setq
 ;; general
 mu4e-maildir "~/Maildir/personal"
 ;; allow for updating mail using 'U' in the main view:
 ;; this is mbsync, a LOT better than offlineimap.
 ;; on arch, pacman -S isync
 mu4e-get-mail-command (concat "mbsync -qaV -c "
                               (expand-file-name "private/.mbsyncrc" weemacs-dir))
 ;; mu4e-update-interval 300

 mu4e-drafts-folder "/brouillons"
 mu4e-sent-folder   "/envois"
 mu4e-trash-folder  "/corbeille"

 ;; don't save message to Sent Messages, Gmail/IMAP takes care of this
 ;;mu4e-sent-messages-behavior 'delete

 ;; required for mbsync
 ;;mu4e-change-filenames-when-moving t

 ;; smtp
 message-send-mail-function 'smtpmail-send-it
 smtpmail-stream-type 'starttls

 ;; attachment dir
 mu4e-attachment-dir  "~/Downloads"

 ;; insert sign
 mu4e-compose-signature "~Julien"
 mu4e-compose-signature-auto-include 't
 ;; don't keep message buffers around
 message-kill-buffer-on-exit t
 )

(provide 'weemacs-mu4e)
