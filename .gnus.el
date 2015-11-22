(setq user-mail-address	"j.parkouss@gmail.com"
      user-full-name "Julien Pagès"
      send-mail-function 'smtpmail-send-it
      message-send-mail-function 'smtpmail-send-it
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587)

;; (setq gnus-asynchronous t)

;;@see http://www.emacswiki.org/emacs/GnusGmail
(setq gnus-select-method '(nntp "news.mozilla.org")
      gnus-ignored-newsgroups "^to\\.\\|^[0-9. ]+\\( \\|$\\)\\|^[\"]\"[#'()]")


;;@see http://gnus.org/manual/gnus_397.html
(add-to-list 'gnus-secondary-select-methods
             '(nnimap "gmail"
                      (nnimap-address "imap.gmail.com")
                      (nnimap-server-port 993)
                      (nnimap-stream ssl)
                      (nnir-search-engine imap)
                      ; @see http://www.gnu.org/software/emacs/manual/html_node/gnus/Expiring-Mail.html
                      ;; press 'E' to expire email
                      (nnmail-expiry-target "nnimap+gmail:[Gmail]/Trash")
                      (nnmail-expiry-wait 90)))

(setq gnus-permanently-visible-groups ".*tools\\|.*INBOX\\|nnrss.*Automation")

;; archive message expired on inbox after two days
(setq gnus-parameters
      '(("nnimap\\+gmail:INBOX" (expiry-wait . 2))))

(add-hook 'gnus-after-getting-new-news-hook 'gnus-notifications)
(gnus-demon-add-handler 'gnus-demon-scan-news 3 t)
