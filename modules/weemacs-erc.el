(require 'erc)

(require 'erc-services)
(erc-services-mode 1) ;; enable nickserv

(let ((pass-file (expand-file-name ".ercpass" weemacs-dir)))
  (if (file-exists-p pass-file)
      (progn
        ;; allow automatic nickserv on mozilla.org
        (add-to-list 'erc-nickserv-alist
                     '(Mozilla  ;; network name
                       "NickServ!services@ircservices.mozilla.org" ;; NickServ user
                       "This nickname is registered and protected\\.  If it is your" ;; first line sent by NickServ
                       "NickServ" ;; the one to answer to
                       "IDENTIFY" ;; keyword to send
                       nil ;; use current nick
                       ))
        ;; don't prompt, take from the pass file
        (setq erc-prompt-for-nickserv-password nil)
        (load pass-file)
        (setq erc-nickserv-passwords
              `((Mozilla (("parkouss" . ,mozilla-parkouss-pass)
                          ("parkouss|afk" . ,mozilla-parkouss-pass)))))
       )))

(setq erc-user-full-name "Julien Pagès")

;; enable flyspell on every erc buffer
(erc-spelling-mode 1)

;; log
;; (setq erc-log-insert-log-on-open nil)
(setq erc-log-channels t)
(setq erc-log-channels-directory (expand-file-name "erc-logs" weemacs-dir))
(setq erc-save-buffer-on-part t)

;; switch afk nick
(defun erc-switch-afk-command (nick)
  (let ((newnick (if (string-suffix-p "|afk" nick)
                     (string-remove-suffix "|afk" nick)
                   (concat nick "|afk")
                   )))
    (concat "/nick " newnick)))

(defun erc-switch-afk ()
  (interactive)
  (goto-char (point-max))
  (if (= (point-max) (line-beginning-position))
      (progn
        (insert (erc-switch-afk-command (erc-current-nick)))
        (erc-send-current-line))
    (message "line is not empy!")))

(add-hook 'erc-insert-post-hook 'erc-save-buffer-in-logs)
(add-hook 'erc-mode-hook '(lambda ()
                            (when (not (featurep 'xemacs))
                              (set (make-variable-buffer-local
                                    'coding-system-for-write)
                                   'emacs-mule))
                            (local-set-key (kbd "C-c a") 'erc-switch-afk)
                            ))

;; freedesktop notifications on PM or nickname mentioned in channel
(add-to-list 'erc-modules 'notifications)

;; Join the #ateam channel whenever connecting to mozilla irc.
(setq erc-autojoin-channels-alist
      '(("mozilla.org" "#ateam")))

(defun mozilla-erc ()
  "Connect to mozilla irc"
  (interactive)
  (erc :server "irc.mozilla.org" :port 6667 :nick "parkouss")
  )

(provide 'weemacs-erc)
