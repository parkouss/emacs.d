(require 'erc)

;; enable flyspell on every erc buffer
(erc-spelling-mode 1)

;; log
;; (setq erc-log-insert-log-on-open nil)
(setq erc-log-channels t)
(setq erc-log-channels-directory (expand-file-name "erc-logs" weemacs-dir))
(setq erc-save-buffer-on-part t)

(defadvice save-buffers-kill-emacs (before save-logs (arg) activate)
  (save-some-buffers t (lambda () (when (and (eq major-mode 'erc-mode)
                                             (not (null buffer-file-name)))))))

(add-hook 'erc-insert-post-hook 'erc-save-buffer-in-logs)
(add-hook 'erc-mode-hook '(lambda () (when (not (featurep 'xemacs))
                                       (set (make-variable-buffer-local
                                             'coding-system-for-write)
                                            'emacs-mule))))

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
