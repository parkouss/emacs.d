(weemacs-require-package 'w3m)

(defun browse-url-conkeror (url &rest args)
  "Open an url in conkeror"
  (interactive (browse-url-interactive-arg "URL: "))
  (let ((browse-url-generic-program "conkeror"))
      (browse-url-generic url)))

(require 'w3m)

(setq w3m-use-cookies t)

;; open url with w3m by default, and conkeror if C-u is used
(setq browse-url-browser-function
      (lambda (url &rest args)
        (apply
         (if current-prefix-arg 'browse-url-conkeror
           'w3m-goto-url-new-session)
         url args)))

;;change w3m user-agent to android
(setq w3m-user-agent "Mozilla/5.0 (Linux; U; Android 2.3.3; zh-tw; HTC_Pyramid Build/GRI40) AppleWebKit/533.1 (KHTML, like Gecko) Version/4.0 Mobile Safari/533.")

(setq w3m-default-display-inline-images t)

(w3m-lnum-mode 1)

(add-hook 'w3m-display-hook
          (lambda (url)
            (let ((buffer-read-only nil))
              (delete-trailing-whitespace))))

;;quick access hacker news
(defun hacker-news ()
  (interactive)
  (browse-url "http://news.ycombinator.com"))

(defun gmail ()
  (interactive)
  (browse-url "https://mail.google.com/mail/u/0/h/1xm23lljy7zd"))

(provide 'weemacs-w3m)
