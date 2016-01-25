(defun weemacs-search (query-url prompt)
  "Open the search url constructed with the QUERY-URL.
PROMPT sets the `read-string prompt."
  (browse-url
   (concat query-url
           (url-hexify-string
            (if mark-active
                (buffer-substring (region-beginning) (region-end))
              (read-string prompt))))))

(defmacro weemacs-install-search-engine (search-engine-name search-engine-url search-engine-prompt)
  "Given some information regarding a search engine, install the interactive command to search through them"
  `(defun ,(intern (format "weemacs-%s" search-engine-name)) ()
       ,(format "Search %s with a query or region if any." search-engine-name)
       (interactive)
       (weemacs-search ,search-engine-url ,search-engine-prompt)))

(weemacs-install-search-engine "google"  "http://www.google.com/search?q="                    "Google: ")
(weemacs-install-search-engine "dxr"     "https://dxr.mozilla.org/mozilla-central/search?q="  "dxr: ")


(defun weemacs-occur-symbol-at-point ()
   (interactive)
   (let ((sym (thing-at-point 'symbol)))
     (if sym
        (push (regexp-quote sym) regexp-history)) ;regexp-history defvared in replace.el
       (call-interactively 'occur)))

(defun weemacs-point-to-register ()
  (interactive)
  (point-to-register 1)
  (message "position saved."))

(defun weemacs-jump-to-register ()
  (interactive)
  (jump-to-register 1))


(defun rename-file-and-buffer ()
  "Rename the current buffer and file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (message "Buffer is not visiting a file!")
      (let ((new-name (read-file-name "New name: " filename)))
        (cond
         ((vc-backend filename) (vc-rename-file filename new-name))
         (t
          (rename-file filename new-name t)
          (set-visited-file-name new-name t t)))))))


(defun create-temp-buffer (name)
  "Create quicly a ready to use buffer"
  (interactive "sName of the new buffer: ")
  (switch-to-buffer (generate-new-buffer name))
  (let ((buffer-file-name name))
    (set-auto-mode t)))

(defun sudo ()
  "Use TRAMP to `sudo' the current buffer"
  (interactive)
  (when buffer-file-name
    (find-alternate-file
     (concat "/sudo:root@localhost:"
             buffer-file-name))))


(provide 'weemacs-core)
