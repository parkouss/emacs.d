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
  `(defun ,(intern (format "prelude-%s" search-engine-name)) ()
       ,(format "Search %s with a query or region if any." search-engine-name)
       (interactive)
       (weemacs-search ,search-engine-url ,search-engine-prompt)))

(weemacs-install-search-engine "google"     "http://www.google.com/search?q="              "Google: ")

(defun weemacs-occur-symbol-at-point ()
   (interactive)
   (let ((sym (thing-at-point 'symbol)))
     (if sym
        (push (regexp-quote sym) regexp-history)) ;regexp-history defvared in replace.el
       (call-interactively 'occur)))

(provide 'weemacs-core)
