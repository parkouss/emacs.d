(defvar my-conf-dir (file-name-directory load-file-name)
  "The root dir of the emacs conf (~/.emacs.d).")

(require 'org)

;; secrets
(load (expand-file-name "passwd.el"
                        (expand-file-name "private" my-conf-dir)))


(org-babel-load-file (expand-file-name "config.org" my-conf-dir))
