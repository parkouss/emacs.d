(defvar my-conf-dir (file-name-directory load-file-name)
  "The root dir of the emacs conf (~/.emacs.d).")

(require 'org)

;; allow to store modules I want to disable
;; example: (setq modules-disabled '((pdf . t)))
(setq modules-disabled '())

;; allow to override, based on some config
(let ((conf-file (expand-file-name "custom.el" my-conf-dir)))
  (if (file-exists-p conf-file)
      (load conf-file)))

(defun module-disabled? (module)
  (cdr (assoc module modules-disabled)))

;; secrets
(setq has-secrets nil)
(let ((secret-file (expand-file-name
                    "passwd.el"
                    (expand-file-name "private" my-conf-dir))))
  (if (file-exists-p secret-file)
      (progn
        (setq has-secrets nil)
        (load secret-file))))


(org-babel-load-file (expand-file-name "config.org" my-conf-dir))
