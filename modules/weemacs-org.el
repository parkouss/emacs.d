(weemacs-require-packages '(org org-bullets
                                ;; htmlize does code coloring for org-reveal
                                ox-reveal htmlize))

(require 'org-protocol)

;; Make windmove work in org-mode (do this before loading org):
(setq org-replace-disputed-keys t)

(require 'ox-reveal)
(require 'ob-dot)

(setq org-return-follows-link t)

(setq org-default-notes-file (expand-file-name "notes.org" weemacs-dir))

(setq org-agenda-files (list (expand-file-name "agenda" weemacs-dir)))

(setq org-tag-alist '((:startgroup . nil)
                      ("@work" . ?w) ("@home" . ?h)
                      (:endgroup . nil)))

(add-hook 'org-mode-hook
          (lambda ()
            (turn-on-auto-fill)
            (org-bullets-mode 1)

            ;; from http://www.howardism.org/Technical/Emacs/orgmode-wordprocessor.html
            ;; remove headers colors
            (let* ((variable-tuple (cond ((x-list-fonts "Source Sans Pro") '(:font "Source Sans Pro"))
                                         ((x-list-fonts "Lucida Grande")   '(:font "Lucida Grande"))
                                         ((x-list-fonts "Verdana")         '(:font "Verdana"))
                                         ((x-family-fonts "Sans Serif")    '(:family "Sans Serif"))
                                         (nil (warn "Cannot find a Sans Serif Font.  Install Source Sans Pro."))))
                   (base-font-color     (face-foreground 'default nil 'default))
                   (headline           `(:inherit default :weight bold :foreground ,base-font-color)))

              (custom-theme-set-faces 'user
                                      `(org-level-8 ((t (,@headline ,@variable-tuple))))
                                      `(org-level-7 ((t (,@headline ,@variable-tuple))))
                                      `(org-level-6 ((t (,@headline ,@variable-tuple))))
                                      `(org-level-5 ((t (,@headline ,@variable-tuple))))
                                      `(org-level-4 ((t (,@headline ,@variable-tuple :height 1.1))))
                                      `(org-level-3 ((t (,@headline ,@variable-tuple :height 1.25))))
                                      `(org-level-2 ((t (,@headline ,@variable-tuple :height 1.5))))
                                      `(org-level-1 ((t (,@headline ,@variable-tuple :height 1.75))))
                                      `(org-document-title ((t (,@headline ,@variable-tuple :height 1.5 :underline nil))))))))

(provide 'weemacs-org)
