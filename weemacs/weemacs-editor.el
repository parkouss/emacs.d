;; don't use tabs to indent
(setq-default indent-tabs-mode nil)

;; Newline at end of file
(setq require-final-newline t)

;; delete the selection with a keypress
(delete-selection-mode t)

;; store all backup and autosave files in the tmp dir
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; revert buffers automatically when underlying files are changed externally
(global-auto-revert-mode t)

;; smart pairing for all
(require 'smartparens-config)

;; saveplace remembers your location in a file when saving files
(require 'saveplace)
(setq save-place-file (expand-file-name "saveplace" weemacs-savefile-dir))
;; activate it for all buffers
(setq-default save-place t)

;; highlight the current line
(global-hl-line-mode +1)

(require 'volatile-highlights)
(volatile-highlights-mode t)

;; sensible undo
(global-undo-tree-mode)

;; flycheck
(add-hook 'after-init-hook #'global-flycheck-mode)

;; flyspell-mode does spell-checking on the fly as you type
(require 'flyspell)
(setq ispell-program-name "aspell" ; use aspell instead of ispell
      ispell-extra-args '("--sug-mode=ultra"))
(flyspell-mode +1)

;; enable the text completion framework
(global-company-mode)

;; python edition
;; first Add company-anaconda to allowed company-mode backends list
(eval-after-load "company"
 '(progn
   (add-to-list 'company-backends 'company-anaconda)))

(add-hook 'python-mode-hook 'anaconda-mode)
;;(add-hook 'python-mode-hook 'eldoc-mode) ;; provide ElDoc

(provide 'weemacs-editor)
