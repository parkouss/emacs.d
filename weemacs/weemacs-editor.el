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
(smartparens-global-mode t)

;; saveplace remembers your location in a file when saving files
;; activate it for all buffers
(setq-default save-place t)
(require 'saveplace)

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

;; ido support
(require 'ido)
(ido-mode t)

(provide 'weemacs-editor)
