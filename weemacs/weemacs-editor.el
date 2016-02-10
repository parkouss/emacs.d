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
(define-key smartparens-mode-map (kbd "C-(") 'sp-backward-slurp-sexp)
(define-key smartparens-mode-map (kbd "C-)") 'sp-forward-slurp-sexp)
(define-key smartparens-mode-map (kbd "C-M-(") 'sp-backward-barf-sexp)
(define-key smartparens-mode-map (kbd "C-M-)") 'sp-forward-barf-sexp)
(add-hook 'prog-mode-hook 'smartparens-mode)

;; smarter kill-ring navigation (M-y)
(require 'browse-kill-ring)
(browse-kill-ring-default-keybindings)

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
(setq ispell-dictionary "english")
(add-hook 'text-mode-hook (lambda () (flyspell-mode 1)))
(add-hook 'prog-mode-hook (lambda () (flyspell-prog-mode)))
(dolist (hook '(change-log-mode-hook log-edit-mode-hook))
  (add-hook hook (lambda () (flyspell-mode -1))))

;; enable the text completion framework
(setq company-require-match nil)  ;; allow to type whatever I want to during completion
(setq company-dabbrev-downcase nil)  ;; try to respect case completion
(global-company-mode)

(define-key company-active-map (kbd "\C-n") 'company-select-next)
(define-key company-active-map (kbd "\C-p") 'company-select-previous)
(define-key company-active-map (kbd "\C-d") 'company-show-doc-buffer)
(define-key company-active-map (kbd "M-.") 'company-show-location)


;; use shift + arrow keys to switch between visible buffers
(require 'windmove)
(windmove-default-keybindings)

;; projectile is a project management mode
(require 'projectile)
(setq projectile-cache-file (expand-file-name  "projectile.cache" weemacs-savefile-dir))
(setq projectile-use-git-grep 1)
(projectile-global-mode t)

;; allow narrowing to focus on part of the buffer
;; C-x n n to narrow down selection
;; C-x n w to see the whole buffer again
(put 'narrow-to-region 'disabled nil)

;; undo and redo changes in the window configuration
(winner-mode 1)

(delete-selection-mode 1)

;; use C-w to kill a line with EOL
(whole-line-or-region-mode 1)

(require 'dired-x)

;; abbreviations
(setq abbrev-file-name (expand-file-name "abbrev_defs" weemacs-dir))
(setq save-abbrevs t)
(setq-default abbrev-mode t)

(provide 'weemacs-editor)
