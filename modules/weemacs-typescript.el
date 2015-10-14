(weemacs-require-package 'tss)

(require 'typescript)
(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-mode))

(require 'tss)

;; no flymake
(defun tss-run-flymake ())

(setq tss-popup-help-key "M-?")
(setq tss-jump-to-definition-key "M-.")

(tss-config-default)

(provide 'weemacs-typescript)
