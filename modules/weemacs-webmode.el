(weemacs-require-package 'web-mode)

(require 'web-mode)

(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.launch?\\'" . web-mode))

(defun my-web-mode-hook ()
  "Hooks for Web mode."
  (setq web-mode-markup-indent-offset 2)
  )

(add-hook 'web-mode-hook  'my-web-mode-hook)

(provide 'weemacs-webmode)
