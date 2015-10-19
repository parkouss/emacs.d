;; Diminish modeline clutter
(require 'diminish)

(eval-after-load "undo-tree"
  '(diminish 'undo-tree-mode))

(eval-after-load 'flycheck
  '(diminish 'flycheck-mode " ©"))

(eval-after-load "flyspell"
  '(diminish 'flyspell-mode " ⓢ"))

(diminish 'company-mode " ⓐ")

(diminish 'volatile-highlights-mode)

(diminish 'projectile-mode)

(provide 'weemacs-diminish)
