;; Diminish modeline clutter
(require 'diminish)

(eval-after-load "undo-tree"
  '(diminish 'undo-tree-mode " U"))

(eval-after-load 'flycheck
  '(diminish 'flycheck-mode " ⓢ"))

(eval-after-load "flyspell"
  '(diminish 'flyspell-mode " ⓢ"))

(diminish 'company-mode " ⓐ")

(provide 'weemacs-diminish)
