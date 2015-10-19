(global-set-key (kbd "C-z") 'undo) ;; Ctrl+z
(global-set-key (kbd "C-S-z") 'undo-tree-redo) ;; Ctrl+Shift+z;

(global-set-key (kbd "C-c g") 'prelude-google)

(global-set-key (kbd "C-h C-m") 'discover-my-major)
(global-set-key (kbd "C-h M-m") 'discover-my-mode)

(global-set-key (kbd "M-s p") 'weemacs-occur-symbol-at-point)

(provide 'weemacs-global-keybindings)
