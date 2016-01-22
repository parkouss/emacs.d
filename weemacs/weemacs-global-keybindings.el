(global-set-key (kbd "C-z") 'undo) ;; Ctrl+z
(global-set-key (kbd "C-S-z") 'undo-tree-redo) ;; Ctrl+Shift+z;

(global-set-key (kbd "C-c l g") 'weemacs-google)
(global-set-key (kbd "C-c l d") 'weemacs-dxr)

(global-set-key (kbd "C-h C-m") 'discover-my-major)
(global-set-key (kbd "C-h M-m") 'discover-my-mode)

(global-set-key (kbd "M-s p") 'weemacs-occur-symbol-at-point)

(global-set-key (kbd "C-c m") 'weemacs-point-to-register)
(global-set-key (kbd "C-c j") 'weemacs-jump-to-register)

(global-set-key (kbd "C-c r")  'rename-file-and-buffer)
(global-set-key (kbd "C-c b") 'create-temp-buffer)

(global-set-key (kbd "TAB") #'company-indent-or-complete-common)

;; full screen is <f11> by default, but I do not see desktop
;; tooltips with gnome in this case - so here is a workaround
(global-set-key (kbd "<f12>") 'toggle-frame-maximized)

(provide 'weemacs-global-keybindings)
