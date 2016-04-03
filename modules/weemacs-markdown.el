;; markdow editing
(weemacs-require-package 'markdown-mode)

(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)

(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

(add-hook 'markdown-mode-hook
          (lambda ()
            (setq fill-column 80)
            (turn-on-auto-fill)))

(provide 'weemacs-markdown)
