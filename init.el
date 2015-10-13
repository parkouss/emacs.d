(defvar weemacs-dir (file-name-directory load-file-name)
  "The root dir of the weemacs (~/.emacs.d).")
(defvar weemacs-core-dir (expand-file-name "weemacs" weemacs-dir)
  "The home of weemacs's core functionality.")

;; add weemacs's directories to Emacs's `load-path'
(add-to-list 'load-path weemacs-core-dir)

;; reduce the frequency of garbage collection by making it happen on
;; each 50MB of allocated data (the default is on every 0.76MB)
(setq gc-cons-threshold 50000000)

;; the core stuff
(require 'weemacs-packages)
(require 'weemacs-ui)
(require 'weemacs-core)
(require 'weemacs-editor)
(require 'weemacs-diminish)
(require 'weemacs-global-keybindings)
