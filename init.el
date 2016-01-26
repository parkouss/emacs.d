(defvar weemacs-dir (file-name-directory load-file-name)
  "The root dir of the weemacs (~/.emacs.d).")
(defvar weemacs-private-dir (expand-file-name "private" weemacs-dir)
  "private dir")
(defvar weemacs-core-dir (expand-file-name "weemacs" weemacs-dir)
  "The home of weemacs's core functionality.")
(defvar weemacs-modules-dir (expand-file-name "modules" weemacs-dir)
  "Module dir of weemacs.")
(defvar weemacs-savefile-dir (expand-file-name "savefile-dir" weemacs-dir)
  "directory of weemacs where temp files are stored.")
(defvar weemacs-modules-file (expand-file-name "weemacs-modules.el" weemacs-dir)
  "This files contains a list of modules that will be loaded by weemacs.")

;; add weemacs's directories to Emacs's `load-path'
(add-to-list 'load-path weemacs-core-dir)
(add-to-list 'load-path weemacs-modules-dir)

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

;; the modules
(if (file-exists-p weemacs-modules-file)
    (load weemacs-modules-file)
  (message "Missing modules file %s" weemacs-modules-file)
  (message "You can get started by copying the bundled example file"))
