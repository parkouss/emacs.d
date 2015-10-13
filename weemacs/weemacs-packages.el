(require 'cl)
(require 'package)

(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
;; set package-user-dir to be relative to weemacs install path
(setq package-user-dir (expand-file-name "melpa" weemacs-dir))
(package-initialize)

(defvar weemacs-packages
  '(
    ;; check syntax on the fly
    flycheck
    ;; great undo-redo
    undo-tree
    ;; sexy mode-line for emacs
    smart-mode-line
    ;; reduce mode-line verbosity
    diminish
    ;; fun with parenthesis
    smartparens
    ;; completion framework
    company
    ;; gives useful visual feedback for what your operation
    ;; actually changed in the buffer
    volatile-highlights
    ;; I like this theme
    spacemacs-theme
    ;; python completion
    anaconda-mode
    company-anaconda)
  "A list of packages to ensure are installed at launch.")


(defun weemacs-packages-installed-p ()
  "Check if all packages in `weemacs-packages' are installed."
  (every #'package-installed-p weemacs-packages))

(defun weemacs-require-package (package)
  "Install PACKAGE unless already installed."
  (unless (memq package weemacs-packages)
    (add-to-list 'weemacs-packages package))
  (unless (package-installed-p package)
    (package-install package)))

(defun weemacs-require-packages (packages)
  "Ensure PACKAGES are installed.
Missing packages are installed automatically."
  (mapc #'weemacs-require-package packages))

(defun weemacs-install-packages ()
  "Install all packages listed in `weemacs-packages'."
  (unless (weemacs-packages-installed-p)
    ;; check for new packages (package versions)
    (message "%s" "weemacs is now refreshing its package database...")
    (package-refresh-contents)
    (message "%s" " done.")
    ;; install the missing packages
    (weemacs-require-packages weemacs-packages)))

;; run package installation
(weemacs-install-packages)

(provide 'weemacs-packages)
