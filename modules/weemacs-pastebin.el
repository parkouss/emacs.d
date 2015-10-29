;;; weemacs-pastebin.el --- A simple interface to the www.pastebin.com webservice

;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2, or (at your option)
;;; any later version.

;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.

;;; You should have received a copy of the GNU General Public License
;;; along with this program; see the file COPYING.  If not, write to the
;;; Free Software Foundation, Inc.,   51 Franklin Street, Fifth Floor,
;;; Boston, MA  02110-1301  USA

;;; Commentary:
;;;
;;; borrowed and adapted from https://github.com/nicferrier/elpastebin.
;;;
;;; Load this file and run:
;;;
;;;  M-x pastebin
;;;
;;; to send just the region.
;;;
;;; In either case the url that pastebin generates is left on the kill
;;; ring and the paste buffer.


;;; Code:

(setq pastebin-dev-key "2b6422b7a9f84246b4a84aa5cf068de7")
(setq pastebin-url "http://pastebin.com/api/api_post.php")

(defcustom pastebin-type-assoc
  '((actionscript-mode . " actionscript")
    (ada-mode . "ada")
    (asm-mode . "asm")
    (autoconf-mode . "bash")
    (bibtex-mode . "bibtex")
    (cmake-mode . "cmake")
    (c-mode . "c")
    (c++-mode . "cpp")
    (cobol-mode . "cobol")
    (conf-colon-mode . "properties")
    (conf-javaprop-mode . "properties")
    (conf-mode . "ini")
    (conf-space-mode . "properties")
    (conf-unix-mode . "ini")
    (conf-windows-mode . "ini")
    (cperl-mode . "perl")
    (csharp-mode . "csharp")
    (css-mode . "css")
    (delphi-mode . "delphi")
    (diff-mode . "dff")
    (magit-diff-mode . "diff")
    (ebuild-mode . "bash")
    (eiffel-mode . "eiffel")
    (emacs-lisp-mode . "lisp")
    (erlang-mode . "erlang")
    (erlang-shell-mode . "erlang")
    (espresso-mode . "javascript")
    (fortran-mode . "fortran")
    (glsl-mode . "glsl")
    (gnuplot-mode . "gnuplot")
    (graphviz-dot-mode . "dot")
    (haskell-mode . "haskell")
    (html-mode . "html4strict")
    (idl-mode . "idl")
    (inferior-haskell-mode . "haskell")
    (inferior-octave-mode . "octave")
    (inferior-python-mode . "python")
    (inferior-ruby-mode . "ruby")
    (java-mode . "java")
    (js2-mode . "javascript")
    (jython-mode . "python")
    (latex-mode . "latex")
    (lisp-mode . "lisp")
    (lua-mode . "lua")
    (makefile-mode . "make")
    (makefile-automake-mode . "make")
    (makefile-gmake-mode . "make")
    (makefile-makepp-mode . "make")
    (makefile-bsdmake-mode . "make")
    (makefile-imake-mode . "make")
    (matlab-mode . "matlab")
    (nxml-mode . "xml")
    (oberon-mode . "oberon2")
    (objc-mode . "objc")
    (ocaml-mode . "ocaml")
    (octave-mode . "matlab")
    (pascal-mode . "pascal")
    (perl-mode . "perl")
    (php-mode . "php")
    (plsql-mode . "plsql")
    (po-mode . "gettext")
    (prolog-mode . "prolog")
    (python-2-mode . "python")
    (python-3-mode . "python")
    (python-basic-mode . "python")
    (python-mode . "python")
    (ruby-mode . "ruby")
    (scheme-mode . "lisp")
    (shell-mode . "bash")
    (sh-mode . "bash")
    (smalltalk-mode . "smalltalk")
    (sql-mode . "sql")
    (tcl-mode . "tcl")
    (visual-basic-mode . "vb")
    (xml-mode . "xml")
    (yaml-mode . "properties"))
  "Alist composed of major-mode names and corresponding pastebin highlight formats."
  :type '(alist :key-type symbol :value-tupe string)
  :group 'pastebin)

;; see http://pastebin.com/api#1
(defun pastebin--build-post-data (text &rest args)
  (let ((dev-key (or (plist-get args :dev-key) pastebin-dev-key))
        (paste-code (url-hexify-string text))
        (paste-private (or (plist-get args :paste-private) "1"))
        (paste-name (url-hexify-string
                     (or (plist-get args :paste-name) "untitled")))
        (paste-expire-date (or (plist-get args :paste-expire-date) "2W"))
        (paste-format (or (plist-get args :paste-format) "text"))
        (user-key (or (plist-get args :user-key) ""))
        )
    (format "api_option=paste&api_user_key=%s&api_paste_private=%s&api_paste_name=%s&api_paste_expire_date=%s&api_paste_format=%s&api_dev_key=%s&api_paste_code=%s"
            user-key
            paste-private
            paste-name
            paste-expire-date
            paste-format
            dev-key
            paste-code)))

(defun pastebin-post (text &rest args)
  (let ((url-request-method "POST")
        (url-request-extra-headers
         '(("Content-Type" . "application/x-www-form-urlencoded")))
        (url-request-data (apply 'pastebin--build-post-data text args)))
    (url-retrieve
     pastebin-url
     (lambda (status)
       (cond
        ((equal :error (car status))
         (signal 'pastebin-error (cdr status)))
        (t
         (re-search-forward "\n\n")
         (clipboard-kill-ring-save (point) (point-max))
         (message "Pastebin URL: %s" (buffer-substring (point) (point-max)))
         (kill-buffer (current-buffer))))))))

(defun pastebin (start end)
  "Send the region to the pastebin.com.

Called interactively pastebin uses the current region for
preference for sending... if the mark is NOT set then the entire
buffer is sent.

Argument START is the start of region.
Argument END is the end of region."

  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end))
     (list (point-min) (point-max))))
  (let ((text (buffer-substring-no-properties start end))
        (paste-format (or (assoc-default major-mode pastebin-type-assoc) "text"))
        (paste-name (buffer-name)))
    (pastebin-post text :paste-format paste-format :paste-name paste-name)))

(provide 'weemacs-pastebin)
;;; weemacs-pastebin.el ends here
