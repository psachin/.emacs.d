;; Execute this file to setup new environment.

(require 'package)

(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
	     '("melpa-stable" . "http://stable.melpa.org/packages/") t)

(package-initialize)
;; --------------------

(when (not package-archive-contents)
  (package-refresh-contents))

;; My packages(add your packages here)
(defvar package-list '(ac-ispell
		       autopair
		       company
		       find-file-in-project
		       insert-shebang
		       jedi
		       auto-complete
		       epc
		       ctable
		       concurrent
		       deferred
		       magit
		       git-timemachine
		       git-commit
		       magit-popup
		       markdown-mode
		       popup
		       swiper
		       with-editor
		       dash
		       async
		       yasnippet
		       elpy
		       iedit
		       bbdb)
  "Packages to be installed at launch")

;; Install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))
