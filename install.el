;; Execute this file to setup new environment.

;; Below snippet will collect installed package list in a file -- packages.txt
;; (with-temp-file "packages.txt" (insert (format "%S" package-activated-list)))

;; Below snippet will store installed package list in a variable -- package-list
;; (setq package-list package-activated-list)

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
		       yasnippet)
  "Packages to be installed at launch")

(unless package-archive-contents
  (package-refresh-contents))

;; Install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))
