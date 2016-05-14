;; load package init

;; To avoid make error
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Declaring-Functions.html
(declare-function global-company-mode "company.el")
(declare-function global-auto-complete-mode "auto-complete.el")
(declare-function autopair-global-mode "autopair.el")
(declare-function ac-ispell-setup "ac-ispell.el")
(declare-function yas-global-mode "yasnippet.el")

(global-company-mode t)
(global-auto-complete-mode t)
(autopair-global-mode t)
;; (run-python)

;; Completion words longer than 4 characters
(custom-set-variables
 '(ac-ispell-requires 4)
 '(ac-ispell-fuzzy-limit 4))

(eval-after-load "auto-complete"
  '(progn
     (ac-ispell-setup)))

(yas-global-mode t)
(setq-default yas-snippet-dirs "~/.emacs.d/snippets/")

;; hidepw
(add-to-list 'load-path "~/.emacs.d/extensions/hidepw/")
(require 'hidepw)
(add-to-list 'auto-mode-alist
	     '("\\.gpg\\'" . (lambda () (hidepw-mode))))

;; ---------- LaTeX ----------
(require 'ox-latex)
(add-to-list 'org-latex-classes
             '("beamer"
               "\\documentclass\[presentation\]\{beamer\}"
               ("\\section\{%s\}" . "\\section*\{%s\}")
               ("\\subsection\{%s\}" . "\\subsection*\{%s\}")
               ("\\subsubsection\{%s\}" . "\\subsubsection*\{%s\}")))

(setq org-latex-listings 'minted)

(setq-default org-export-latex-custom-lang-environments
	      '(
		(emacs-lisp "common-lispcode")
		))

(setq-default org-export-latex-minted-options
	      '(("frame" "lines")
		("fontsize" "\\scriptsize")
		("linenos" "")))

(setq org-latex-pdf-process
  '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
    "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
    "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))

;; Do not highlight indentation, specially during elpy-mode
(setq-default highlight-indentation-mode nil)

(provide 'package_init)
