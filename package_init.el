;; load package init

(global-company-mode t)
(global-auto-complete-mode t)
(autopair-global-mode t)
(run-python)

;; Completion words longer than 4 characters
(custom-set-variables
 '(ac-ispell-requires 4)
 '(ac-ispell-fuzzy-limit 4))

(eval-after-load "auto-complete"
  '(progn
     (ac-ispell-setup)))

(yas-global-mode t)
(setq yas-snippet-dirs "~/.emacs.d/snippets/")

(setq org-latex-listings 'minted)

(setq org-export-latex-custom-lang-environments
      '(
        (emacs-lisp "common-lispcode")
        ))

(setq org-export-latex-minted-options
      '(("frame" "lines")
        ("fontsize" "\\scriptsize")
        ("linenos" "")))

(setq org-latex-pdf-process
  '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
    "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
    "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))


;; hidepw
(add-to-list 'load-path "~/.emacs.d/extensions/hidepw/")
(require 'hidepw)

(add-to-list 'auto-mode-alist
	     '("\\.gpg\\'" . (lambda () (hidepw-mode))))

;; Ruby--not working
(add-hook 'ruby-mode-hook 'robe-mode)

;; insert-shebang
(add-hook 'find-file-hook 'insert-shebang)

(provide 'package_init)
