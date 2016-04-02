;; load other configs

(load-file "~/.emacs.d/general_config.el")
(load-file "~/.emacs.d/install.el")
(load-file "~/.emacs.d/defalias.el")
(load-file "~/.emacs.d/kbd_macro.el")
(load-file "~/.emacs.d/package_init.el")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ac-ispell-fuzzy-limit 4)
 '(ac-ispell-requires 4)
 '(custom-enabled-themes (quote (tango-dark)))
 '(insert-shebang-file-types
   (quote
    (("py" . "python")
     ("sh" . "bash")
     ("pl" . "perl")
     ("rb" . "ruby"))))
 '(org-latex-minted-langs
   (quote
    ((emacs-lisp "common-lisp")
     (cc "c++")
     (cperl "perl")
     (shell-script "bash")
     (caml "ocaml")
     (ruby "ruby"))))
 '(package-selected-packages
   (quote
    (git-timemachine find-file-in-project robe markdown-mode jedi yasnippet ac-ispell insert-shebang magit company autopair)))
 '(read-buffer-completion-ignore-case t)
 '(read-file-name-completion-ignore-case t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
