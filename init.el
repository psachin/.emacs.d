;;; Let the magic begin..

;; Increase gc to 500MB for easy startup
(setq gc-cons-threshold (* 500 1024 1024))

(require 'package)
(add-to-list 'package-archives
			 '("gnu" . "http://elpa.gnu.org/packages/"))
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
			 '("melpa-stable" . "http://stable.melpa.org/packages/"))
(add-to-list 'package-archives
			 '("melpa" . "http://melpa.org/packages/"))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; From use-package Readme
(eval-when-compile
  (require 'use-package))
(require 'diminish)                ;; if you use :diminish
(require 'bind-key)                ;; if you use any :bind variant

(defun my-tangle-section-canceled ()
  "Return t if the current section header was CANCELED, else nil."
  (save-excursion
    (if (re-search-backward "^\\*+\\s-+\\(.*?\\)?\\s-*$" nil t)
        (string-prefix-p "CANCELED" (match-string 1))
      nil)))


;; This is GPLv2. If you still don't know the details, read
;; http://www.gnu.org/licenses/old-licenses/gpl-2.0.en.html
(defun my-tangle-config-org (orgfile)
  "This function will write all source blocks from =config.org= into
=config.el= that are ...

- not marked as :tangle no
- have a source-code of =emacs-lisp=
- doesn't have the todo-marker CANCELED"
  (let* ((body-list ())
		 (gc-cons-threshold most-positive-fixnum)
         (org-babel-src-block-regexp   (concat
                                        ;; (1) indentation                 (2) lang
                                        "^\\([ \t]*\\)#\\+begin_src[ \t]+\\([^ \f\t\n\r\v]+\\)[ \t]*"
                                        ;; (3) switches
                                        "\\([^\":\n]*\"[^\"\n*]*\"[^\":\n]*\\|[^\":\n]*\\)"
                                        ;; (4) header arguments
                                        "\\([^\n]*\\)\n"
                                        ;; (5) body
                                        "\\([^\000]*?\n\\)??[ \t]*#\\+end_src")))
    (with-temp-buffer
      (insert-file-contents orgfile)
      (goto-char (point-min))
      (while (re-search-forward org-babel-src-block-regexp nil t)
        (let ((lang (match-string 2))
              (args (match-string 4))
              (body (match-string 5))
              (canc (my-tangle-section-canceled)))
          (when (and (string= lang "emacs-lisp")
                     (not (string-match-p ":tangle\\s-+no" args))
                     (not canc))
			(add-to-list 'body-list body)))))
    (with-temp-file (setq elfile
					  (replace-regexp-in-string "org" "el" orgfile))
      (insert (format ";; Don't edit this file, edit %s instead ...\n\n" orgfile))
      (apply 'insert (reverse body-list)))
    (message "Wrote %s ..." elfile)))


;; Load config.org - my Emacs configuration
(my-tangle-config-org (concat user-emacs-directory "ReadMe.org"))

;; gc - decrease threshold to 5 MB
(add-hook 'after-init-hook (lambda () (setq gc-cons-threshold (* 5 1024 1024))))

;;; init.el ends here
