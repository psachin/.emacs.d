;;; Let the magic begin..

;; Increase gc to 500MB for easy startup
(setq gc-cons-threshold (* 500 1024 1024))

(require 'package)
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(add-to-list 'package-archives '("marmalade" . "https://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package)
  (package-install 'org)
  (package-install 'diminish)
  (package-install 'bind-key))

;; From use-package Readme
(eval-when-compile
  (require 'use-package))
(require 'diminish)                ;; if you use :diminish
(require 'bind-key)                ;; if you use any :bind variant

;; Load config.org - my Emacs configuration
(org-babel-load-file (concat user-emacs-directory "ReadMe.org"))

;; gc - decrease threshold to 5 MB
(add-hook 'after-init-hook (lambda () (setq gc-cons-threshold (* 5 1024 1024))))

;;; init.el ends here
(put 'scroll-left 'disabled nil)
