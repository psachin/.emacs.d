;; emacs_general_config.el

(setq debug-on-error t)

;; Personal info
(setq user-full-name "Sachin"
      user-mail-address "iclcoolster@gmail.com")

;; start emacs server
(require 'server)
(unless (server-running-p)
  (server-start))

;;; --------------------
;;; package conf

;; add marmalade repo
(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))


;; melpa repo
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)
;; --------------------


;; Turn off mouse interface early in startup to avoid momentary display
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; 10 mins emacs for clojure
(setq-default inhibit-startup-screen t)

;; show matching parenthesis 
(show-paren-mode t)
(setq show-paren-style 'parenthesis) ; highlight just brackets
;; (setq show-paren-style 'expression) ; highlight entire bracket expression

;; hide tool-bar and menubar
(menu-bar-mode 0)
(tool-bar-mode 0)

;; show elisp function docs in result bar
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
(add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)

;; enable column-number mode
(column-number-mode t)

(ignore-errors (display-battery-mode t))
(display-time-mode t)
(size-indication-mode t)

;; delete selected text while typing
(delete-selection-mode 1)

;; turn on word wrap
(add-hook 'text-mode-hook '(lambda ()
			     (auto-fill-mode t)))

;; show funtion name in a mode line
(which-function-mode t)

;; enable pretty syntax highlighting everywhere
(global-font-lock-mode t)

;; To have the down arrow key move by screen lines
'(line-move-visual t)

;; wrap lines automatically
(auto-fill-mode t)

;; enable ido-mode to browse throught files quickly
;; (ido-mode t)

;; network-status-mode
;; (network-status-mode t)

;; Save history
(savehist-mode 1)

;; Indicate empty line
(setq-default indicate-empty-lines t)
(setq-default show-trailing-whitespace t)

;; Change "yes or no" to "y or n"
;; http://dl.dropboxusercontent.com/u/3968124/sacha-emacs.html
(fset 'yes-or-no-p 'y-or-n-p)

;; Make TAB key always call a indent command
(setq-default tab-always-indent t)

;; Make TAB key do indent first and then completion
(setq-default tab-always-indent 'complete)

;; Fill a line with space after a period
(setq sentence-end-double-space nil)

;; --------------------

(defun pluralize (word count &optional plural)
  "Pluralize the word."
  (if (= count 1)
      word
    (if (null plural)
	(concat word "s")
      plural)))

;; limit to 80 chars
;; TODO: to apply only code other than java.core
(require 'whitespace)
(setq whitespace-line-column 80) ;; limit line length
(setq whitespace-style '(face lines-tail))
(add-hook 'prog-mode-hook 'whitespace-mode)
(global-whitespace-mode +1)

;; Modes for certain file extensions
;; add C++ mode for .ino files(Arduino files)
(add-to-list 'auto-mode-alist
	     '("\\.ino\\.pde\\'" . c++-mode)
	     '("\\.h\\'" . c++-mode))

;; Enable Allman Style of indentation for C code. OpenSource for you, Jan 2014.
(setq c-default-style "linux"
      c-basic-offset 4)

;; Save all backup files in pne space
(setq backup-directory-alist '(("." . "/home/sachin/.emacs-saves")))

;; delete indentation
;; http://emacsredux.com/blog/2013/05/30/joining-lines/
(defun top-join-line()
  "Join the cunrrent line with the line beneath it"
  (interactive)
  (delete-indentation 1)
  )
(global-set-key (kbd "M-^") 'top-join-line)

;; search with google
(defun google ()
  "Google the selected region if any, display a query prompt otherwise."
  (interactive)
  (browse-url
   (concat
    "http://www.google.com/search?ie=utf-8&oe=utf-8&q="
    (url-hexify-string (if mark-active
         (buffer-substring (region-beginning) (region-end))
       (read-string "Search Google: "))))))
(global-set-key (kbd "C-x g") 'google)

;; search youtube[emacsredux.com]
(defun youtube ()
  "Search YouTube with a query or region if any."
  (interactive)
  (browse-url
   (concat
    "http://www.youtube.com/results?search_query="
    (url-hexify-string (if mark-active
                           (buffer-substring (region-beginning) (region-end))
                         (read-string "Search YouTube: "))))))
(global-set-key (kbd "C-x y") 'youtube)

;; terminal at your fingerprint
;; http://emacsredux.com/blog/page/2/
(defun visit-term-buffer ()
  "Create or visit a terminal buffer."
  (interactive)
  (if (not (get-buffer "*ansi-term*"))
      (progn
        (split-window-sensibly (selected-window))
        (other-window 1)
        (ansi-term (getenv "SHELL")))
    (switch-to-buffer-other-window "*ansi-term*")))
(global-set-key (kbd "C-c t") 'visit-term-buffer)

;; count total number of words in current buffer
(defun count-words-buffer ()
  "Count total number of words in current buffer."
  (interactive)
  (let ((count 0))
    (save-excursion
      (goto-char (point-min))
      (while (< (point) (point-max))
	(forward-word 1)
	(setq count (1+ count)))
      (if (zerop count)
	  (message "buffer has no words.")
	(message "buffer approximately has %d %s." count 
		 (pluralize "word" count))))))
(global-set-key (kbd "C-x c") 'count-words-buffer)

;; (defun count-sentence-buffer ()
;;   (interactive)
;;   (let ((count 0))
;;     (save-excursion
;;       (goto-char (point-min))
;;       (while (< (point) (point-max))
;; 	(forward-sentence 1)
;; 	(setq count (1+ count)))
;;       (message "buffer approximately has %d sentences." count))))
;; (global-set-key (kbd "C-c s") 'count-sentence-buffer)

(defun goto-percent (pct)
  "Go to place in a buffer expressed in percentage."
  (interactive "nPercent: ")
  (goto-char (/ (* (point-max) pct) 100)))
(global-set-key (kbd "C-x p") 'goto-percent)

;; Add workflow state in org-mode
(setq org-todo-keywords
  '((sequence "TODO" "IN-PROGRESS" "WAITING" "DONE")))

;; This will create a date-time stamp for tasks marked as 'DONE'.
(setq org-log-done t)

;; My org-agenda
(setq org-agenda-files (let (list)
			 (add-to-list 'list "/home/sachin/org/")))

;; Highlight Comment Annotations
(defun font-lock-comment-annotations ()
  "Highlight a bunch of well known comment annotations.
This functions should be added to the hooks of major modes for
programming."
  (font-lock-add-keywords
   nil '(("\\<\\(FIX\\(ME\\)?\\|TODO\\|OPTIMIZE\\|HACK\\|REFACTOR\\):"
          1 font-lock-warning-face t))))
(add-hook 'prog-mode-hook 'font-lock-comment-annotations)

;; edit emacs conf file [emacsredux.com]
(defun open-user-init-file ()
  "Find User Init file."
  (interactive)
  (find-file-other-window (concat
  user-init-file ".d/init/emacs_general_config.el")))
(global-set-key (kbd "C-x e") 'open-user-init-file)

;; Open emacs24.3 HOME [emacsredux.com]
(defun open-emacs-home ()
  "Open emacs HOME."
  (interactive)
  (find-file-other-window (substring
  user-init-file 0 -6)))
(global-set-key (kbd "C-c h") 'open-emacs-home)

;;; edit root file [emacsredux.com]

;;(defadvice find-file (after find-file-sudo activate)
;;  "Visit file as root."
;;  (unless (and buffer-file-name
;;	       (file-writable-p buffer-file-name))
;;    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

(defun sudo-edit (&optional arg)
  "Edit currently visited file as root.

With a prefix ARG prompt for a file to visit.
Will also prompt for a file to visit if current
buffer is not visiting a file."
  (interactive "P")
  (if (or arg (not buffer-file-name))
      (find-file (concat "/sudo:root@localhost:"
                         (ido-read-file-name "Find file(as root): ")))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))
(global-set-key (kbd "C-x C-r") 'sudo-edit)

;; instant access to shell init file
(defun find-shell-init-file ()
  "Edit the shell init file in another window."
  (interactive)
  (let* ((shell (car (reverse (split-string (getenv "SHELL") "/"))))
         (shell-init-file (cond
                           ((string-equal "zsh" shell) ".zshrc")
                           ((string-equal "bash" shell) ".bashrc")
                           (t (error "Unknown shell")))))
    (find-file-other-window (expand-file-name shell-init-file (getenv "HOME")))))
(global-set-key (kbd "C-c S") 'find-shell-init-file)


;;; for octopress
(setq org-publish-project-alist
      '(("blog" .  (:base-directory "/home/sachin/tmp/octopress/"
				    :base-extension "org"
				    :publishing-directory "/home/sachin/tmp/octopress/source/_posts/"
				    :sub-superscript ""
				    :recursive t
				    :publishing-function org-publish-org-to-octopress
				    :headline-levels 4
				    :html-extension "markdown"
				    :octopress-extension "markdown"
				    :body-only t))
	("blog-extra" . (:base-directory "/home/sachin/tmp/octopress/"
					 :publishing-directory "/home/sachin/tmp/octopress/source/"
					 :base-extension "css\\|pdf\\|png\\|jpg\\|gif\\|svg"
					 :publishing-function org-publish-attachment
					 :recursive t
					 :author nil
					 ))
	("blog" . (:components ("blog-org" "blog-extra")))
	))

;; Source: http://blog.paphus.com/blog/2012/08/01/introducing-octopress-blogging-for-org-mode/
(defun save-then-publish ()
  (interactive)
  (save-buffer)
  (org-save-all-org-buffers)
  (org-publish-current-project))

(add-to-list 'load-path "/home/sachin/github/orgmode-octopress/")
(require 'org-octopress)


(define-key global-map (kbd "RET") 'newline-and-indent)
(setq-default indent-tabs-mode t)

(defun viooz ()
  "Search movie on Viooz.co with selected region if any, display
a query prompt otherwise."
  (interactive)
  (browse-url
   (concat
    "http://viooz.co/search?q="
    (url-hexify-string (if mark-active
         (buffer-substring (region-beginning) (region-end))
       (read-string "Search Viooz.co: "))) "&s=t")))
(global-set-key (kbd "C-c v") 'viooz)

(defun sort-buffers ()
  "Put the buffer list in alphabetical order."
  (interactive)
  (dolist (buff (buffer-list-sorted)) (bury-buffer buff))
  (when (interactive-p) (list-buffers)))
;;(global-set-key "\M-b"    'sort-buffers)

(defun buffer-list-sorted ()
  (sort (buffer-list)
	(function
	 (lambda
	   (a b) (string<
		  (downcase (buffer-name a))
		  (downcase (buffer-name b))
		  )))))

(defun screenshot-frame ()
    "Take a screenshot of 400x200 pixels of the Emacs frame."
      (interactive)
      (shell-command-to-string
       (concat "sleep 1; "
	       "import -window 0x2e00006 "
	       "-crop 400x200+13+0 +repage ~/frames/`date +%s`.png")))

(defun surround(tag)
  "Surround word within TAG.
  TAG can be <>," ",' ',[ ], etc."
  (interactive "sWord should be inside: ")
  (backward-word)
  (mark-word)
  (when (region-active-p)
    (kill-region (region-beginning) (region-end)))
  (insert tag)
  (backward-char)
  (yank))

(provide 'emacs_general_config)

