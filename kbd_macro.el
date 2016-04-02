;;; kbd_macro.el
;; This is where I define my keyboard macros.

(defun switch_buffer ()
  "switch to next window"
  (interactive)
    (other-window 1)
    )
;; set <F7> key for killing buffer
(global-set-key (kbd "<f7>") 'switch_buffer)

(defun kill_buffer ()
   "kill THIS buffer"
   (interactive)
   (kill-buffer (buffer-name))
   )
;; set <F8> key for killing buffer
(global-set-key (kbd "<f8>") 'kill_buffer)
;; set <F9> key for delete present window
(global-set-key (kbd "<f9>") 'delete-window)
;; set <F10> key for delete all other windows except THIS
(global-set-key (kbd "<f6>") 'delete-other-windows)

(global-set-key (kbd "RET") 'newline-and-indent)

;; Scaling
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)

;; cycle emacs-buffer
;; (global-set-key [(f9)]        'cycle-buffer-backward)
;; (global-set-key [(f10)]       'cycle-buffer)
;; (global-set-key [(shift f9)]  'cycle-buffer-backward-permissive)
;; (global-set-key [(shift f10)] 'cycle-buffer-permissive)

;; keybinding for Org-mode
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

;; (global-set-key "\C-xs" 'eshell)

;; re-visit a file in current buffer when it is changed by an external program.
(global-set-key "\C-cr" 'revert-buffer)

;; Manage window size
(global-set-key (kbd "<C-up>") 'enlarge-window)
(global-set-key (kbd "<C-down>") 'shrink-window)
(global-set-key (kbd "<C-left>") 'shrink-window-horizontally)
(global-set-key (kbd "<C-right>") 'enlarge-window-horizontally)

;; fold-this
(global-set-key (kbd "C-c C-f") 'fold-this-all)
(global-set-key (kbd "C-c C-F") 'fold-this)
(global-set-key (kbd "C-c M-f") 'fold-this-unfold-all)

;; Hide DOT files
(global-set-key (kbd "M-o") 'dired-omit-mode)

(provide 'kbd_macro)
