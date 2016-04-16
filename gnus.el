;; gnus.el

(setq user-mail-address "iclcoolster@gmail.com")
(setq user-full-name "Sachin")


;; --------------------------------------------------
;; for GMail
(setq-default gnus-select-method
	      '(nnimap "gmail"
		       (nnimap-address "imap.gmail.com")
		       (nnimap-server-port 993)
		       (nnimap-stream ssl)))

;; original-port: 587
(setq-default message-send-mail-function 'smtpmail-send-it
	      smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
	      smtpmail-auth-credentials '(("smtp.gmail.com" 587
					   "iclcoolster@gmail.com" nil))
	      smtpmail-default-smtp-server "smtp.gmail.com"
	      smtpmail-smtp-server "smtp.gmail.com"
	      smtpmail-smtp-service 587
	      gnus-ignored-newsgroups "^to\\.\\|^[0-9. ]+\\( \\|$\\)\\|^[\"]\"[#'()]")
;; --------------------------------------------------
