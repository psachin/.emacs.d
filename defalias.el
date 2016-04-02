;; Default aliases

(defalias 'rs 'replace-string)
(defalias 'eb 'eval-buffer)
(defalias 'er 'eval-region)
(defalias 'lf 'load-file)
(defalias 'cg 'customize-group)

(defalias 'ms 'magit-status)
(defalias 'log 'magit-log)

;; For easier access to regex search/replace.
(defalias 'qrr 'query-replace-regexp)

;; comment-region
(defalias 'cor 'comment-region)

(defalias 'ur 'uncomment-region)

(provide 'defalias)
