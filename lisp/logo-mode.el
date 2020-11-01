
(defvar logo-keywords
  '("stop" "op" "output"))

;;;###autoload
(define-derived-mode logo-mode fundamental-mode "logo"
  "Major mode for editing Logo source code"
  (setq font-lock-defaults `(((";+.*$" . font-lock-comment-face)
			      ("\"[^ \t\n]*" . font-lock-constant-face)
			      (":[a-zA-Z0-9.]+" . font-lock-variable-name-face)
			      ("^[ \t]*\\(to\\|\\.macro\\|end\\)" . font-lock-keyword-face)
			      ( ,(regexp-opt logo-keywords 'words) . font-lock-keyword-face))
			     t))
  (setq comment-start ";;")
  (setq comment-end ""))

(provide 'logo-mode)
