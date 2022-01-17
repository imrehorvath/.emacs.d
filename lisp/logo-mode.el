;;; logo-mode.el --- Logo language support  -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Imre Horvath

;; Author: Imre Horvath <imi.horvath@gmail.com>
;; Created: 17 Jan 2022
;; Keywords: languages

;;; Code:

(defvar logo-mode--logo-keywords
  '("stop" "op" "output" "throw" "catch"))

(define-derived-mode logo-mode fundamental-mode "logo"
  "Major mode for editing Logo source code"
  (setq font-lock-defaults `(((";+.*$" . font-lock-comment-face)
			      ("\"[\\]." . font-lock-constant-face)
			      ("\"\\(\\w\\|[.,`\"_=?@#*]\\)*" . font-lock-constant-face)
			      (":\\(\\w\\|[.,`_=?@#*]\\)*" . font-lock-variable-name-face)
			      ("^[ \t]*\\(to\\|\\.macro\\)[ \t]+" . font-lock-keyword-face)
			      ("^[ \t]*end\\($\\|[ \t]+\\)" . font-lock-keyword-face)
			      ( ,(regexp-opt logo-mode--logo-keywords 'words) . font-lock-keyword-face))
			     t))
  (setq comment-start ";;")
  (setq comment-end ""))

(provide 'logo-mode)

;;; logo-mode.el ends here
