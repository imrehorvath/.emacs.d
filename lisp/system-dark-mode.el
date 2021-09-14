;; system-dark-mode.el

(defvar preferred-dark-theme nil)
(defvar preferred-light-theme nil)

(defvar dark-or-light-state nil)

(defun system-dark-mode-enabled-p ()
  "Checks if the system is in dark mode.

When the system-specific check for dark mode has been implemented by
this function, returns the result of this check.
Returns nil when the check is not implemented."
  (cond ((eq system-type 'darwin)
	 (string= "Dark"
		  (string-trim (shell-command-to-string
				"defaults read -g AppleInterfaceStyle"))))
	;; TODO: Add check for dark mode on other systems too!
	(t nil)))

(defun match-system-dark-mode ()
  "Check if the system is in dark mode, and set themes accordingly."
  (interactive)
  (unless (and preferred-dark-theme
	       preferred-light-theme)
    (error "preferred-dark-theme and preferred-light-theme variables needs to be set prior calling function match-system-dark-mode"))
  (cond ((eq dark-or-light-state 'light)
	 (when (system-dark-mode-enabled-p)
	   (disable-theme preferred-light-theme)
	   (load-theme preferred-dark-theme t)
	   (setq dark-or-light-state 'dark)))
	((eq dark-or-light-state 'dark)
	 (unless (system-dark-mode-enabled-p)
	   (disable-theme preferred-dark-theme)
	   (load-theme preferred-light-theme t)
	   (setq dark-or-light-state 'light)))
	((not dark-or-light-state)
	 (if (system-dark-mode-enabled-p)
	     (progn (load-theme preferred-dark-theme t)
		    (setq dark-or-light-state 'dark))
	   (load-theme preferred-light-theme t)
	   (setq dark-or-light-state 'light)))
	(t (error "dark-or-light-state is invalid: %s" dark-or-light-state))))

(provide 'system-dark-mode)
