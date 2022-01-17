;; sysdm.el

(defvar sysdm-dark-theme nil)
(defvar sysdm-light-theme nil)

(defun sysdm-dark-mode-enabled-p ()
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

(defun sysdm-match-system-dark-mode ()
  "Match the system dark mode settings with preferred emacs themes.

Enables/disables themes in emacs to match the system-wide dark mode settings."
  (interactive)
  (dolist (theme custom-enabled-themes) (disable-theme theme))
  (if (sysdm-dark-mode-enabled-p)
      (if sysdm-dark-theme
	  (load-theme sysdm-dark-theme t))
    (if sysdm-light-theme
	(load-theme sysdm-light-theme t))))

(provide 'sysdm)
