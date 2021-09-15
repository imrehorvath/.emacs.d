;; system-dark-mode.el

(defvar preferred-dark-theme nil)
(defvar preferred-light-theme nil)

(defvar dark-mode-state-initialized nil)
(defvar emacs-in-dark-mode nil)

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
  "Match the system dark mode settings with preferred emacs themes.

Enables/disables themes in emacs to match the system-wide dark mode settings."
  (interactive)
  (let ((system-in-dark-mode (system-dark-mode-enabled-p)))
    (when (or (not dark-mode-state-initialized)
	      (not (eq emacs-in-dark-mode system-in-dark-mode)))
      (let ((theme-to-use (if system-in-dark-mode
			      preferred-dark-theme
			    preferred-light-theme)))
	(when theme-to-use
	  (dolist (theme custom-enabled-themes) (disable-theme theme))
	  (load-theme theme-to-use t)))
      (setq emacs-in-dark-mode system-in-dark-mode
	    dark-mode-state-initialized t))))

(provide 'system-dark-mode)
