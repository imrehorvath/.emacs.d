;;; sysdm.el --- System Dark Mode support  -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Imre Horvath

;; Author: Imre Horvath <imi.horvath@gmail.com>
;; Created: 17 Jan 2022
;; Keywords: faces

;;; Code:

(defgroup sysdm nil
  "Make Emacs use the user-preferred light/dark themes, based on the system settings."
  :prefix "sysdm-"
  :group 'customize)

(defcustom sysdm-dark-theme nil
  "User-preferred dark theme."
  :type '(symbol :tag "Dark theme" nil))

(defcustom sysdm-light-theme nil
  "User-preferred light theme."
  :type '(symbol :tag "Light theme" nil))

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
  (if (sysdm-dark-mode-enabled-p)
      (when sysdm-dark-theme
	(if (and sysdm-light-theme
		 (memq sysdm-light-theme custom-enabled-themes))
	    (disable-theme sysdm-light-theme))
	(if (custom-theme-p sysdm-dark-theme)
	    (enable-theme sysdm-dark-theme)
	  (load-theme sysdm-dark-theme t)))
    (when sysdm-light-theme
      (if (and sysdm-dark-theme
	       (memq sysdm-dark-theme custom-enabled-themes))
	  (disable-theme sysdm-dark-theme))
      (if (custom-theme-p sysdm-light-theme)
	  (enable-theme sysdm-light-theme)
	(load-theme sysdm-light-theme t)))))

(provide 'sysdm)

;;; sysdm.el ends here
