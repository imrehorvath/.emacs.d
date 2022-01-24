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

(defvar sysdm--last-enabled-theme nil
  "Variable to store the name of the theme last enabled by this library.")

(defun sysdm--dark-mode-enabled-p ()
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

(defun sysdm--switch-to-theme (theme)
  "Switch to the given theme."
  (unless (eq sysdm--last-enabled-theme theme)
    (if sysdm--last-enabled-theme
	(disable-theme sysdm--last-enabled-theme))
    (if theme
	(if (custom-theme-p theme)
	    (enable-theme theme)
	  (load-theme theme t)))
    (setq sysdm--last-enabled-theme theme)))

(defun sysdm-match-system-dark-mode ()
  "Match the system dark mode settings with preferred emacs themes.

Enables/disables themes in emacs to match the system-wide dark mode settings."
  (interactive)
  (if (sysdm--dark-mode-enabled-p)
      (sysdm--switch-to-theme sysdm-dark-theme)
    (sysdm--switch-to-theme sysdm-light-theme)))

(provide 'sysdm)

;;; sysdm.el ends here
