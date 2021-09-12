;; Turn off mouse interface early in startup to avoid momentary display
(dolist (mode '(menu-bar-mode tool-bar-mode scroll-bar-mode))
  (when (fboundp mode) (funcall mode -1)))

;; No splash screen please
(setq inhibit-startup-message t)

;; Keep emacs Custom-settings in separate file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

(defvar dark-mode-theme 'wheatgrass)
(defvar light-mode-theme 'whiteboard)

(defun dark-mode-enabled-p ()
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

(defun match-system-dark-mode-setting ()
  "Check if the system is in dark mode, and set themes accordingly."
  (interactive)
  (if (dark-mode-enabled-p)
      (progn (disable-theme light-mode-theme)
	     (load-theme dark-mode-theme t))
    (disable-theme dark-mode-theme)
    (load-theme light-mode-theme t)))

;; Match system dark mode setting at startup
(match-system-dark-mode-setting)

;; Setup appearances
(when window-system
  (setq frame-title-format '(buffer-file-name "%f" ("%b")))
  (tooltip-mode -1)
  (blink-cursor-mode -1))

;; Use visible bell
(setq visible-bell t)

;; Enable buffer size indication
(size-indication-mode)

;; Highlight matching parentheses when the point is on them.
(show-paren-mode 1)

;; Save point position between sessions
(if (version< emacs-version "25.1")
    (progn
      (require 'saveplace)
      (setq-default save-place t))
  (save-place-mode 1))

;; Enable more meaningful buffer names
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; Comment/uncomment this line to enable MELPA Stable if desired.  See `package-archive-priorities`
;; and `package-pinned-packages`. Most users will not need or want to do this.
;;(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)

;; Package archive initialization
(unless package-archive-contents
  (package-refresh-contents))

(defun install-package-if-not-installed (pkg)
  "Install package if not installed already"
  (unless (package-installed-p pkg)
    (package-install pkg)))

;; On macOS setup the path from the user shell
(when (eq system-type 'darwin)
  (install-package-if-not-installed 'exec-path-from-shell)
  (exec-path-from-shell-initialize))

;; Install packages if not present
(dolist (pkg '(company
	       flx
	       flx-ido
	       json-mode
	       magit
	       paredit
	       ztree))
  (install-package-if-not-installed pkg))

;; Use flx-ido
(require 'flx-ido)
(ido-mode 1)
(ido-everywhere 1)
(flx-ido-mode 1)
;; disable ido faces to see flx highlights.
(setq ido-enable-flex-matching t)
(setq ido-use-faces nil)

;; Use company globally
(require 'company)
(add-hook 'after-init-hook 'global-company-mode)

;; Enable paredit for the following major modes
(add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)
(add-hook 'scheme-mode-hook 'enable-paredit-mode)

;; Use Guile scheme
(setq scheme-program-name "guile --no-auto-compile")

;; Associate Arduino sketch files with c++-mode
(add-to-list 'auto-mode-alist '("\\.ino\\'" . c++-mode))

;; Add local lisp directory to load-path
(add-to-list 'load-path (concat user-emacs-directory
				(convert-standard-filename "lisp/")))

;; Logo mode
(require 'logo-mode)

;; Setup dired
(setq dired-dwim-target t)
(setq wdired-allow-to-change-permissions t)
