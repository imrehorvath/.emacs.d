;; Turn off mouse interface early in startup to avoid momentary display
(dolist (mode '(menu-bar-mode tool-bar-mode scroll-bar-mode))
  (when (fboundp mode) (funcall mode -1)))

;; No splash screen please
(setq inhibit-startup-message t)

;; Keep emacs Custom-settings in separate file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

;; Use visible bell
(setq visible-bell t)

;; Setup dired
(setq dired-dwim-target t)

;; Setup appearances
(when window-system
  (setq frame-title-format '(buffer-file-name "%f" ("%b")))
  (tooltip-mode -1)
  (blink-cursor-mode -1))

;; Enable more meaningful buffer names
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; Save point position between sessions
(if (version< emacs-version "25.1")
    (progn
      (require 'saveplace)
      (setq-default save-place t))
  (save-place-mode 1))

;; Highlight matching parentheses when the point is on them.
(show-paren-mode 1)

;; Are we on a mac?
(defconst is-mac (equal system-type 'darwin)
  "Boolean indicating if we are on a mac")

;; GnuTLS configuration
(setq gnutls-verify-error t)
(setq gnutls-min-prime-bits 1024)
(setq gnutls-algorithm-priority "SECURE128:-VERS-SSL3.0:-VERS-TLS1.3")

(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (when no-ssl
    (warn "\
Your version of Emacs does not support SSL connections,
which is unsafe because it allows man-in-the-middle attacks.
There are two things you can do about this warning:
1. Install an Emacs version that does support SSL and be safe.
2. Remove this warning from your init file so you won't see it again."))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives (cons "gnu" (concat proto "://elpa.gnu.org/packages/")))))
(package-initialize)

;; Package archive initialization
(unless package-archive-contents
  (package-refresh-contents))

(defun install-package-if-not-installed (pkg)
  "Install package if not installed already"
  (unless (package-installed-p pkg)
    (package-install pkg)))

;; Setup environment variables from the user's shell.
(when is-mac
  (install-package-if-not-installed 'exec-path-from-shell)
  (exec-path-from-shell-initialize))

;; Install packages if not present
(dolist (pkg '(company
	       flx
	       flx-ido
	       paredit))
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
