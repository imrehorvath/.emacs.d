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

;; Setup appearances
(when window-system
  (setq frame-title-format '(buffer-file-name "%f" ("%b")))
  (tooltip-mode -1)
  (blink-cursor-mode -1))

;; Use ido
(ido-mode t)
(setq ido-enable-flex-matching t)

;; Enable more meaningful buffer names
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; Save point position between sessions
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file (expand-file-name ".places" user-emacs-directory))

;; Use ibuffer
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; Highlight matching parentheses when the point is on them.
(show-paren-mode 1)

;; Are we on a mac?
(setq is-mac (equal system-type 'darwin))

(require 'package)
;; Setup and init packages
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(defun install-package-if-not-installed (pkg)
  "Install package if not installed already"
  (when (not (package-installed-p pkg))
    (package-install pkg)))

;; Setup environment variables from the user's shell.
(when is-mac
  (install-package-if-not-installed 'exec-path-from-shell)
  (exec-path-from-shell-initialize))

;; Install packages if not present
(dolist (pkg '(paredit))
  (install-package-if-not-installed pkg))

;; Setup Scheme
(setq scheme-program-name "stk-simply")
(defun run-scheme-below ()
  "Run scheme below the current buffer."
  (interactive)
  (split-window-below)
  (other-window 1)
  (call-interactively 'run-scheme))
(global-set-key (kbd "C-c M-s") 'run-scheme-below)

;; Enable paredit for the following major modes
(add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)
(add-hook 'scheme-mode-hook 'enable-paredit-mode)
