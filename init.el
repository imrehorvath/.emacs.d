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

(require 'package)
;; Setup and init packages
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/")))
(package-initialize)
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

;; Use STKlos Scheme
(setq scheme-program-name "stklos --no-line-editor")

;; Use GNU Smalltalk
(push '("\\.st\\'" . smalltalk-mode) auto-mode-alist)
(autoload 'smalltalk-mode "/usr/local/share/emacs/site-lisp/gnu-smalltalk/smalltalk-mode.el" "" t)
(autoload 'gst "/usr/local/share/emacs/site-lisp/gnu-smalltalk/gst-mode.el" "" t)
