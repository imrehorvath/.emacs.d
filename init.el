;; Turn off mouse interface early in startup to avoid momentary display
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

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

;; Add melpa repo
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)

(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

;; Install a package on-demand.
(defun require-package (package)
  "Install a package on-demand."
  (if (package-installed-p package)
      t
    (unless (assoc package package-archive-contents)
       (package-refresh-contents))
    (package-install package)))

;; Setup environment variables from the user's shell.
(when is-mac
  (require-package 'exec-path-from-shell)
  (exec-path-from-shell-initialize))

;; The list of my packages to be installed
(setq my-package-list '(paredit
			smartparens
			expand-region
			multiple-cursors
			js2-mode
			js2-refactor
			restclient
			magit))

(defun packages-install (packages)
  "Install the packages listed."
  (dolist (package packages)
    (unless (package-installed-p package)
      (package-install package)))
  (delete-other-windows))

(condition-case nil
    (packages-install my-package-list)
  (error
   (package-refresh-contents)
   (packages-install my-package-list)))

;; I use the UCB Scheme with the Simply Scheme extension.
(setq scheme-program-name "stk-simply")

(defun run-scheme-below ()
  "Run scheme below the current buffer."
  (interactive)
  (split-window-below)
  (other-window 1)
  (call-interactively 'run-scheme))

(global-set-key (kbd "C-c M-s") 'run-scheme-below)

;; I use paredit for emacs-lisp and scheme
(add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)
(add-hook 'scheme-mode-hook 'enable-paredit-mode)

;; Setup smartparens for the listed modes
(require 'smartparens-config)
(setq sp-autoescape-string-quote nil)
(dolist (mode '(css-mode-hook
		restclient-mode-hook
		js2-mode-hook
		markdown-mode))
  (add-hook mode 'turn-on-smartparens-mode))

;; I use the javascript-IDE for javascript
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

;; Setup multiple cursors
(require 'multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;; Setup expand region
(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)

;; Setup js2-refactor
(require 'js2-refactor)
(js2r-add-keybindings-with-prefix "C-c C-m")

