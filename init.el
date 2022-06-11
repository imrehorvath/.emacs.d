;;; init.el

;; Early setup of general UI configuration
(dolist (mode '(menu-bar-mode tool-bar-mode scroll-bar-mode))
  (when (fboundp mode) (funcall mode -1)))

(setq inhibit-startup-message t)

;; Launch GUI app with max window size
(add-to-list 'initial-frame-alist '(fullscreen . maximized))

;; When emacs runs as GUI app and not in terminal
(when window-system
  (setq frame-title-format '(buffer-file-name "%f" ("%b")))
  (tooltip-mode -1)
  (blink-cursor-mode -1))

;; Add user-local directory of elisp files to the load-path
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; Store custom settings in a separate file
(setq custom-file (locate-user-emacs-file "custom.el"))

;; Setup mode line
(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)

;; Don't ring bell please
(setq ring-bell-function 'ignore)

;; Show matching parens everywhere
(show-paren-mode 1)

;; Use saveplace, to remember point location
(if (version< emacs-version "25.1")
    (progn
      (require 'saveplace)
      (setq-default save-place t))
  (save-place-mode 1))

;; Treat Arduino sketch files as c++ source
(add-to-list 'auto-mode-alist '("\\.ino\\'" . c++-mode))

;; Packages setup and use use-package
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :ensure t
  :config
  (exec-path-from-shell-initialize))

(use-package dracula-theme
  :ensure t)

(use-package sysdm
  :config
  (setq sysdm-dark-theme 'dracula)
  (setq sysdm-light-theme 'adwaita)
  (global-set-key (kbd "C-S-m") 'sysdm-match-system-dark-mode)
  (sysdm-match-system-dark-mode))

(use-package flx
  :ensure t)

(use-package uniquify
  :config
  (setq uniquify-buffer-name-style 'forward))

(use-package dired
  :config
  (setq dired-recursive-deletes 'always)
  (setq dired-recursive-copies 'always)
  (setq dired-dwim-target t)
  (setq wdired-allow-to-change-permissions t))

(use-package company
  :ensure t
  :config
  (setq company-idle-delay 0.5)
  (global-company-mode t))

(use-package counsel
  :ensure t
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  (setq ivy-re-builders-alist
	'((t . ivy--regex-ignore-order)))
  (global-set-key (kbd "C-s") 'swiper-isearch)
  (global-set-key (kbd "M-x") 'counsel-M-x)
  (global-set-key (kbd "C-x C-f") 'counsel-find-file)
  (global-set-key (kbd "M-y") 'counsel-yank-pop)
  (global-set-key (kbd "<f1> f") 'counsel-describe-function)
  (global-set-key (kbd "<f1> v") 'counsel-describe-variable)
  (global-set-key (kbd "<f1> l") 'counsel-find-library)
  (global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
  (global-set-key (kbd "<f2> u") 'counsel-unicode-char)
  (global-set-key (kbd "<f2> j") 'counsel-set-variable)
  (global-set-key (kbd "C-x b") 'ivy-switch-buffer)
  (global-set-key (kbd "C-c v") 'ivy-push-view)
  (global-set-key (kbd "C-c V") 'ivy-pop-view)
  (global-set-key (kbd "C-c C-r") 'ivy-resume)
  (global-set-key (kbd "C-t") 'counsel-company))

(use-package paredit
  :ensure t
  :config
  (add-hook 'emacs-lisp-mode-hook #'paredit-mode)
  (add-hook 'scheme-mode-hook #'paredit-mode))

(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status)))

(use-package ztree
  :ensure t)

(use-package json-mode
  :ensure t)

(use-package swift-mode
  :ensure t)

(use-package scheme
  :config
  (setq scheme-program-name "guile --no-auto-compile"))

(use-package logo-mode
  :commands logo-mode)

;; Enable access from emacsclient
(add-hook 'after-init-hook (lambda ()
			     (require 'server)
			     (unless (server-running-p)
			       (server-start))))

;; Load configuration, set via the customize interface, if exists
(when (file-exists-p custom-file)
  (load custom-file))
