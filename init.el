(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)
(blink-cursor-mode 0)

(when (eq window-system 'w32)
  ;; start fullscreen
  ;;(toggle-frame-fullscreen)
  ;; maximized when not fullscreen
  (toggle-frame-maximized))

(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(setq  user-full-name "Tyler Lee"
       user-mail-address "wtleeiv@gmail.com")

(setq default-directory "~"
      backup-directory-alist '(("" . "~/.emacs.d/backup"))
      auto-save-file-name-transforms `((".*"  "~/.emacs.d/backup/" t)))

(setq vc-follow-symlinks t
      require-final-newline t
      inhibit-startup-message t
      ring-bell-function 'ignore
      initial-scratch-message nil
      sentence-end-double-space nil
      uniquify-buffer-name-style 'post-forward)

(require 'paren)
(show-paren-mode t)
(setq show-paren-delay 0.0)

(global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "M-1") 'delete-other-windows)
(global-set-key (kbd "M-2") 'split-window-below)
(global-set-key (kbd "M-3") 'split-window-right)
(global-set-key (kbd "M-0") 'delete-window)
;; move windows with S-<arrow>
(windmove-default-keybindings)

(if (eq window-system 'x)
	(set-face-attribute 'default nil :family "Source Code Pro" :height 110)
	(set-face-attribute 'default nil :family "Siddhanta Mono" :height 120))

(global-linum-mode t)
(global-hl-line-mode t)
;; syntax highlighting
(global-font-lock-mode t)
;; update buffer when file changes on disk
(global-auto-revert-mode t)
(setq-default dired-listing-switches "-alh")
(fset 'yes-or-no-p 'y-or-n-p)
(add-hook 'before-save-hook
	  'delete-trailing-whitespace)
(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-verbose t
      use-package-always-ensure t)

(use-package noctilux-theme
  :config
  (load-theme 'noctilux t))
(set-cursor-color "#ccaaff")
(set-face-background 'show-paren-match (face-background 'default))
(set-face-attribute 'show-paren-match nil
		    :foreground "#ccaaff"
		    :weight 'bold
		    :underline t)

(unless (package-installed-p 'counsel)
  (package-install 'counsel))
(use-package ivy
  :config
  (setq ivy-use-virtual-buffers t
        ivy-count-format "(%d/%d) ")
  (ivy-mode 1)
  (counsel-mode 1))
(global-set-key (kbd "C-s") 'swiper)
(global-set-key (kbd "C-c f") 'counsel-rg)

(use-package magit
  :defer t
  :bind ("C-c g" . magit-status))

;; autocomplete
(use-package company)
(add-hook 'after-init-hook 'global-company-mode)
(define-key company-active-map (kbd "C-n") 'company-select-next-or-abort)
(define-key company-active-map (kbd "C-p") 'company-select-previous-or-abort)

;; syntax checking
(use-package flycheck
  :defer t
  :config (global-flycheck-mode))

(require 'org)
(require 'org-habit)
(require 'org-agenda)
(setq org-directory "~/org/"
      org-default-notes-file (concat org-directory "notes.org")
      org-agenda-files (list (concat org-directory "agenda.org")
			     org-default-notes-file)
      org-src-fontify-natively t
      org-src-tab-acts-natively t
      org-confirm-babel-evaluate nil
      org-refile-targets '((org-agenda-files . (:level . 1)))
      org-agenda-custom-commands '(("n" "Agenda and TODOs"
				    ((agenda "")
				     (tags-todo "-habits")))))
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c l") 'org-store-link)



;;; lisp

(if (eq window-system 'x)
    (load "~/.roswell/helper.el")
  (progn
    (setq inferior-lisp-program "ros run")
    (require 'sly-autoloads)))
;; paredit
(require 'paredit)
(add-hook 'emacs-lisp-mode-hook 'paredit-mode)
(add-hook 'sly-mrepl-mode-hook 'paredit-mode)
(add-hook 'lisp-mode-hook 'paredit-mode)
;; echo arg list
(require 'eldoc)
(eldoc-add-command
 'paredit-backward-delete
 'paredit-close-round)

;;; python

(use-package company-anaconda
  :defer t
  :config
  (add-to-list 'company-backends 'company-anaconda)
  (add-hook 'python-mode-hook 'anaconda-mode))

(use-package ob-ipython
  :defer t
  :config
  (setf python-shell-interpreter "ipython"
	python-shell-interpreter-args "-i --simple-prompt"))

;;; c

;;; cpp

;;; eshell

(defalias 'open 'find-file)
(defalias 'openo 'find-file-other-window)

;;; latex

(use-package tex-site
  :ensure auctex
  :defer t
  :config
  ;; enable document parsing
  (setq TeX-auto-save t
	TeX-parse-self t)
  ;; multi file documents (or \include \input)
  (setq-default TeX-master nil))

(use-package latex-extra
  :after tex-site
  :defer t
  :config
  (add-hook 'LaTeX/P-mode-hook 'latex-extra-mode))

(use-package company-auctex
  :after tex-site
  :defer t
  :config
  (company-auctex-init))

;;; common

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (ipython . t)
   (lisp . t)))

;;; linux

(when (eq window-system 'x)
  (use-package pdf-tools
    :defer t
    :config
    (pdf-tools-install)
    (setq pdf-view-midnight-colors '("#ffffff" . "#000000"))))

;;; windows

(when (eq window-system 'w32)
  (setq explicit-shell-file-name "c:/cygwin64/bin/bash.exe")
  (defalias 'gcc 'gcc.exe))
