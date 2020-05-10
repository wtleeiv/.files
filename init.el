;;; init.el --- emacs config  -*- lexical-binding: t; coding:utf-8 -*-

;;; Commentary:

;; A bare-boned config template. Use "outshine-cycle-buffer" (<Tab> and <S-Tab>
;; in org style) to navigate through sections, and "imenu" to locate individual
;; use-package definition.

;;; Bootstrap

;; Speed up startup
(setq gc-cons-threshold 400000000
      gc-cons-percentage 0.6)
(add-hook 'after-init-hook
          `(lambda ()
             (setq gc-cons-threshold 800000
                   gc-cons-percentage 0.1)
             (garbage-collect))
          t)

;;;; Turn off mouse interface early in startup to avoid momentary display

(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; Swap C-x and C-t
;; (keyboard-translate ?\C-t ?\C-x)
;; (keyboard-translate ?\C-x ?\C-t)

;;;; Setup modifier keys

(when (eq window-system 'ns) ; only on mac
  (setq ns-command-modifier 'control
        ns-option-modifier 'super
        ns-control-modifier 'meta))

;;;; Don't add custom section to init.el

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file :noerror))

;;;; Defaults

(setq-default  user-full-name "Tyler Lee"
	       user-mail-address "wtleeiv@gmail.com"
	       default-directory "~"
               backup-directory-alist '(("" . "~/.emacs.d/backup"))
	       auto-save-file-name-transforms `((".*"  "~/.emacs.d/backup/" t))
	       indent-tabs-mode nil
               vc-follow-symlinks t
	       shift-select-mode nil
               require-final-newline t
	       inhibit-startup-message t
	       ring-bell-function 'ignore
	       confirm-kill-processes nil
	       initial-scratch-message nil
	       sentence-end-double-space nil
               dired-listing-switches "-alh"
	       buffer-file-coding-system 'utf-8-unix
	       uniquify-buffer-name-style 'post-forward
               ediff-split-window-function 'split-window-horizontally
               ediff-window-setup-function 'ediff-setup-windows-plain)

(require 'paren)
(show-paren-mode t)
(setq show-paren-delay 0)

(blink-cursor-mode 0)
;; (global-hl-line-mode t)

(delete-selection-mode t)

;; syntax highlighting
(global-font-lock-mode t)
;; update buffer when file changes on disk
(global-auto-revert-mode t)
;; window undo/redo C-c <left/right>
(winner-mode 1)

(fset 'yes-or-no-p 'y-or-n-p)

(add-hook 'before-save-hook
	  'delete-trailing-whitespace)

(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "C-c i") 'imenu)
(global-set-key (kbd "M-z") 'zap-up-to-char)

(global-set-key (kbd "<f5>") (lambda () (interactive) (split-window-right) (other-window 1)))
(global-set-key (kbd "<f6>") (lambda () (interactive) (split-window-below) (other-window 1)))
(global-set-key (kbd "<f7>") 'delete-other-windows)
(global-set-key (kbd "C-<f7>") 'delete-window)

(global-set-key (kbd "<f8>") 'previous-buffer)
(global-set-key (kbd "<f9>") 'next-buffer)

;;; GUI

(when (window-system)

;;;; Font

  (when (eq window-system 'ns)
    (toggle-frame-maximized)
    (add-to-list 'default-frame-alist
		 '(font . "Dank Mono-13:antialias=subpixel")))

;;;; Transparency

  (set-frame-parameter (selected-frame) 'alpha '(77 . 50))
  (add-to-list 'default-frame-alist '(alpha . (77 . 50)))

  (defun my/toggle-transparency ()
    (interactive)
    (let ((alpha (frame-parameter nil 'alpha)))
      (set-frame-parameter
       nil 'alpha
       (if (eql (cond ((numberp alpha) alpha)
		      ((numberp (cdr alpha)) (cdr alpha))
		      ;; Also handle undocumented (<active> <inactive>) form.
		      ((numberp (cadr alpha)) (cadr alpha)))
		100)
	   '(77 . 50) '(100 . 100)))))

  (global-set-key (kbd "C-c t") 'my/toggle-transparency))

;;; Packages

;; Initialize package.el
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

;; Bootstrap `use-package'
(setq-default use-package-always-ensure t ; Auto-download package if not exists
              use-package-always-defer t ; Always defer load package to speed up startup
              use-package-verbose nil ; Don't report loading details
              use-package-expand-minimally t ; make the expanded code as minimal as possible
              use-package-enable-imenu-support t) ; Let imenu finds use-package definitions
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))

(use-package diminish
  :demand t)

;;;; Theme

(when (window-system)
  (use-package modus-vivendi-theme
    :demand t
    :init
    (setq modus-vivendi-theme-slanted-constructs t
          modus-vivendi-theme-subtle-diffs t
          modus-vivendi-theme-distinct-org-blocks t
          modus-vivendi-theme-rainbow-headings t)
    :config
    (load-theme 'modus-vivendi t)))

;;;; Helpful/Discoverability

(use-package which-key
  :demand t
  :diminish
  :config
  (setq which-key-idle-delay 0.01)
  (which-key-mode))

(use-package discover-my-major
  :bind
  ("C-h C-m" . discover-my-major)
  ("C-h M-m" . discover-my-mode))

(use-package helpful
  :bind
  ("C-h f" . helpful-callable)
  ("C-h v" . helpful-variable)
  ("C-h k" . helpful-key)
  ("C-c d" . helpful-at-point))

(use-package outshine
  ;; Easier navigation for source files, especially this one.
 :bind (:map outshine-mode-map
              ("<backtab>" . outshine-cycle-buffer))
  :hook (emacs-lisp-mode . outshine-mode))

(use-package keyfreq
  :demand t
  :config
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1))

;;;; General/Usability

(use-package ido
  :demand t
  :config
  (setq ido-enable-flex-matching t
        ido-use-virtual-buffers t
        ido-everywhere t)
  (ido-mode 1))

(use-package ido-completing-read+
  :demand t
  :after ido
  :config
  (ido-ubiquitous-mode 1))

(use-package ido-vertical-mode
  :demand t
  :after ido-completing-read+
  :config
  (ido-vertical-mode 1)
  (setq ido-vertical-define-keys 'C-n-and-C-p-only))

(use-package flx-ido
  :demand t
  :after ido-vertical-mode
  :config
  (flx-ido-mode 1))

(use-package smex
  :bind
  ("M-x" . smex)
  ("M-X" . smex-major-mode-commands)
  :config
  (smex-initialize))

;; (use-package undo-tree
;;   :bind
;;   ("C-x u" . undo-tree-visualize)
;;   :config
;;   (global-undo-tree-mode)
;;   (setq undo-tree-auto-save-history t
;;         undo-tree-history-directory-alist '((".*" . "~/.emacs.d/backup/"))))

(use-package ace-window
  :bind
  ("M-o" . ace-window)
  :config
  ;; o & u are part of keybindings (press ? to show)
  (setq aw-keys '(?h ?t ?n ?s ?a ?e ?i ?d)))

(use-package avy
  :bind
  ("C-." . avy-goto-char-timer)
  :config
  (setq  avy-timeout-seconds 0.2
         avy-keys '(?u ?h ?e ?t ?o ?n ?a ?s ?i ?d)))

(use-package ace-link
  :demand t
  :config
  (ace-link-setup-default))

(use-package perspective
  :demand t
  :config
  (persp-mode))

(when (eq window-system 'ns)
  (use-package exec-path-from-shell
    :demand t
    :config
    (add-to-list 'exec-path-from-shell-variables "PERL5LIB")
    (add-to-list 'exec-path-from-shell-variables "PERL_LOCAL_LIB_ROOT")
    (add-to-list 'exec-path-from-shell-variables "PERL_MB_OPT")
    (add-to-list 'exec-path-from-shell-variables "PERL_MM_OPT")
    (add-to-list 'exec-path-from-shell-variables "GTAGSCONF")
    (add-to-list 'exec-path-from-shell-variables "GTAGSLABEL")
    (exec-path-from-shell-initialize)))

;; (use-package restart-emacs
;;   :commands (restart-emacs))

;; (use-package symon
;;   :demand t
;;   :config
;;   (symon-mode 1))

(use-package dumb-jump
  :bind
  ("M-g ." . dumb-jump-go)
  ("M-g ," . dumb-jump-back))

(with-eval-after-load 'shr ; lazy load is very important, it can save you a lot of boot up time
  (require 'shrface)
  (shrface-basic) ; enable shrfaces, must be called before loading eww/dash-docs/nov.el
  (shrface-trial) ; enable shrface experimental face(s), must be called before loading eww/dash-docs/nov.el
  (setq shrface-href-versatile t) ; enable versatile URL faces support
                                        ; (http/https/ftp/file/mailto/other), if
                                        ; `shrface-href-versatile' is nil, default
                                        ; face `shrface-href-face' would be used.
  (setq shrface-toggle-bullets t) ; Set t if you do not like headline bullets

  ;; eww support
  (with-eval-after-load 'eww
    (add-hook 'eww-after-render-hook 'shrface-mode)))

(use-package smartparens
  :hook ((emacs-lisp-mode) . turn-on-smartparens-strict-mode)
  :bind
  (:map smartparens-mode-map
        ("C-M-f" . sp-forward-sexp)
        ("C-M-b" . sp-previous-sexp)

        ("C-M-n" . sp-next-sexp)
        ("C-M-p" . sp-backward-sexp)

        ("C-M-u" . sp-backward-up-sexp)
        ("C-M-d" . sp-down-sexp)

        ("C-M-a" . sp-backward-down-sexp)
        ("C-M-e" . sp-up-sexp)

        ("C-M-y" . sp-beginning-of-sexp)
        ("C-M-z" . sp-end-of-sexp)

        ("C-M-," . sp-forward-symbol)
        ("C-M-;" . sp-backward-symbol)

        ("C-M-k" . sp-kill-sexp)
        ("C-M-w" . sp-copy-sexp)
        ("C-M-t" . sp-transpose-sexp)

        ("C-M-SPC" . sp-mark-sexp)

        ("M-<delete>" . sp-unwrap-sexp)
        ("M-<backspace>" . sp-backward-unwrap-sexp)

        ("C-<right>" . sp-forward-slurp-sexp)
        ("C-<left>" . sp-forward-barf-sexp)

        ("C-M-<left>" . sp-backward-slurp-sexp)
        ("C-M-<right>" . sp-backward-barf-sexp)

        ("M-D" . sp-splice-sexp)
        ("C-M-<delete>" . sp-splice-sexp-killing-forward)
        ("C-M-<backspace>" . sp-splice-sexp-killing-backward)
        ("C-S-<backspace>" . sp-splice-sexp-killing-around)
        ("C-]" . sp-select-next-thing-exchange)
        ("C-M-]" . sp-select-next-thing))
  :config
  (require 'smartparens-config))

(use-package god-mode
  :bind
  ("s-b" . god-local-mode)
  :config
  (setq god-mode-alist '((nil . "C-M-")
                         ("g" . "M-"))))

;; (use-package lispy
;;   :hook emacs-lisp-mode)

(use-package projectile
  :ensure t
  :ensure ripgrep
  :bind
  ("C-c p" . projectile-command-map)
  :config
  (setq projectile-tags-command "uctags -Re --extras=+r -f \"%s\" %s \"%s\"")
  (projectile-mode 1))

(use-package magit
  :bind
  ("C-c g" . magit-status)
  :config
  (setq magit-no-confirm '(stage-all-changes unstage-all-changes)))

(use-package expand-region
  :bind
  ("C-q" . er/expand-region))

(use-package deadgrep
  :bind ("C-c s" . deadgrep))

(use-package restart-emacs
  :bind ("C-c r" . restart-emacs))

(use-package olivetti
  :bind ("C-c z" . olivetti-mode))

;;;; Languages

;;;;; Perl

(defun my/load-perly-sense ()
  (interactive)
  ;; *** PerlySense Config ***

  ;; ** PerlySense **
  ;; The PerlySense prefix key (unset only if needed, like for \C-o)
  (global-unset-key "\C-o")
  (setq ps/key-prefix "\C-o")

  ;; ** Flymake **
  ;; Load flymake if t
  ;; Flymake must be installed.
  ;; It is included in Emacs 22
  ;;     (or http://flymake.sourceforge.net/, put flymake.el in your load-path)
  (setq ps/load-flymake t)
  ;; Note: more flymake config below, after loading PerlySense

  ;; *** PerlySense load (don't touch) ***
  (setq ps/external-dir (shell-command-to-string "perly_sense external_dir"))
  (if (string-match "Devel.PerlySense.external" ps/external-dir)
      (progn
        (message
         "PerlySense elisp files  at (%s) according to perly_sense, loading..."
         ps/external-dir)
        (setq load-path (cons
                         (expand-file-name
                          (format "%s/%s" ps/external-dir "emacs"))
                         load-path))
        (load "perly-sense"))
    (message "Could not identify PerlySense install dir.
Is Devel::PerlySense installed properly?
Does 'perly_sense external_dir' give you a proper directory? (%s)" ps/external-dir)))

(defun my/indent-perl ()
  (interactive)
  (setq perl-indent-level 8
        perl-close-paren-offset -8
        perl-continued-statement-offset 8
        perl-indent-parens-as-block t
        perl-tab-always-indent t))

(defun my/load-sepia ()
  (interactive)
  (add-to-list 'load-path "~/.emacs.d/me/")
  (load "sepia"))

;;;; Org

(use-package org
  :bind
  ("C-c a" . org-agenda)
  ("C-c c" . org-capture)
  ("C-c l" . org-store-link)
  :config
  (require 'org)
  (require 'org-habit)
  (require 'org-agenda)
  (setq org-directory "~/org/"
        org-default-notes-file (concat org-directory "notes.org")
        org-agenda-files (list (concat org-directory "habits.org")
			  org-default-notes-file)
        org-src-fontify-natively t
        org-src-tab-acts-natively t
        org-confirm-babel-evaluate nil
        org-src-preserve-indentation t
        org-refile-targets '((org-agenda-files . (:level . 1)))
        org-agenda-custom-commands '(("n" "Agenda and TODOs"
          			    ((agenda "")
          			     (tags-todo "-habits"))))
        org-capture-templates `(("n" "Note" entry (file org-default-notes-file)
          		         "* %?")
          		        ("t" "Task" entry (file+headline org-default-notes-file "Tasks")
          		         "** TODO %?")
                                ("l" "Learnal" entry (file+olp+datetree ,(concat org-directory "learnal.org"))
                                 "* %?" :tree-type week))
        )

  ;; (org-babel-do-load-languages
  ;;  'org-babel-load-languages
  ;;  '((emacs-lisp . t)n
  ;;    (ipython . t)
  ;;    (lisp . t)))
  )


;;; Defuns

(defun my/tldr ()
  "TLDRs a query or region if any."
  (interactive)
  (browse-url
   (concat
    "https://tldr.ostera.io/"
    (if mark-active
        (buffer-substring (region-beginning) (region-end))
      (read-string "TLDR: ")))))

(defun my/edit-config ()
  (interactive)
  (find-file user-init-file))

(global-set-key (kbd "C-c e") 'my/edit-config)

;;; Super keys

(defun global-keys (key-alist)
  (mapc (lambda (key-pair)
          (global-set-key (kbd (car key-pair)) (cdr key-pair)))
        key-alist))

;; (global-keys '(("s-t" . next-line)
;;                ("s-c" . previous-line)))

;;; Wrap-up

(add-hook 'emacs-startup-hook #'(lambda () (message "Happy hackses :)")))

;; When config gets stable, using emacs server may be more convenient
;; (require 'server)
;; (unless (server-running-p)
;;   (server-start))
