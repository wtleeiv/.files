;;; Disable interface

(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))

;;; Graphical setup

(when window-system
  (load-theme 'deeper-blue t)
  ;; (load-theme 'tango-dark t)
  (set-face-attribute 'font-lock-comment-face 'nil :slant 'italic)

  (set-frame-parameter (selected-frame) 'alpha '(77 . 55))
  (add-to-list 'default-frame-alist '(alpha . (77 . 55)))

  ;; Maximize window if it isn't already
  (when (eq 'ns window-system)
    (unless (eq 'maximized (frame-parameter (selected-frame) 'fullscreen))
      (toggle-frame-maximized)))

  (when (eq 'ns window-system)
    (set-face-attribute 'default nil :family "Fira Code" :height 130)
    (set-face-attribute 'fixed-pitch nil :family "Fira Code" :height 1.0)
    (set-face-attribute 'fixed-pitch nil :family "ETBookOT" :height 1.2))
  (when (eq 'x window-system)
    (set-face-attribute 'default nil :family "Ubuntu Mono" :height 120)
    (set-face-attribute 'fixed-pitch nil :family "Ubuntu Mono" :height 1.0)
    (set-face-attribute 'variable-pitch nil :family "Source Sans Pro" :height 1.05))

  (defun my/toggle-transparency ()
    (interactive)
    (let ((alpha (frame-parameter nil 'alpha)))
      (set-frame-parameter
       nil 'alpha
       (if (eql (cond ((numberp alpha) alpha)
		      ((numberp (cdr alpha)) (cdr alpha))
		      ((numberp (cadr alpha)) (cadr alpha)))
		100)
	   '(77 . 55) '(100 . 100)))))

  (global-set-key (kbd "C-c t") 'my/toggle-transparency))

;;; Setup modifier keys

(when (eq 'ns window-system)
  (setq ns-command-modifier 'control
        ns-control-modifier 'meta
	ns-option-modifier 'meta))

;;; Defaults

(setq-default cursor-type 'bar
	      fill-column 77)

(setq user-full-name "Tyler Lee"
      user-mail-address "wtleeiv@gmail.com"

      default-directory "~/"
      backup-directory-alist '(("" . "~/.emacs.d/backup"))
      auto-save-file-name-transforms `((".*"  "~/.emacs.d/backup/" t))

      apropos-do-all t			; better apropos searching
      buffer-file-coding-system 'utf-8-unix
      confirm-kill-processes nil
      delete-by-moving-to-trash t
      dired-listing-switches "-alh"
      ediff-split-window-function 'split-window-horizontally
      ediff-window-setup-function 'ediff-setup-windows-plain
      help-window-select t	      ; focus help sindow when created
      inhibit-startup-message t
      initial-scratch-message nil
      require-final-newline t
      ring-bell-function 'ignore
      sentence-end-double-space nil
      set-mark-command-repeat-pop t
      shift-select-mode nil
      split-height-threshold 84		; disssuade horizontal split
      uniquify-buffer-name-style 'post-forward
      vc-follow-symlinks t)

;; Don't add custom section to init.el
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file :noerror))

;;; Modes

;; Completion
(setq completion-styles '(initials partial-completion flex))
(fido-mode 1)
;; Highlight matching pair
(show-paren-mode t)
(setq show-paren-delay 0)
;; Wraps with active region!
(electric-pair-mode 1)
;; Column number
(column-number-mode 1)
;; Display time
(setq display-time-day-and-date t
      display-time-default-load-average nil)
(display-time-mode 1)
;; Syntax highlighting
(global-font-lock-mode 1)

;; Update buffer when file changes on disk
(global-auto-revert-mode t)
;; Recursive editing
(setq enable-recursive-minibuffers t)
(minibuffer-depth-indicate-mode 1)
;; Change navigation
(global-highlight-changes-mode 1)
(setq highlight-changes-visibility-initial-state nil)
(global-set-key (kbd "C-c n") 'highlight-changes-next-change)
(global-set-key (kbd "C-c p") 'highlight-changes-previous-change)
;; Remember last place in visited files
(save-place-mode 1)
;; Select windows with S-<up/down/left/right>
(windmove-default-keybindings)
(setq windmove-wrap-around t)
;; Window undo/redo C-c <left/right>
(winner-mode 1)


(fset 'yes-or-no-p 'y-or-n-p)

(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'focus-out-hook 'garbage-collect)

;;; Keybindings

(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)

;; comment-dwim :: M-;
(global-set-key (kbd "M-c") 'capitalize-dwim)
(global-set-key (kbd "M-l") 'downcase-dwim)
(global-set-key (kbd "M-u") 'upcase-dwim)
;; Can be replaced
;; C-x C-l :: downcase-region
;; C-x C-u :: upcase-region

;; navigate read-only buffers w/o modifiers (C-x C-q)
(setq view-read-only t)
(define-key view-mode-map (kbd "s") 'isearch-forward-regexp)
(define-key view-mode-map (kbd "r") 'isearch-backward-regexp)


(global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "M-z") 'zap-up-to-char)

(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-c i") 'imenu)

(global-set-key (kbd "<f5>") (lambda ()
			       (interactive)
			       (split-window-right)
			       (other-window 1)))
(global-set-key (kbd "<f6>") (lambda ()
			       (interactive)
			       (split-window-below)
			       (other-window 1)))
(global-set-key (kbd "<f7>") 'delete-other-windows)
(global-set-key (kbd "C-<f7>") 'delete-window)

(global-set-key (kbd "<f8>") 'previous-buffer)
(global-set-key (kbd "<f9>") 'next-buffer)

;;; Functions

(defun my/edit-config ()
  (interactive)
  (find-file user-init-file))
(global-set-key (kbd "C-c c") 'my/edit-config)

(defun my/tldr ()
  "TLDRs a query or region if any."
  (interactive)
  (browse-url
   (concat
    "https://tldr.ostera.io/"
    (if mark-active
        (buffer-substring (region-beginning) (region-end))
      (read-string "TLDR: ")))))
(global-set-key (kbd "C-c ?") 'my/tldr)

(defun my/pulse-point-line (&rest _)
  (pulse-momentary-highlight-one-line (point)))
(dolist (my/command '(scroll-up-command scroll-down-command
		      isearch-repeat-forward isearch-repeat-backward
		      windmove-up windmove-down
		      windmove-left windmove-right
		      recenter-top-bottom other-window))
	(advice-add my/command :after 'my/pulse-point-line))

;;; Desktop

(setq tab-bar-show nil)

;; (desktop-save-mode 1)
;; desktop-remove
;; desktop-clear
;; (setq desktop-restore-eager)
