;;; grey clouds overhead
;;; tiny black birds rise and fall
;;; snow covers emacs
;;; 
;;; Defaults

;;;; Startup

;;;;; Garbage collection

;; (setq gc-cons-threshold most-positive-fixnum) -- set in early-init.el
(add-hook 'emacs-startup-hook
	  (lambda ()
	    (setq gc-cons-threshold
		  (car (get 'gc-cons-threshold 'standard-value)))))

;;;;; Disable interface

(unless (eq 'ns window-system)
  (when (fboundp 'menu-bar-mode) (menu-bar-mode -1)))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))

;;;; Graphical

;;;;; Load color theme

;; wombat renders eww wikipedia formulas properly
;; fyi: *help* buffer links are hard to see
;; (load-theme 'wombat t)
;; cooler-looking comments
;; (set-face-attribute 'font-lock-comment-face 'nil :slant 'italic)
;; make the cursor easier to spot than the default grey
;; (set-face-attribute 'cursor 'nil :background "#f6f3e8")

;;;;; Make initial frame transparent

(set-frame-parameter (selected-frame) 'alpha '(77 . 55))
(add-to-list 'default-frame-alist '(alpha . (77 . 55)))

;;;;; Maximize window if it isn't already

(when (eq 'ns window-system)
  ;; now fullscreen will preserve transparency
  (setq ns-use-native-fullscreen nil))
(unless (eq 'maximized (frame-parameter (selected-frame) 'fullscreen))
  (toggle-frame-maximized))

;;;;; Setup initial window layout

(when (eq 'ns window-system)
  (split-window-right))
(split-window-right)
(balance-windows)
(other-window 1)

;;;;; Fonts

;;;;;; Mac

(when (eq 'ns window-system)
  (set-face-attribute 'default nil :family "Fira Code" :height 130)
  (set-face-attribute 'fixed-pitch nil :family "Fira Code" :height 1.0)
  (set-face-attribute 'variable-pitch nil :family "ETBookOT" :height 1.2))

;;;;;; Linux

(when (eq 'x window-system)
  (set-face-attribute 'default nil :family "Ubuntu Mono" :height 120)
  (set-face-attribute 'fixed-pitch nil :family "Ubuntu Mono" :height 1.0)
  (set-face-attribute 'fixed-pitch-serif nil :family "Ubuntu Mono" :height 1.0)
  ;; doesn't seem to render ETBookOT properly
  ;; - the characters do not resting on a horizontal line
  (set-face-attribute 'variable-pitch nil :family "Source Sans Pro" :height 1.05))

;;;;; Transparency
  
(defun my/toggle-transparency ()
  (interactive)
  (let ((alpha (frame-parameter nil 'alpha)))
    (set-frame-parameter nil 'alpha
     (if (eql (cond ((numberp alpha) alpha)
		    ((numberp (cdr alpha)) (cdr alpha))
		    ((numberp (cadr alpha)) (cadr alpha)))
	      100)
	 '(77 . 55) '(100 . 100)))))

(global-set-key (kbd "C-c T") 'my/toggle-transparency)

;;;; Common

;;;;; Variables -- setq-default

(setq-default cursor-type 'bar
	      ;; half-screen on chromebook
	      fill-column 80)

;;;;; Variables -- setq

(setq user-full-name "Tyler Lee"
      user-mail-address "wtleeiv@gmail.com"

      default-directory "~/"
      backup-directory-alist '(("" . "~/.emacs.d/backup/"))
      auto-save-file-name-transforms `((".*"  "~/.emacs.d/backup/" t))

      apropos-do-all t ; better apropos searching
      buffer-file-coding-system 'utf-8-unix
      confirm-kill-processes nil
      ;; delete-by-moving-to-trash t
      dired-listing-switches "-alh"
      ediff-split-window-function 'split-window-horizontally
      ediff-window-setup-function 'ediff-setup-windows-plain
      ;; focus help window when created
      ;; - so I can close them easier with "q"
      help-window-select t
      inhibit-startup-message t
      initial-scratch-message nil
      require-final-newline t
      ring-bell-function 'ignore
      sentence-end-double-space nil
      set-mark-command-repeat-pop t
      shift-select-mode nil
      ;; prevent horizontal split
      split-height-threshold nil
      uniquify-buffer-name-style 'post-forward
      vc-follow-symlinks t)

;;;;; Custom File

;; Don't add custom section to init.el
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file :noerror))

;;;;; Modes

;;;;;; Display-modifying

;; Completion
;; - ~initials~ didn't seem to work for me, use ~partial-completion~ first
(setq completion-styles '(partial-completion substring flex))
(fido-mode 1)
;; don't complete with spc, so that I can type them in
(define-key minibuffer-local-completion-map " " 'self-insert-command)
;; Highlight matching pair
(setq show-paren-delay 0)
(show-paren-mode 1)
;; Wraps with active region!
(electric-pair-mode 1)
;; Column number in modeline
(column-number-mode 1)
;; Display time
(setq display-time-day-and-date t
      display-time-default-load-average nil)
(display-time-mode 1)
;; Syntax highlighting
(global-font-lock-mode 1)

;;;;;; Usability-modifying

;; Update buffer when file changes on disk
;; - will not try to revert remote (tramp) files
;; - auto-revert-tail-mode *does* work for remote files
(global-auto-revert-mode 1)

;; Remember last place in visited files
;; (save-place-mode 1)

;; Recursive editing
;; - sometimes useful, sometimes I forget to turn them off
;; - exit minibuffer session with "C-]"
(setq enable-recursive-minibuffers t)
(minibuffer-depth-indicate-mode 1)

;;;;; Hooks

;; "y/n" is good-enough, and less intrusive
(fset 'yes-or-no-p 'y-or-n-p)

;; Clean up buffers on save
;; - disable, since this might affect TRAMP buffers
;; (add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Garbage collect when idle
;;(add-hook 'focus-out-hook 'garbage-collect)

;; Report start-up statistics
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Ready in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time (time-subtract after-init-time
							before-init-time)))
                     gcs-done)))

;;;;; Keybindings

;;;;;; Differentiate C-m/C-i from RET/TAB

;; this breaks terminal compatibility, but so what - I use vim in the terminal

(define-key input-decode-map [?\C-i] [C-i]) ;; tab

(define-key input-decode-map [?\C-m] [C-m]) ;; ret

;; ;;;;;; Translate C-<tab> to M-<tab>

;; (define-key local-function-key-map (kbd "C-<tab>") (kbd "M-<tab>"))

;; ;;;;;; Setup modifier keys on Mac

;; no longer needed, since kinesis keyboard tap & hold is awesome
;; (when (eq 'ns window-system)
;;   (setq ns-command-modifier 'control
;;         ns-control-modifier 'meta
;;         ns-option-modifier 'meta))

;;;;;; Swaps

;;;;;;; Swap regex and default isearch bindings

(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)

;;;;;;; Swap dwim commands for their word-based siblings

;; comment-dwim :: M-; -- for reference
(global-set-key (kbd "M-c") 'capitalize-dwim)
(global-set-key (kbd "M-l") 'downcase-dwim)
(global-set-key (kbd "M-u") 'upcase-dwim)
;; Can be replaced
;; C-x C-l :: downcase-region
;; C-x C-u :: upcase-region

;;;;;; Additions

;;;;;;; Navigate read-only buffers w/o modifiers (C-x C-q)

(require 'view)
(setq view-read-only t)
;; don't remap view mode search "s", since "n" and "p" wont work

;;;;;;; Editing

;; rebinds center-line, center-paragraph
(global-set-key (kbd "M-o") 'other-window)

(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "M-z") 'zap-up-to-char)
(global-set-key (kbd "C-x C-M-t") 'transpose-regions)

;; explicitly map <home> and <end>
;; - desktop maps them to beg/end-of-line
;; - and C-<home>/<end> to beg/end-of-buffer
(global-set-key (kbd "<home>") 'beginning-of-buffer)
(global-set-key (kbd "<end>") 'end-of-buffer)

;;;;;;; Usability

;; (global-set-key (kbd "C-h a") 'apropos)	; apropos all the things
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-c i") 'imenu)

;; Change navigation
(global-highlight-changes-mode 1)
(setq highlight-changes-visibility-initial-state nil)
(global-set-key (kbd "C-c <down>") 'highlight-changes-next-change)
(global-set-key (kbd "C-c <up>") 'highlight-changes-previous-change)

;; Select windows with S-<up/down/left/right>
;; - "M-o" binding for ~other-window~ is good enough
;; - org-mode bindings override
(windmove-default-keybindings)
(setq windmove-wrap-around t)

;; Window undo/redo C-c <left/right>
(winner-mode 1)

;;;;;;; Function keys

;;;;;;;; Windows

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

;;;;;;;; Buffers

(global-set-key (kbd "<f8>") 'previous-buffer)
(global-set-key (kbd "<f9>") 'next-buffer)

;;;;; Functions and "C-c" bindings

;;;;;; Easy config editing

(defun my/edit-config ()
  (interactive)
  (find-file user-init-file))

(global-set-key (kbd "C-c e") 'my/edit-config)

;;;;;; Tldr search

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

;;;;;; Pulse line on common navigation jumps

(defun my/pulse-point-line (&rest _)
  (pulse-momentary-highlight-one-line (point)))

;; you can pulse current line with C-l
(dolist (my/command '(scroll-up-command scroll-down-command
		      isearch-repeat-forward isearch-repeat-backward
		      windmove-up windmove-down
		      windmove-left windmove-right
		      forward-button backward-button
		      recenter-top-bottom other-window))
  (advice-add my/command :after 'my/pulse-point-line))

;;;;;; Auto-pomodoro -- via org-timer-set-timer

(autoload 'org-timer-set-timer "org-timer" "get up and move!" t nil)
;; (with-eval-after-load "org-timer") -- not needed, since
(setq org-timer-default-timer 20) ; defined with defvar

(defun my/auto-pomodoro ()   ; popup display code from ns-print-buffer
  (let ((last-nonmenu-event (if (listp last-nonmenu-event)
				last-nonmenu-event
                              ;; Fake it -- ensure popup is displayed
                              '(mouse-1 POSITION 1))))
    (if (y-or-n-p "Time to take a break")
	(progn
	  (org-timer-set-timer org-timer-default-timer)
	  (message "Good boy, very mart :)"))
      (progn
	(org-timer-set-timer 2)
	(message "Ming!")))))

(add-hook 'org-timer-done-hook 'my/auto-pomodoro)

(add-hook 'emacs-startup-hook
	  (lambda ()
	    (org-timer-set-timer org-timer-default-timer)))

;;;;;; Recentf and completing read binding

(recentf-mode 1)

(defun my/recent-files ()
  (interactive)
  (let ((my/file-to-open (completing-read "Recent files: "
					  recentf-list nil t)))
    (when my/file-to-open
      (find-file my/file-to-open))))

;; set recentf-max-saved-items to something >20 (default) if desired
(global-set-key (kbd "C-c r") 'my/recent-files)

;;;;; Sessions

;;;;;; Tabs

;; "C-x t" prefix
(setq tab-bar-show nil)

;;;;;; Desktop

;; Desktop mode -- exists
;; - maybe enable later
;; (desktop-save-mode 1)
;; (setq desktop-restore-eager)
;; Functions to remember
;; desktop-remove :: delete desktop so emacs won't use it next time it loads
;; desktop-clear :: clears current emacs session

;;;;; Tramp

;; don't waste time checking vc for remote files
(setq vc-ignore-dir-regexp
      (format "\\(%s\\)\\|\\(%s\\)"
              vc-ignore-dir-regexp
              tramp-file-name-regexp))

;; use cached directory contents for filename completion
(setq tramp-completion-reread-directory-timeout nil)

;; cache remote file properties for N sec
;; - perhaps set to nil -- always use cache
(setq remote-file-name-inhibit-cache 10) ; default -- 10

;; log file caching ... for now (testing purposes)
(setq tramp-verbose 7) ; default -- 3

;; persist remote connections, don't prompt credentials every 5 min
(setq tramp-use-ssh-controlmaster-options t) ; default -- t
(setq tramp-ssh-controlmaster-options
      (concat "-o ControlMaster=auto "
	      "-o ControlPath=/tmp/ssh-ControlPath-%%r@%%h:%%p "
	      "-o ControlPersist=yes"))

;;;;; Org

(setq org-directory "~/notes/")
(setq org-agenda-files (list org-directory))
(setq org-default-notes-file (concat org-directory "inbox.org"))

(setq org-capture-templates
      `(("n" "Note" entry (file+headline org-default-notes-file "Notes")
	 "** %?")
	("t" "Task" entry (file+headline org-default-notes-file "Tasks")
	 "** TODO %?")))

(setq org-hide-leading-stars t)
(setq org-hide-emphasis-markers t)
(setq org-list-allow-alphabetical t)
(setq org-pretty-entities t) ;; toggle with C-c C-x \
(setq org-return-follows-link t)
(setq org-src-preserve-indentation t)
(setq org-src-confirm-babel-evaluate nil)

(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c l") 'org-store-link)

;; (defun my/notes-inbox ()
;;   (interactive)
;;   (find-file org-default-notes-file))

;; (global-set-key (kbd "C-c n")
;; 		'my/notes-inbox)

;;; Packages
(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/"))
;; (package-initialize) -- unnecessary in emacs 27+

;; native-compile packages
(setq package-native-compile t)

;; asynchronous compilation
(setq comp-deferred-compilation t)

;;;; Org

;; also install:
;; - anki-editor
;; - org-anki

;;;;; Org noter

(setq org-noter-notes-search-path (list org-directory))
(setq org-noter-always-create-frame nil)
(setq org-noter-separate-notes-from-heading nil)

;;;;; Deft

(setq deft-directory org-directory)
(setq deft-recursive t)

(global-set-key (kbd "<C-m> s") 'deft)

;;;;; Org roam -- v1

(setq org-roam-directory (concat org-directory "zettelkasten/"))
(setq org-roam-index-file "20210416153028-index.org")
(setq org-roam-db-update-method 'idle-timer)
;; (setq org-roam-buffer-position 'left)
;; (setq org-roam-buffer-width 0.15)
(setq org-roam-dailies-directory "journal/")

(global-set-key (kbd "<C-m> f") #'org-roam-find-file)
(global-set-key (kbd "<C-m> g") #'org-roam-graph)
(global-set-key (kbd "<C-m> i") #'org-roam-jump-to-index)
(global-set-key (kbd "<C-m> l") #'org-roam-insert-immediate)
(global-set-key (kbd "<C-m> L") #'org-roam-insert)
(global-set-key (kbd "<C-m> c") #'org-roam-dailies-capture-today)
(global-set-key (kbd "<C-m> t") #'org-roam-dailies-find-today)
(global-set-key (kbd "<C-m> p") #'org-roam-dailies-find-yesterday)
(global-set-key (kbd "<C-m> n") #'org-roam-dailies-find-tomorrow)

(add-hook 'after-init-hook 'org-roam-mode)

;; ;;;;; nroam

;; nroam is a little buggy still
;; it's messing with org-capture, and the read-only thing when I
;; - disable the mode is annoying/troublesome
;; I like the idea of having all links displayed in-buffer,
;; - but the implementation needs work

;; (global-set-key (kbd "<C-m> r") 'nroam-mode)

;; (add-hook 'org-mode-hook 'nroam-setup-maybe)

;;;; Diminish

(diminish 'highlight-changes-mode)
(diminish 'eldoc-mode)

;;;; Doom themes

(setq doom-themes-enable-bold t)
(setq doom-themes-enable-italic t)

(setq my/theme 'dark)
(setq my/dark-theme 'doom-miramare)
(setq doom-miramare-brighter-comments t)
(setq my/light-theme 'doom-flatwhite)

(load-theme my/dark-theme t)
(with-eval-after-load 'org-mode
  (doom-themes-enable-org-config))

(defun my/toggle-theme ()
  (interactive)
  (if (eq my/theme 'dark)
      (progn
	(disable-theme my/dark-theme)
	(load-theme my/light-theme t)
	(set-face-attribute 'font-lock-comment-face 'nil :slant 'italic)
	(setq my/theme 'light))
    (progn
      (disable-theme my/light-theme)
      (load-theme my/dark-theme t)
      (set-face-attribute 'font-lock-comment-face 'nil :slant 'italic)
      (setq my/theme 'dark))))

(global-set-key (kbd "C-c t") 'my/toggle-theme)

;;;; vterm

(setq vterm-module-cmake-args "-DUSE_SYSTEM_LIBVTERM=no")

;;;; Magit

(global-set-key (kbd "C-c g") 'magit-file-dispatch)

;;;; Pdf tools

(pdf-tools-install)
;; wombat colors
;; (setq pdf-view-midnight-colors (cons "#f6f3e8" "#242424"))

;;;; Outshine

;; emacs init file org-folding
;; - can use org speed commands -- maybe try out one day
(add-hook 'emacs-lisp-mode-hook 'outshine-mode)
(add-hook 'emacs-lisp-mode-hook (lambda () (diminish 'outshine-mode)))
(add-hook 'emacs-lisp-mode-hook (lambda () (diminish 'outline-minor-mode)))
;; disable in scratch buffer
(add-hook 'lisp-interaction-mode-hook
	  (lambda ()
	    (outshine-mode -1)
	    (outline-minor-mode -1)))

;;;; Undo tree

(setq undo-tree-auto-save-history t)
(setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/backup/")))
;; undo-tree might slow down TRAMP, re-enable lighter to test
(setq undo-tree-mode-lighter "")
(setq undo-tree-visualizer-diff t)
(setq undo-tree-visualizer-timestamps t)

(global-undo-tree-mode)

;;; 
;;; my dot emacs grows
;;; then one day, I look inside
;;; singularity
