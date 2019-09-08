(setq  user-full-name "Tyler Lee"
       user-mail-address "wtleeiv@gmail.com")

(setq default-directory "~"
      backup-directory-alist '(("" . "~/.emacs.d/backup"))
      auto-save-file-name-transforms `((".*"  "~/.emacs.d/backup/" t)))

(setq indent-tabs-mode nil
      vc-follow-symlinks t
      require-final-newline t
      inhibit-startup-message t
      ring-bell-function 'ignore
      initial-scratch-message nil
      sentence-end-double-space nil
      uniquify-buffer-name-style 'post-forward)

(require 'paren)
(show-paren-mode t)
(setq show-paren-delay 0)

(require 'doom-modeline)
(doom-modeline-mode 1)

(require 'which-key)
(which-key-mode)

(blink-cursor-mode 0)
(global-hl-line-mode t)
(global-display-line-numbers-mode 1)

;; syntax highlighting
(global-font-lock-mode t)
;; update buffer when file changes on disk
(global-auto-revert-mode t)

(setq-default dired-listing-switches "-alh")
(setq-default buffer-file-coding-system 'utf-8-unix)

(fset 'yes-or-no-p 'y-or-n-p)

(add-hook 'before-save-hook
	  'delete-trailing-whitespace)

;; start emacs server
;;; connect with :: emacsclient <file> in shell
(server-start)

(provide 'wtleeiv-general)
