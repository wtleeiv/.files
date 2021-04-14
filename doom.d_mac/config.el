;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Tyler Lee"
      user-mail-address "wtleeiv@gmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))
(setq doom-font (font-spec :family "Iosevka"
                           :size 15
                           ;; :slant 'italic
                           :weight 'light))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-wilmersdorf)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type nil)


;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

(setq confirm-kill-emacs nil)

(after! evil ; move to newly-split windows
  (setq evil-split-window-below t
        evil-vsplit-window-right t))

(after! tramp
  ;; use remote path
  ;; (add-to-list 'tramp-remote-path 'tramp-own-remote-path)
  ;; don't waste time checking vc for remote files
  (setq vc-ignore-dir-regexp
        (format "\\(%s\\)\\|\\(%s\\)"
                vc-ignore-dir-regexp
                tramp-file-name-regexp))
  ;; use cached directory contents for filename completion
  (setq tramp-completion-reread-directory-timeout nil)
  ;; cache remote file properties for N sec
  ;; - perhaps set to nil -- always use cache
  (setq remote-file-name-inhibit-cache nil) ; default -- 10
  ;; limit logging to warnings and errors
  (setq tramp-verbose 2) ; default -- 3
  ;; persist remote connections, don't prompt credentials every 5 min
  (setq tramp-use-ssh-controlmaster-options t) ; default -- t
  (setq tramp-ssh-controlmaster-options
        (concat "-o ControlMaster=auto "
	        "-o ControlPath=/tmp/ssh-ControlPath-%%r@%%h:%%p "
	        "-o ControlPersist=yes")))

(after! which-key
  (setq which-key-idle-delay 0.2
        which-key-idle-secondary-delay 0.2))

(after! writeroom-mode
  (setq writeroom-width 82)
  (remove-hook 'writeroom-global-effects 'writeroom-set-alpha)
  (remove-hook 'writeroom-mode-hook '+zen-enable-mixed-pitch-mode-h)
  (remove-hook 'writeroom-mode-hook '+zen-enable-text-scaling-mode-h))


;; (use-package! lispy ; lispyville will run in any mode lispy does
;;   :hook ((cider-repl-mode . lispy-mode)))

(when (eq window-system 'ns)            ; only on mac
  (setq ;; ns-command-modifier 'control
        ;; ns-control-modifier 'meta
        ns-right-option-modifier 'meta))

(defun toggle-transparency ()
  (interactive)
  (let* ((alpha (frame-parameter nil 'alpha))
         (transparency-off (or (null alpha)
                               (eql (cond ((numberp alpha)
                                           alpha)
                                          ((numberp (cdr alpha))
                                           (cdr alpha))
                                          ((numberp (cadr alpha))
                                           (cadr alpha)))
                                    100))))
    (set-frame-parameter nil 'alpha (if transparency-off
                                        '(77 . 55)
                                      '(100 . 100)))))

(map! :map doom-leader-toggle-map
      :desc "Transparency" "T" #'toggle-transparency)

(add-hook! window-setup
  (setq display-time-day-and-date t
        display-time-default-load-average nil)
  (display-time))

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
