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
(setq doom-font (font-spec :family "Menlo" :size 13))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-horizon)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type nil)


;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c g k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c g d') to jump to their definition and see how
;; they are implemented.

(after! tramp
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path))

(when (eq window-system 'ns) ; only on mac
  (setq ns-command-modifier 'control
        ns-control-modifier 'meta
        ns-option-modifier 'super))

(defun toggle-transparency ()
  (interactive)
  (let ((alpha (frame-parameter nil 'alpha)))
    (set-frame-parameter
     nil 'alpha
     (if (or (null alpha) ; initial load
             (eql (cond ((numberp alpha) alpha)
                        ((numberp (cdr alpha)) (cdr alpha))
                        ;; Also handle undocumented (<active> <inactive>) form.
                        ((numberp (cadr alpha)) (cadr alpha)))
                  100))
         '(80 . 70) '(100 . 100)))))

(map! :map doom-leader-toggle-map
      :desc "Transparency" "t" #'toggle-transparency)

;; (setq modus-vivendi-theme-slanted-constructs t
;;       modus-vivendi-theme-subtle-diffs t
;;       modus-vivendi-theme-distinct-org-blocks t
;;       modus-vivendi-theme-rainbow-headings t)

;; (toggle-frame-maximized) ; M-f10
;; (set-frame-parameter (selected-frame) 'alpha '(85 . 75))
;; (add-to-list 'default-frame-alist '(alpha . (85 . 75)))

;; (add-hook 'emacs-startup-hook #'(lambda () (message "Happy Hackses :)")))
