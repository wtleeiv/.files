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
;; (setq doom-font (font-spec :family "Iosevka" :size 15))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-miramare)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/Sync/org/")

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

(setq confirm-kill-emacs nil
      source-directory "/home/ty/bin/emacs-27.1/"
      browse-url-browser-function #'eww-browse-url)

(after! bibtex-completion
  (setq bibtex-completion-bibliography "~/Documents/MyLibrary.bib"
        bibtex-completion-notes-path org-directory))

(add-hook! org-roam-mode org-roam-bibtex-mode)

;; (add-hook! cider-repl-mode lispy-mode)

(after! deft
  (setq deft-directory org-directory))

(after! doom-modeline
  (setq doom-modeline-major-mode-icon t))

(after! evil ; move to newly-split windows
  (setq evil-split-window-below t
        evil-vsplit-window-right t))

(after! eww
  (setq eww-search-prefix "https://google.com/search?q="))

;; (after! org
;;   (setq org-babel-clojure-backend 'cider))

(after! org-noter
  (setq org-noter-always-create-frame nil
        org-noter-separate-notes-from-heading nil)
  (map! :map pdf-view-mode-map
        :n "i" #'org-noter-insert-note))

(after! pdf-view
  (map! :map pdf-view-mode-map
        :n "<left>" #'pdf-view-scroll-down-or-previous-page
        :n "<right>" #'pdf-view-scroll-up-or-next-page))

(add-hook! eww-after-render shrface-mode)

(after! shrface
  (setq shrface-href-versatile t)
  (shrface-basic)
  (shrface-trial)
  (map! :map eww-mode-map
        :n "<tab>" #'shrface-outline-cycle
        :n "<backtab>" #'shrface-outline-cycle-buffer))

;; (after! tramp
;;   (add-to-list 'tramp-remote-path 'tramp-own-remote-path))

;; (after! which-key
;;   (setq which-key-idle-delay 0.01
;;         which-key-idle-secondary-delay 0.01))

(after! writeroom-mode
  (remove-hook 'writeroom-global-effects 'writeroom-set-alpha)
  (remove-hook 'writeroom-mode-hook '+zen-enable-mixed-pitch-mode-h)
  (remove-hook 'writeroom-mode-hook '+zen-enable-text-scaling-mode-h))

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
                                        '(88 . 77)
                                      '(100 . 100)))))

(map! :map doom-leader-toggle-map
      :desc "Transparency" "T" #'toggle-transparency)

(add-hook! window-setup
  ;; (toggle-frame-fullscreen)
  (toggle-transparency))

(add-hook! after-init
  (setq display-time-default-load-average nil
        display-time-day-and-date t)
  (display-time))
