;;;; Notes
;; package-refresh-contents :: update package list
;; package-install :: install package

;;;; Packages
;; doom-modeline
;; counsel
;; magit
;; paredit
;; company-quickhelp
;; slime-company
;; ob-ipython
;; company-anaconda

(require 'wtleeiv-package)
(require 'wtleeiv-general)
(require 'wtleeiv-counsel)
(require 'wtleeiv-magit)
(require 'wtleeiv-paste)
(require 'wtleeiv-lisp)
(require 'wtleeiv-paredit)
(require 'wtleeiv-company)
(require 'wtleeiv-python)
(require 'wtleeiv-forth)
(require 'wtleeiv-org)
(require 'wtleeiv-eshell)
(when (eq window-system 'x)
  (require 'wtleeiv-linux))
(when (eq window-system 'w32)
  (require 'wtleeiv-windows))
(require 'wtleeiv-keybindings)
(when (window-system)
  (require 'wtleeiv-frame))

;; ;; syntax checking
;; (use-package flycheck
;;              :defer t
;;              :config (global-flycheck-mode))

;; ;;; Lisp
;; ;; echo arg list
;; (require 'eldoc)
;; (eldoc-add-command
;;  'paredit-backward-delete
;;  'paredit-close-round)

(provide 'wtleeiv)
