(defun wtleeiv-install-packages (pkg-list)
  (package-refresh-contents)
  (dolist (pkg pkg-list)
    (package-install pkg)))

(defvar wtleeiv-common-packages
  '(magit
    ;; doom-modeline
    counsel
    paredit
    company-quickhelp
    slime-company
    ob-ipython
    company-anaconda
    expand-region
    real-gud
    clojure-mode
    cider
    flycheck-joker
    tide
    which-key
    xah-fly-keys
    noctilux-theme
    kaolin-themes))

(defvar wtleeiv-latex-packages
  '(latex-extra
    company-auctex))

(defvar wtleeiv-linux-packages
  '(pdf-tools
    exec-path-from-shell))

(require 'wtleeiv-package)
(when (window-system)
  (require 'wtleeiv-frame))
(require 'wtleeiv-general)
;; (require 'wtleeiv-counsel)
(require 'wtleeiv-paste)
;;(require 'wtleeiv-lisp)
(require 'wtleeiv-clojure)
(require 'wtleeiv-paredit)
(require 'wtleeiv-company)
(require 'wtleeiv-python)
;; (require 'wtleeiv-typescript)
;; (require 'wtleeiv-forth)
;;(require 'wtleeiv-factor)
(require 'wtleeiv-org)
(when (eq window-system 'x)
  (require 'wtleeiv-linux))
(when (eq window-system 'w32)
  (require 'wtleeiv-windows))
(require 'wtleeiv-keybindings)

;; (require 'wtleeiv-latex)

(provide 'wtleeiv)
