(defun wtleeiv-install-packages (pkg-list)
  (package-refresh-contents)
  (dolist (pkg pkg-list)
    (package-install pkg)))

(defvar wtleeiv-common-packages
  '(doom-modeline
    counsel
    magit
    paredit
    company-quickhelp
    slime-company
    ob-ipython
    company-anaconda
    expand-region))

(defvar wtleeiv-latex-packages
  '(latex-extra
    company-auctex))

(defvar wtleeiv-linux-packages
  '(pdf-tools))

(require 'wtleeiv-package)
(when (window-system)
  (require 'wtleeiv-frame))
(require 'wtleeiv-general)
(require 'wtleeiv-counsel)
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

;; (require 'wtleeiv-latex)

(provide 'wtleeiv)
