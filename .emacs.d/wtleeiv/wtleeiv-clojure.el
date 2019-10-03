(require 'clojure-mode)
(require 'clj-refactor)
(require 'cider)
(require 'flycheck-joker)

(defun wtleeiv-clojure-mode-hook ()
  (clj-refactor-mode 1)
  (yas-minor-mode 1) ; for adding require/use/import statements
  ;; This choice of keybinding leaves cider-macroexpand-1 unbound
  (cljr-add-keybindings-with-prefix "C-c C-m")
  (flycheck-mode 1)
  (cider-mode 1))

(add-hook 'clojure-mode-hook #'my-clojure-mode-hook)

(provide 'wtleeiv-clojure)
