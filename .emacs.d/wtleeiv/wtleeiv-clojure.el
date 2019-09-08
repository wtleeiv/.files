(require 'clojure-mode)
(require 'cider)
(require 'paredit)

(add-hook 'clojure-mode-hook 'enable-paredit-mode)
(add-hook 'cider-repl-mode-hook 'enable-paredit-mode)
(add-hook 'clojurescript-mode-hook 'enable-paredit-mode)

(provide 'wtleeiv-clojure)
