(require 'clojure-mode)
(require 'cider)
(require 'flycheck-joker)

(add-hook 'clojure-mode-hook 'cider-mode)
(add-hook 'clojure-mode-hook 'flycheck-mode)

(provide 'wtleeiv-clojure)
