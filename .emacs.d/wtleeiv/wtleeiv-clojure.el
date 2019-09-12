(require 'clojure-mode)
(require 'cider)
(require 'flycheck-joker)

(add-hook 'clojure-mode-hook 'flycheck-mode)
(add-hook 'clojure-mode-hook 'cider-mode)

(provide 'wtleeiv-clojure)
