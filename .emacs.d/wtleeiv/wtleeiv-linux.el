(require 'pdf-tools)
(pdf-tools-install)
(setq pdf-view-midnight-colors '("#ffffff" . "#000000"))

;; add ipython to path
(add-to-list 'exec-path "/home/wtleeiv/anaconda3/condabin")
(add-to-list 'exec-path "/home/wtleeiv/anaconda3/bin")

(provide 'wtleeiv-linux)
