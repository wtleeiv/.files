(require 'counsel)

(setq ivy-use-virtual-buffers t
      ivy-count-format "(%d/%d) "
      enable-recursive-minibuffers t)

(ivy-mode 1)
(counsel-mode 1)

(provide 'wtleeiv-counsel)
