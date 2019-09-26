(require 'counsel)

(setq ivy-use-virtual-buffers t
      ivy-count-format "(%d/%d) "
      enable-recursive-minibuffers t)

(ivy-mode 1)
(counsel-mode 1)

(global-set-key (kbd "C-s") 'swiper)
(global-set-key (kbd "C-c f") 'counsel-rg)

(provide 'wtleeiv-counsel)
