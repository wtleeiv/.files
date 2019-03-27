(require 'cl-lib)

(tool-bar-mode 0)
(scroll-bar-mode 0)

(add-to-list 'default-frame-alist
             '(font . "Noto Mono-10:antialias=subpixel"))

(require 'noctilux-theme)
(load-theme 'noctilux t)
(set-cursor-color "#ccaaff")
(set-face-background 'show-paren-match (face-background 'default))
(set-face-attribute 'show-paren-match nil
		    :foreground "#ccaaff"
		    :weight 'bold
		    :underline t)

(add-hook 'emacs-startup-hook #'(lambda () (message "Happy hackses :)")))

(provide 'wtleeiv-frame)
