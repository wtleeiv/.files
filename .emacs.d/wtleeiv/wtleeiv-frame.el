(require 'cl-lib)

(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)

;;(toggle-frame-maximized)
(when (eq window-system 'x)
  (add-to-list 'default-frame-alist
	       '(font . "Cousine-11:antialias=subpixel")))

<<<<<<< HEAD
(when (eq window-system 'darwin)
  (toggle-frame-maximized)
  (add-to-list 'default-frame-alist
               '(font . "Dank Mono-13:antialias=subpixel")))

(require 'planet-theme)
(load-theme 'planet t)

(defun wtleeiv-noctilux-theme ()
  "not run, but here if you need it"
  (require 'noctilux-theme)
  (load-theme 'noctilux t)
  (set-cursor-color "#ccaaff")
  (set-face-background 'show-paren-match (face-background 'default))
  (set-face-attribute 'show-paren-match nil
		      :foreground "#ccaaff"
		      :weight 'bold
		      :underline t))

(set-frame-parameter (selected-frame) 'alpha '(77 . 50))
(add-to-list 'default-frame-alist '(alpha . (77 . 50)))

(defun toggle-transparency ()
  (interactive)
  (let ((alpha (frame-parameter nil 'alpha)))
    (set-frame-parameter
     nil 'alpha
     (if (eql (cond ((numberp alpha) alpha)
                    ((numberp (cdr alpha)) (cdr alpha))
                    ;; Also handle undocumented (<active> <inactive>) form.
                    ((numberp (cadr alpha)) (cadr alpha)))
              100)
         '(77 . 50) '(100 . 100)))))

(global-set-key (kbd "C-c t") 'toggle-transparency)

(add-hook 'emacs-startup-hook #'(lambda () (message "Happy hackses :)")))

(provide 'wtleeiv-frame)
