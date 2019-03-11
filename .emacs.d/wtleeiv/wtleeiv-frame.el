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

;;; Restore frame size

(setq wtleeiv-framegeometry-file (wtleeiv-path ".framegeometry.el"))

(defun wtleeiv--normalized-frame-parameter (parameter)
  (let ((value (frame-parameter (selected-frame) parameter)))
    (if (number-or-marker-p value) (max value 0) 0)))

(defun wtleeiv-save-framegeometry ()
  (let* ((props '(left top width height))
         (values (mapcar 'wtleeiv--normalized-frame-parameter props)))
    (with-temp-buffer
        (cl-loop for prop in props
                 for val in values
                 do (insert (format "(add-to-list 'initial-frame-alist '(%s . %d))\n"
                                    prop val)))
      (write-file wtleeiv-framegeometry-file))))

(defun wtleeiv-load-framegeometry ()
  (when (file-exists-p wtleeiv-framegeometry-file)
    (load-file wtleeiv-framegeometry-file))
  (message "Happy hackses :)"))

(add-hook 'emacs-startup-hook 'wtleeiv-load-framegeometry)
(add-hook 'kill-emacs-hook 'wtleeiv-save-framegeometry)

(provide 'wtleeiv-frame)
