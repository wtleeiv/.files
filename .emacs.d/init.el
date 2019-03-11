(setq wtleeiv-root (concat user-emacs-directory "wtleeiv/"))
(defun wtleeiv-path (path)
  (concat wtleeiv-root path))

(add-to-list 'load-path wtleeiv-root)
(if (locate-library "wtleeiv")
    (load-library "wtleeiv")
    (display-warning :warning "Your config files are not present."))
