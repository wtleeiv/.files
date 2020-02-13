;; Make startup faster by reducing the frequency of garbage
;; collection.  The default is 800 kilobytes.  Measured in bytes.
(setq gc-cons-threshold (* 50 1000 1000))

(setq wtleeiv-root (concat user-emacs-directory "wtleeiv/"))
(defun wtleeiv-path (path)
  (concat wtleeiv-root path))
;; init emacs
(add-to-list 'load-path wtleeiv-root)
(if (locate-library "wtleeiv")
    (load-library "wtleeiv")
    (display-warning :warning "Your config files are not present."))

;; Make gc pauses faster by decreasing the threshold.
(setq gc-cons-threshold (* 2 1000 1000))
