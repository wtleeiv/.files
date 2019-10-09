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
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#292929" "#ff3333" "#aaffaa" "#aaeecc" "#aaccff" "#FF1F69" "#aadddd" "#999999"])
 '(background-color "#202020")
 '(background-mode dark)
 '(cursor-color "#cccccc")
 '(custom-safe-themes
   (quote
    ("ff829b1ac22bbb7cee5274391bc5c9b3ddb478e0ca0b94d97e23e8ae1a3f0c3e" "fa477d10f10aa808a2d8165a4f7e6cee1ab7f902b6853fbee911a9e27cf346bc" "886fe9a7e4f5194f1c9b1438955a9776ff849f9e2f2bbb4fa7ed8879cdca0631" "8885761700542f5d0ea63436874bf3f9e279211707d4b1ca9ed6f53522f21934" "11e0bc5e71825b88527e973b80a84483a2cfa1568592230a32aedac2a32426c1" default)))
 '(foreground-color "#cccccc")
 '(package-selected-packages
   (quote
    (ergoemacs-mode ido-completing-read+ clj-refactor zenburn-theme xah-fly-keys which-key tide smex slime-company realgud rainbow-delimiters pdf-tools paredit ob-ipython noctilux-theme neotree magit lispy latex-extra kaolin-themes forth-mode flycheck-joker expand-region exec-path-from-shell esup doom-modeline company-quickhelp company-auctex company-anaconda clojure-mode-extra-font-locking cider 4clojure))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
