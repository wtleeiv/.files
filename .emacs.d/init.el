;; don't flash menu bars initially
(menu-bar-mode 0)
(blink-cursor-mode 0)
(scroll-bar-mode 0)
(tool-bar-mode 0)

;; steps required by melpa to add package source
(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; put here for config.org
(setq vc-follow-symlinks t)

;; load config file
(org-babel-load-file "~/.emacs.d/config.org")
