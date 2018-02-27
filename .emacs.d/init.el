;; steps required by melpa to add package source
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)

;; load config file
(org-babel-load-file "~/.emacs.d/config.org")
