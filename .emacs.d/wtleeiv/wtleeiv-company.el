(require 'company)

(company-quickhelp-mode 1)
(setq company-minimum-prefix-length 1
      company-idle-delay 0
      company-quickhelp-delay 0
      company-tooltip-align-annotations t)

(global-company-mode)
;; TODO disable shell mode here
(defun wtleeiv-disable-completion ()
  (company-mode -1))

(add-hook 'shell-mode-hook #'wtleeiv-disable-completion)
(add-hook 'eshell-mode-hook #'wtleeiv-disable-completion)

(push 'slime-company slime-contribs)

(define-key company-active-map (kbd "C-n") 'company-select-next-or-abort)
(define-key company-active-map (kbd "C-p") 'company-select-previous-or-abort)
(define-key company-active-map (kbd "C-d") 'company-show-doc-buffer)
(define-key company-active-map (kbd "M-.") 'company-show-location)

(global-set-key (kbd "M-\\") 'company-complete)

(provide 'wtleeiv-company)
