(require 'company)

(company-quickhelp-mode 1)
(setq company-minimum-prefix-length 1
      company-idle-delay 0
      company-quickhelp-delay 0
      company-tooltip-align-annotations t)

(global-company-mode)
;; TODO disable shell mode here

(push 'slime-company slime-contribs)

(define-key company-active-map (kbd "M-t") 'company-select-next-or-abort)
(define-key company-active-map (kbd "M-c") 'company-select-previous-or-abort)
(define-key company-active-map (kbd "C-d") 'company-show-doc-buffer)
(define-key company-active-map (kbd "M-.") 'company-show-location)

(global-set-key (kbd "M-\\") 'company-complete)

(provide 'wtleeiv-company)
