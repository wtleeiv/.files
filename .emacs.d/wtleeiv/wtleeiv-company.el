(require 'company)

(company-quickhelp-mode 1)
(setq company-quickhelp-delay 0.7
      company-tooltip-align-annotations t)

(global-company-mode)
(push 'slime-company slime-contribs)

(define-key company-active-map (kbd "C-n") 'company-select-next-or-abort)
(define-key company-active-map (kbd "C-p") 'company-select-previous-or-abort)
(define-key company-active-map (kbd "C-d") 'company-show-doc-buffer)
(define-key company-active-map (kbd "M-.") 'company-show-location)

(provide 'wtleeiv-company)
