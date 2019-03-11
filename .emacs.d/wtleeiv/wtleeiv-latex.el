(require 'tex-site)
;; enable document parsing
(setq TeX-auto-save t
      TeX-parse-self t)
;; multi file documents (or \include \input)
(setq-default TeX-master nil)

;; (require 'latex-extra)
;; (add-hook 'LaTeX-mode-hook #'latex-extra-mode)

(require 'company-auctex)
(company-auctex-init)

(provide 'wtleeiv-latex)
