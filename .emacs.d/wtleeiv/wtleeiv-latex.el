(require 'tex-site)
;; enable document parsing
(setq TeX-auto-save t
      TeX-parse-self t)
;; multi file documents (or \include \input)
(setq-default TeX-master nil)

(require 'latex-extra)
(add-hook 'LaTeX-mode-hook #'latex-extra-mode)

(require 'company-auctex)
(company-auctex-init)

(add-hook 'LaTeX-mode-hook
	  (lambda ()
	    ;; (turn-on-reftex)
	    ;; (setq reftex-plug-into-AUCTeX t)
	    ;; (reftex-isearch-minor-mode)
	    (setq TeX-PDF-mode t)
	    (setq TeX-source-correlate-method 'synctex)
	    (setq TeX-source-correlate-start-server t)))

;; Update PDF buffers after successful LaTeX runs
(add-hook 'TeX-after-TeX-LaTeX-command-finished-hook
	  #'TeX-revert-document-buffer)

;; to use pdfview with auctex
(setq TeX-view-program-selection '((output-pdf "pdf-tools"))
      TeX-source-correlate-start-server t)
(setq TeX-view-program-list '(("pdf-tools" "TeX-pdf-tools-sync-view")))

(provide 'wtleeiv-latex)
