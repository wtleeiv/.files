(require 'ob-ipython)
(setf python-shell-interpreter "ipython"
      python-shell-interpreter-args "-i --simple-prompt")

(require 'company-anaconda)
(add-to-list 'company-backends 'company-anaconda)
(add-hook 'python-mode-hook 'anaconda-mode)

(require 'realgud)

(provide 'wtleeiv-python)
