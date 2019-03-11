(load (expand-file-name "~/.quicklisp/slime-helper.el"))
(setq inferior-lisp-program "sbcl --dynamic-space-size 2048")

(require 'slime)

(setq slime-contribs '(slime-fancy slime-asdf slime-sprof slime-mdot-fu
                       slime-compiler-notes-tree slime-hyperdoc
                       slime-indentation slime-repl)
      slime-complete-symbol-function 'slime-fuzzy-complete-symbol
      slime-net-coding-system 'utf-8-unix
      slime-startup-animation nil
      slime-auto-select-connection 'always
      slime-kill-without-query-p t
      slime-description-autofocus t
      slime-fuzzy-explanation ""
      slime-asdf-collect-notes t
      slime-inhibit-pipelining nil
      slime-load-failed-fasl 'always
      slime-when-complete-filename-expand t
      slime-repl-history-remove-duplicates t
      slime-repl-history-trim-whitespaces t
      slime-export-symbol-representation-auto t
      lisp-indent-function 'common-lisp-indent-function
      lisp-loop-indent-subclauses nil
      lisp-loop-indent-forms-like-keywords t
      lisp-lambda-list-keyword-parameter-alignment t)

(defun slime-repl-return-at-end ()
  (interactive)
  (if (<= (point-max) (point))
      (slime-repl-return)
      (slime-repl-newline-and-indent)))

(defun set-slime-repl-return ()
  (define-key slime-repl-mode-map (kbd "RET") 'slime-repl-return-at-end)
  (define-key slime-repl-mode-map (kbd "<return>") 'slime-repl-return-at-end))

(add-hook 'slime-repl-mode-hook 'set-slime-repl-return)


(provide 'wtleeiv-lisp)
