(require 'org)
(require 'org-habit)
(require 'org-agenda)
(setq org-directory "~/Dropbox/org/"
      org-default-notes-file (concat org-directory "scratch.org")
      org-agenda-files (list (concat org-directory "habits.org")
			     org-default-notes-file)
      org-src-fontify-natively t
      org-src-tab-acts-natively t
      org-confirm-babel-evaluate nil
      org-src-preserve-indentation t
      org-refile-targets '((org-agenda-files . (:level . 1)))
      org-agenda-custom-commands '(("n" "Agenda and TODOs"
				    ((agenda "")
				     (tags-todo "-habits"))))
      org-capture-templates `(("s" "Scratch" entry (file org-default-notes-file)
			           "* %?")
			      ("l" "Learnal" entry (file+olp+datetree ,(concat org-directory "learnal.org"))
			           "* %?" :tree-type week)))

(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c l") 'org-store-link)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (ipython . t)
   (lisp . t)))

(provide 'wtleeiv-org)
