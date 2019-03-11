(global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "M-1") 'delete-other-windows)
(global-set-key (kbd "M-2") 'split-window-below)
(global-set-key (kbd "M-3") 'split-window-right)
(global-set-key (kbd "M-0") 'delete-window)
;; move windows with S-<arrow>
(windmove-default-keybindings)

(defun delete-this-buffer-and-file ()
  "Removes file connected to current buffer and kills buffer."
  (interactive)
  (let ((filename (buffer-file-name))
        (buffer (current-buffer))
        (name (buffer-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
        (when (yes-or-no-p "Are you sure you want to remove this file? ")
          (delete-file filename)
          (kill-buffer buffer)
          (message "File '%s' successfully removed" filename)))))

(global-set-key (kbd "C-c k") 'delete-this-buffer-and-file)

(global-set-key (kbd "C-s") 'swiper)
(global-set-key (kbd "C-c f") 'counsel-rg)

(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c l") 'org-store-link)

(require 'magit)
(setq magit-no-confirm '(stage-all-changes unstage-all-changes))
(global-set-key (kbd "C-c g") 'magit-status)

(require 'expand-region)
(global-set-key (kbd "C-q") 'er/expand-region)
;; Workaround for https://github.com/magnars/expand-region.el/issues/220
(setq shift-select-mode nil)

(provide 'wtleeiv-keybindings)
