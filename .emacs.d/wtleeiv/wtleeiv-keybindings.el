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

(require 'magit)
(setq magit-no-confirm '(stage-all-changes unstage-all-changes))
(global-set-key (kbd "C-c g") 'magit-status)

(require 'expand-region)
(global-set-key (kbd "C-q") 'er/expand-region)
;; Workaround for https://github.com/magnars/expand-region.el/issues/220
(setq shift-select-mode nil)

(require 'neotree)
(global-set-key (kbd "C-c d") 'neotree-toggle)

(setq xah-fly-use-control-key nil)
(require 'xah-fly-keys)
(xah-fly-keys-set-layout "programer-dvorak")
(xah-fly-keys 1)
(define-key xah-fly-key-map (kbd "<home>") 'xah-fly-mode-toggle)
(add-hook 'shell-mode-hook 'xah-fly-insert-mode-activate)
(add-hook 'magit-mode-hook 'xah-fly-insert-mode-activate)
(add-hook 'dired-mode-hook 'xah-fly-insert-mode-activate)

(provide 'wtleeiv-keybindings)
