(require 'magit)

(setq magit-no-confirm '(stage-all-changes unstage-all-changes))
;; no style conventions
(setq git-commit-fill-column 9999)
(setq git-commit-summary-max-length 9999)
(setq git-commit-finish-query-functions nil)

(provide 'wtleeiv-magit)
