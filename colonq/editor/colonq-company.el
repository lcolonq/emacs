;;; colonq-company --- autocompletion -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'colonq-package)

(use-package company
  :custom
  (company-idle-delay 0)
  (company-frontends '(company-pseudo-tooltip-unless-just-one-frontend company-preview-frontend))
  :config
  (define-key company-active-map (kbd "<escape>") 'company-abort)
  (define-key company-active-map (kbd "<tab>") (lambda () (interactive) (company-complete-common) (company-select-next)))
  (define-key company-active-map (kbd "<backtab>") 'company-select-previous)
  (global-company-mode))

(use-package company-pcomplete :after company
  :custom
  (pcomplete-expand-before-complete nil)
  (pcomplete-cycle-completions nil))

(provide 'colonq-company)
;;; colonq-company.el ends here
