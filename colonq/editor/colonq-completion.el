;;; colonq-completion --- autocompletion -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'colonq-package)

;; (use-package company
;;   :custom
;;   (company-idle-delay 0)
;;   (company-frontends '(company-pseudo-tooltip-frontend))
;;   :config
;;   (define-key company-active-map (kbd "<f12>") 'company-abort)
;;   (define-key company-active-map (kbd "<tab>") (lambda () (interactive) (company-complete-common) (company-select-next)))
;;   (define-key company-active-map (kbd "<backtab>") 'company-select-previous)
;;   (global-company-mode))
;; 
;; (use-package company-pcomplete :after company
;;   :custom
;;   (pcomplete-expand-before-complete nil)
;;   (pcomplete-cycle-completions nil))

(use-package othonoi :load-path "~/src/othonoi")

(provide 'colonq-completion)
;;; colonq-completion.el ends here
