;;; colonq-flycheck --- program analysis -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'colonq-package)

(use-package flycheck
  :custom
  (flycheck-display-errors-function (lambda (errors) (ignore errors) nil))
  (eldoc-idle-delay 0.0)
  (flycheck-display-errors-delay 0.1)
  (flycheck-emacs-lisp-load-path 'inherit)
  (flycheck-check-syntax-automatically '(save mode-enabled))
  :config
  (setf (cdr flycheck-mode-map) nil)
  (global-flycheck-mode))

(use-package quick-peek)

(use-package flycheck-inline
  :config
  (setq flycheck-inline-display-function
        (lambda (msg &optional pos err)
          (ignore err)
          (set-text-properties 0 (length msg) nil msg)
          (let* ((ov (quick-peek-overlay-ensure-at pos))
                 (contents (quick-peek-overlay-contents ov)))
            (setf (quick-peek-overlay-contents ov)
                  (concat contents (when contents "\n") msg))
            (quick-peek-update ov)))
        flycheck-inline-clear-function #'quick-peek-hide)
  (global-flycheck-inline-mode))

(provide 'colonq-flycheck)
;;; colonq-flycheck.el ends here
