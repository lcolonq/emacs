;;; colonq-c --- C/C++ support -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'colonq-package)
(require 'colonq-hydra)
(require 'colonq-flycheck)
(require 'colonq-company)

(defun colonq/c-setup ()
  "Custom configuration for C programming."
  (c-set-style "linux")
  (flycheck-inline-mode)
  (setq-local indent-tabs-mode t)
  (setq-local tab-width 4)
  (setq-local c-basic-offset 4)
  (setq-local company-backends '((company-dabbrev-code company-keywords)))
  (define-key c-mode-map (kbd "M-h") #'windmove-left)
  (define-key c-mode-map (kbd "M-l") #'windmove-right)
  (define-key c-mode-map (kbd "M-k") #'windmove-up)
  (define-key c-mode-map (kbd "M-j") #'windmove-down)
  (when (projectile-project-root)
    (setq-local flycheck-gcc-include-path (list (concat (projectile-project-root) "include")))))
(add-hook 'c-mode-hook 'colonq/c-setup)

(provide 'colonq-c)
;;; colonq-c ends here