;;; colonq-projectile --- project management -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'colonq-package)

(use-package projectile
  :custom
  (projectile-completion-system 'default)
  :config
  (projectile-mode)
  (defun colonq/projectile-project-root-wrapper (f &rest args)
    "Wrapper around F (projectile-project-root), passing ARGS."
    (unless (file-remote-p default-directory)
      (apply f args)))
  (advice-add 'projectile-project-root :around 'colonq/projectile-project-root-wrapper)
  (setf (cdr projectile-mode-map) nil))

(provide 'colonq-projectile)
;;; colonq-projectile.el ends here
