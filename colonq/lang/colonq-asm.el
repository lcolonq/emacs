;;; colonq-asm --- Support for various assemblers -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'colonq-package)
(require 'colonq-hydra)
(require 'colonq-flycheck)
(require 'colonq-company)

(use-package asm-mode
  :config
  (defun asm-colon ()
    "Insert a colon; if it follows a label, delete the label's indentation."
    (interactive)
    (let ((labelp nil))
      (save-excursion
        (skip-syntax-backward "w_")
        (skip-syntax-backward " ")
        (if (setq labelp (bolp)) (delete-horizontal-space)))
      (call-interactively 'self-insert-command)
      (when labelp
        (delete-horizontal-space)
        (company-cancel))))

  (defun colonq/asm-backtab ()
    "De-indent line."
    (interactive)
    (move-beginning-of-line nil)
    (delete-horizontal-space))

  (defun colonq/asm-setup ()
    "Custom configuration for assembly programming."
    (flycheck-inline-mode)
    (setq-local indent-tabs-mode t)
    (setq-local tab-width 8)
    (setq-local company-backends '((company-dabbrev-code)))
    (setq-local comment-start "# ")
    (electric-indent-local-mode -1)
    (local-unset-key (vector asm-comment-char))
    (evil-define-key 'normal asm-mode-map (kbd "<backtab>") #'colonq/asm-backtab)
    (evil-define-key 'insert asm-mode-map (kbd "<backtab>") #'colonq/asm-backtab))
  (add-hook 'asm-mode-hook #'colonq/asm-setup))

(provide 'colonq-asm)
;;; colonq-asm ends here
