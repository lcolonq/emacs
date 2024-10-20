;;; colonq-evil --- modal editing -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'colonq-package)

(use-package undo-tree
  :custom
  (undo-tree-auto-save-history nil)
  :config
  (global-undo-tree-mode))

(use-package evil
  :custom
  (evil-auto-balance-windows nil)
  (evil-want-abbrev-expand-on-insert-exit nil)
  (evil-want-minibuffer t)
  (evil-undo-system 'undo-tree)
  :config
  (evil-mode)

  (setq evil-emacs-state-modes '())
  (evil-set-initial-state 'eshell-mode 'normal)
  (evil-set-initial-state 'term-mode 'normal)
  (evil-set-initial-state 'image-mode 'motion)
  (evil-set-initial-state 'special-mode 'motion)
  (evil-set-initial-state 'pdf-view-mode 'motion)
  (evil-set-initial-state 'org-agenda-mode 'motion)
  (evil-set-initial-state 'compilation-mode 'motion)
  (evil-set-initial-state 'grep-mode 'motion)
  (evil-set-initial-state 'Info-mode 'motion)
  (evil-set-initial-state 'magit--mode 'motion)
  (evil-set-initial-state 'magit-status-mode 'motion)
  (evil-set-initial-state 'magit-diff-mode 'motion)
  (evil-set-initial-state 'magit-stashes-mode 'motion)
  (evil-set-initial-state 'epa-key-list-mode 'motion)
  (evil-set-initial-state 'fuel-debug-mode 'motion)

  (evil-set-initial-state 'magit-popup-mode 'emacs)
  (evil-set-initial-state 'exwm-mode 'emacs)

  (define-key evil-motion-state-map (kbd "RET") nil)

  (define-key evil-normal-state-map (kbd ":") nil)
  (define-key evil-motion-state-map (kbd ":") nil)
  (define-key evil-visual-state-map (kbd ":") nil)

  (define-key evil-normal-state-map (kbd "m") 'evil-record-macro)

  (define-key evil-normal-state-map (kbd "0") 'evil-first-non-blank)
  (define-key evil-motion-state-map (kbd "0") 'evil-first-non-blank)
  (define-key evil-visual-state-map (kbd "0") 'evil-first-non-blank)

  (define-key evil-normal-state-map (kbd "C-z") nil)
  (define-key evil-insert-state-map (kbd "C-z") nil)
  (define-key evil-motion-state-map (kbd "C-z") nil)
  (define-key evil-visual-state-map (kbd "C-z") nil)
  (define-key evil-replace-state-map (kbd "C-z") nil)
  (define-key evil-operator-state-map (kbd "C-z") nil)

  (define-key evil-normal-state-map (kbd "C-d") nil)
  (define-key evil-insert-state-map (kbd "C-d") nil)
  (define-key evil-motion-state-map (kbd "C-d") nil)
  (define-key evil-visual-state-map (kbd "C-d") nil)
  (define-key evil-replace-state-map (kbd "C-d") nil)
  (define-key evil-operator-state-map (kbd "C-d") nil)

  (define-key evil-emacs-state-map (kbd "q") 'colonq/dispatcher)
  (define-key evil-normal-state-map (kbd "q") 'colonq/dispatcher)
  (define-key evil-motion-state-map (kbd "q") 'colonq/dispatcher)

  (define-key evil-emacs-state-map (kbd "Q") 'colonq/other-monitor)
  (define-key evil-normal-state-map (kbd "Q") 'colonq/other-monitor)
  (define-key evil-motion-state-map (kbd "Q") 'colonq/other-monitor)

  (define-key evil-normal-state-map (kbd "#") #'comment-dwim)
  (define-key evil-visual-state-map (kbd "#") #'comment-dwim)

  (define-key read-passwd-map (kbd "C-v") 'evil-paste-after))

(provide 'colonq-evil)
;;; colonq-evil.el ends here
