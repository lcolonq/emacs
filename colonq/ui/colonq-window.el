;;; colonq-window --- window configuration -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'colonq-package)
(require 'colonq-hydra)

(use-package shackle
  :custom
  (shackle-default-rule '(:same t))
  (pop-up-windows nil)
  (even-window-heights nil)
  :config
  (setq shackle-default-rule '(:same t :inhibit-window-quit t))
  (shackle-mode))

(use-package eyebrowse
  :config
  (define-key eyebrowse-mode-map (kbd "C-c") nil)
  (eyebrowse-mode))

(use-package buffer-move
  :config
  (global-set-key (kbd "M-H") #'buf-move-left)
  (global-set-key (kbd "M-L") #'buf-move-right)
  (global-set-key (kbd "M-K") #'buf-move-up)
  (global-set-key (kbd "M-J") #'buf-move-down))

(provide 'colonq-window)
;;; colonq-window ends here
