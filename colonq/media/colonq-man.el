;;; colonq-man --- man pages and other documentation -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'colonq-package)
(require 'colonq-hydra)

(use-package man
  :custom
  (Man-notify-method 'pushy))

(use-package info
  :config
  (define-key Info-mode-map (kbd "q") nil)
  (define-key Info-mode-map (kbd "w") nil)
  (define-key Info-mode-map (kbd "b") nil)
  (define-key Info-mode-map (kbd "g") nil)
  (define-key Info-mode-map (kbd "G") nil)
  (set-face-attribute 'Info-quoted nil :inherit nil)
  (set-face-attribute 'info-menu-star nil :foreground nil))

(provide 'colonq-man)
;;; colonq-man ends here
