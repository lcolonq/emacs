;;; colonq-xml --- XML language support -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'colonq-package)
(require 'colonq-hydra)

(use-package nxml-mode
  :config
  (define-key nxml-mode-map (kbd "M-h") nil)
  (define-key nxml-mode-map (kbd "M-l") nil)
  (define-key nxml-mode-map (kbd "M-k") nil)
  (define-key nxml-mode-map (kbd "M-j") nil))

(provide 'colonq-xml)
;;; colonq-xml ends here
