;;; colonq-theme --- UI theme -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'colonq-package)
(require 'colonq-hydra)

(set-frame-font "Iosevka Comfy:pixelsize=20")
(set-face-font 'default "Iosevka Comfy:pixelsize=20")

(set-fontset-font
 t 'symbol
 (font-spec
  :family "Noto Color Emoji"
  :size 18
  :weight 'normal
  :width 'normal
  :slant 'normal))

(use-package whitespace
  :config
  (setq whitespace-style '(face tabs))
  (whitespace-mode))

(use-package moe-theme
  :config
  (setq moe-theme-mode-line-color 'w/b)
  (moe-dark))

(use-package outshine
  :config
  (add-hook 'outline-minor-mode-hook 'outshine-mode))

(provide 'colonq-theme)
;;; colonq-theme ends here
