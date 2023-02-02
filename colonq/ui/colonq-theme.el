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

(use-package ef-themes
  :config
  (ef-themes-select 'ef-tritanopia-dark)
  (ef-themes-with-colors
    (set-face-attribute 'vertical-border nil
                        :foreground bg-alt
                        :background bg-alt)
    (set-face-attribute 'fringe nil
                        :foreground bg-alt
                        :background bg-alt)))

(use-package outshine
  :config
  (add-hook 'outline-minor-mode-hook 'outshine-mode))

(provide 'colonq-theme)
;;; colonq-theme.el ends here
