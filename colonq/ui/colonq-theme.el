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

;; (load-theme 'ef-rosa t)

(use-package ef-themes
  :load-path "~/src/ef-themes"
  :config
  ;; (ef-themes-select 'ef-rosa)
  ;; (ef-themes-select 'ef-maris-dark)
  ;; (ef-themes-select 'ef-tritanopia-dark)
  ;; (ef-themes-select 'ef-duo-dark)
  (ef-themes-select 'ef-bio)
  ;; (ef-themes-select 'ef-autumn)
  (ef-themes-with-colors
    (setenv "COLONQ_BGCOLOR" bg-main)
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
