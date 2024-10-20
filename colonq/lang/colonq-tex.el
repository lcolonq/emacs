;;; colonq-tex --- TeX support -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'colonq-package)
(require 'colonq-hydra)

;; (use-package company-auctex)

(use-package tex
  :config
  (TeX-engine-set 'luatex)
  (defhydra colonq/ide-tex (:color teal :hint nil)
    "Dispatcher > TeX IDE"
    ("<f12>" keyboard-escape-quit))
  (defun colonq/tex-setup ()
    (setq-local colonq/contextual-ide 'colonq/ide-tex/body)
    ;; (company-auctex-init)
    )
  (add-hook 'TeX-mode-hook #'colonq/tex-setup))

(provide 'colonq-tex)
;;; colonq-tex.el ends here
