;;; colonq-fennel --- Fennel support -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'colonq-package)
(require 'colonq-hydra)

(use-package fennel-mode
  :config
  (defhydra colonq/ide-fennel (:color teal :hint nil)
    "Dispatcher > Fennel IDE"
    ("<f12>" keyboard-escape-quit)
    ("r" fennel-repl "repl")
    ("RET" fennel-reload "load")
    ("D" fennel-find-definition "goto def"))
  (defun colonq/fennel-setup ()
    "Custom configuration for Fennel programming."
    (setq-local colonq/contextual-ide 'colonq/ide-fennel/body))
  (add-hook 'fennel-mode-hook #'colonq/fennel-setup))

(provide 'colonq-fennel)
;;; colonq-fennel.el ends here
