;;; colonq-purescript --- Purescript support -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'colonq-package)
(require 'colonq-hydra)

(use-package purescript-indentation)

(use-package purescript-mode :mode "\\.purs\\'"
  :config
  (defhydra colonq/ide-purescript (:color teal :hint nil)
    "Quinine > Purescript IDE"
    ("<f12>" keyboard-escape-quit)
    ("S" lsp-workspace-restart "start")
    ("i" lsp-execute-code-action "poke")
    ("D" xref-find-definitions "goto def")
    ("R" xref-find-references "goto refs"))
  (defun colonq/purescript-setup ()
    (setq-local colonq/contextual-ide 'colonq/ide-purescript/body)
    (turn-on-purescript-indentation)
    (lsp))
  (add-hook 'purescript-mode-hook 'colonq/purescript-setup))

(provide 'colonq-purescript)
;;; colonq-purescript ends here
