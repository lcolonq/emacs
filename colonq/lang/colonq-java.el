;;; colonq-java --- Java support -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'colonq-package)
(require 'colonq-hydra)

(use-package lsp-java
  :config
  (defhydra colonq/ide-java (:color teal :hint nil)
    "Dispatcher > Java IDE"
    ("<f12>" keyboard-escape-quit)
    ("S" lsp-workspace-restart "start")
    ("i" lsp-execute-code-action "poke")
    ("e" flycheck-next-error "goto err")
    ("r" lsp-rename "rename")
    ("D" xref-find-definitions "goto def")
    ("R" xref-find-references "goto refs"))
  (defun colonq/java-setup ()
    (setq-local lsp-eldoc-hook nil)
    (setq-local colonq/contextual-ide 'colonq/ide-java/body)
    (setq-local flycheck-check-syntax-automatically '(save mode-enabled))
    (lsp))
  (add-hook 'java-mode-hook #'colonq/java-setup))

(provide 'colonq-java)
;;; colonq-java.el ends here
