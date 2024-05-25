;;; colonq-rust --- Rust support -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'colonq-package)
(require 'colonq-hydra)

(use-package rustic
  :mode ("\\.rs\\'" . rustic-mode)
  :config
  (defhydra colonq/ide-rust (:color teal :hint nil)
    "Dispatcher > Rust IDE"
    ("<f12>" keyboard-escape-quit)
    ("S" lsp-workspace-restart "start")
    ("i" lsp-execute-code-action "poke")
    ("e" flycheck-next-error "goto err")
    ("r" lsp-rename "rename")
    ("D" xref-find-definitions "goto def")
    ("R" xref-find-references "goto refs"))
  (defun colonq/rust-setup ()
    (setq-local lsp-eldoc-hook nil)
    (setq-local colonq/contextual-ide 'colonq/ide-rust/body)
    (setq-local flycheck-check-syntax-automatically '(save mode-enabled)))
  (add-hook 'rustic-mode-hook #'colonq/rust-setup))

(provide 'colonq-rust)
;;; colonq-rust.el ends here
