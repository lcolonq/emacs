;;; colonq-ocaml --- OCaml support -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package tuareg
  :mode ("\\.ml\\'" . tuareg-mode)
  :load-path "~/src/tuareg"
  :config
  (defhydra colonq/ide-ocaml (:color teal :hint nil)
    "Dispatcher > OCaml IDE"
    ("<f12>" keyboard-escape-quit)
    ("S" lsp-workspace-restart "start")
    ("i" lsp-execute-code-action "poke")
    ("D" xref-find-definitions "goto def")
    ("R" xref-find-references "goto refs"))
  (defun colonq/ocaml-setup ()
    (setq-local xref-prompt-for-identifier nil)
    (setq-local colonq/contextual-ide 'colonq/ide-ocaml/body)
    (lsp))
  (add-hook 'tuareg-mode-hook #'colonq/ocaml-setup))

(provide 'colonq-ocaml)
;;; colonq-ocaml.el ends here
