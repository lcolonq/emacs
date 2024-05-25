;;; colonq-python --- Python support -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'colonq-package)
(require 'colonq-hydra)

(use-package poetry)

(use-package lsp-python-ms
  :init
  (setq lsp-python-ms-executable (executable-find "python-language-server"))
  :config
  (defhydra colonq/ide-python (:color teal :hint nil)
    "Dispatcher > Python IDE"
    ("<f12>" keyboard-escape-quit)
    ("S" lsp-workspace-restart "start")
    ("v" poetry-venv-workon "venv")
    ("i" lsp-execute-code-action "poke")
    ("D" xref-find-definitions "goto def")
    ("R" xref-find-references "goto refs"))
  (defun colonq/python-setup ()
    ;; (when (poetry-venv-exist-p)
    ;;   (poetry-venv-workon))
    (lsp-deferred)
    (yas-minor-mode)
    (setq-local lsp-eldoc-hook nil)
    (setq-local colonq/contextual-ide #'colonq/ide-python/body))
  (add-hook 'python-mode-hook #'colonq/python-setup))

(provide 'colonq-python)
;;; colonq-python ends here
