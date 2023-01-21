;;; colonq-nix --- Nix support -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'colonq-package)
(require 'colonq-hydra)

(defun colonq/home-manager-switch ()
  "Run home-manager switch in a `compilation-mode' buffer."
  (interactive)
  (switch-to-buffer (compilation-start "cd ~; home-manager switch"))
  (rename-buffer "*home-manager-switch*"))

(use-package nix-prettify-mode
  :config
  (add-to-list 'nix-prettify-special-modes 'eshell-mode)
  (nix-prettify-global-mode))

(use-package nix-mode
  :config
  (defhydra colonq/nix-dispatcher (:color teal :hint nil :body-pre (setq exwm-input-line-mode-passthrough t) :post (setq exwm-input-line-mode-passthrough nil))
    "Dispatcher > Nix"
    ("<escape>" 'keyboard-escape-quit)
    ("x" nix-repl "repl")
    ("h" colonq/home-manager-switch "home-manager")
    ("b" build-farm "build farm")))

(provide 'colonq-nix)
;;; colonq-nix.el ends here
