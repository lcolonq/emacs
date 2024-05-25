;;; colonq-elisp --- Emacs Lisp support -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'colonq-package)
(require 'colonq-hydra)

(defhydra colonq/ide-elisp (:color teal :hint nil)
  "Dispatcher > ELisp IDE"
  ("<f12>" keyboard-escape-quit)
  ("e" eval-defun "eval")
  ("i" eval-buffer "buffer")
  ("r" ielm "repl"))

(defun colonq/elisp-setup ()
  "Custom configuration for ELisp programming."
  (outline-minor-mode)
  (setq-local colonq/contextual-ide #'colonq/ide-elisp/body)
  (setq-local colonq/contextual-lookup #'selector-apropos)
  (setq-local colonq/contextual-hyper-state-bindings
              '(("h" . lispy-left)
                ("l" . lispy-right)
                ("k" . lispy-up)
                ("j" . lispy-down)
                ("f" . lispy-forward)
                ("b" . lispy-back))))
(add-hook 'emacs-lisp-mode-hook 'colonq/elisp-setup)
(add-hook 'ielm-mode-hook 'colonq/elisp-setup)

(provide 'colonq-elisp)
;;; colonq-elisp ends here
