;;; colonq-forth --- Forth support -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'colonq-package)
(require 'colonq-hydra)

(setq forth-interaction-buffer nil)

(defun colonq/forth-help (word)
  "Display help for WORD."
  (interactive (list (forth-word-at-point)))
  (let ((buffer (get-buffer-create "*forth-help*")))
    (pop-to-buffer buffer)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert (forth-interaction-send "help " word)))
    (special-mode)))

(use-package forth-mode
  :config
  (setq forth-executable "gforth")
  (evil-define-key 'insert forth-mode-map (kbd "RET") #'newline-and-indent)
  (evil-define-key 'insert forth-mode-map (kbd "<tab>") #'company-complete)
  (defun colonq/start-or-switch-to-forth ()
    (interactive)
    (if (and forth-interaction-buffer (buffer-live-p forth-interaction-buffer))
        (forth-switch-to-output-buffer)
      (run-forth)))
  (defhydra colonq/ide-forth (:color teal :hint nil)
    "Dispatcher > Forth IDE"
    ("<f12>" keyboard-escape-quit)
    ("S" forth-restart "restart")
    ("e" forth-eval-defun "eval")
    ("i" forth-load-file "buffer")
    ("r" colonq/start-or-switch-to-forth "repl"))
  (defun colonq/forth-setup ()
    (electric-indent-local-mode -1)
    (setq-local colonq/contextual-lookup #'colonq/forth-help)
    (setq-local colonq/contextual-ide #'colonq/ide-forth/body)
    (setq-local flycheck-check-syntax-automatically '(save mode-enabled)))
  (add-hook 'forth-mode-hook #'colonq/forth-setup)
  (defhydra colonq/ide-forth-interaction (:color teal :hint nil)
    "Dispatcher > Forth IDE"
    ("<f12>" keyboard-escape-quit)
    ("S" forth-restart "restart")
    ("r" forth-switch-to-source-buffer "repl"))
  (defun colonq/forth-interaction-setup ()
    (setq-local colonq/contextual-ide 'colonq/ide-forth-interaction/body))
  (add-hook 'forth-interaction-mode-hook #'colonq/forth-interaction-setup))

(provide 'colonq-forth)
;;; colonq-forth.el ends here
