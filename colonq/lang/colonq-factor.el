;;; colonq-factor --- Factor support -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'f)
(require 'colonq-package)
(require 'colonq-hydra)

(use-package factor-mode
  :config
  (defhydra colonq/ide-factor-debug (:color teal :hint nil)
    "Dispatcher > Factor Debug"
    ("<f12>" keyboard-escape-quit)
    ("u" fuel-debug-update-usings "using")
    ("1" (fuel-debug-exec-restart 1))
    ("2" (fuel-debug-exec-restart 2))
    ("3" (fuel-debug-exec-restart 3))
    ("4" (fuel-debug-exec-restart 4))
    ("5" (fuel-debug-exec-restart 5))
    ("6" (fuel-debug-exec-restart 6))
    ("7" (fuel-debug-exec-restart 7))
    ("8" (fuel-debug-exec-restart 8))
    ("9" (fuel-debug-exec-restart 9)))
  (defun colonq/factor-debug-setup ()
    (setq-local colonq/contextual-ide 'colonq/ide-factor-debug/body))
  (add-hook 'fuel-debug-mode-hook #'colonq/factor-debug-setup)
  ;; this function was broken combined with hl-line, so let's redefine
  (defun factor-on-vocab ()
    "t if point is on a vocab name. We just piggyback on
  font-lock's pretty accurate information."
    (eq (plist-get (text-properties-at (point)) 'face) 'factor-font-lock-vocabulary-name))
  (defun colonq/factor-lookup ()
    (interactive)
    (if (factor-on-vocab)
        (fuel-help--get-vocab (factor-symbol-at-point))
      (fuel-help--word-help (factor-symbol-at-point))))
  (defun colonq/start-or-switch-to-factor ()
    (interactive)
    (colonq/factor-refresh)
    (if (and fuel-listener--buffer (buffer-live-p fuel-listener--buffer))
        (switch-to-buffer fuel-listener--buffer)
      (run-factor)))
  (defhydra colonq/ide-factor (:color teal :hint nil)
    "Dispatcher > Factor IDE"
    ("<f12>" keyboard-escape-quit)
    ("S" run-factor "start")
    ("i" fuel-run-file "buffer")
    ("r" colonq/start-or-switch-to-factor "repl"))
  (defun colonq/factor-refresh ()
    (interactive)
    (when-let ((root (getenv "FACTOR_ROOT")))
      (setq
       fuel-factor-root-dir root
       fuel-listener-factor-binary (f-join root "bin/factor")
       fuel-listener-factor-image (f-join root "lib/factor/factor.image"))))
  (defun colonq/factor-setup ()
    (colonq/factor-refresh)
    ;; (electric-indent-local-mode -1)
    (setq-local colonq/contextual-ide #'colonq/ide-factor/body)
    (setq-local colonq/contextual-lookup #'colonq/factor-lookup)
    (setq-local flycheck-check-syntax-automatically '(save mode-enabled)))
  (add-hook 'factor-mode-hook #'colonq/factor-setup)
  (defun colonq/factor-listener-setup ()
    (colonq/factor-refresh)
    (setq-local colonq/contextual-lookup #'colonq/factor-lookup))
  (add-hook 'fuel-listener-mode-hook #'colonq/factor-listener-setup))

(provide 'colonq-factor)
;;; colonq-factor.el ends here
