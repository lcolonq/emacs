;;; colonq-c --- C/C++ support -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'colonq-package)
(require 'colonq-hydra)
(require 'colonq-flycheck)
(require 'colonq-company)

(push '(gdb-source . writable) window-persistent-parameters)
(defun colonq/get-gdb-source-window ()
  "Get the marked GDB source window."
  (-first (lambda (w) (window-parameter w 'gdb-source)) (window-list)))
(defun colonq/set-gdb-source-window ()
  "Set the marked GDB source window to the current window."
  (interactive)
  (set-window-parameter (get-buffer-window (current-buffer)) 'gdb-source t))

(defhydra colonq/ide-c (:color teal :hint nil)
  "Dispatcher > C IDE"
  ("<escape>" keyboard-escape-quit)
  ("i" gud-break "break")
  ("I" gud-remove)
  ("r" gdb "debugger"))

(defun colonq/c-setup ()
  "Custom configuration for C programming."
  (c-set-style "linux")
  (hl-line-mode)
  (flycheck-inline-mode)
  (setq-local indent-tabs-mode t)
  (setq-local tab-width 4)
  (setq-local c-basic-offset 4)
  (setq-local company-backends '((company-dabbrev-code company-keywords)))
  (setq-local colonq/contextual-ide #'colonq/ide-c/body)
  (define-key c-mode-map (kbd "M-h") #'windmove-left)
  (define-key c-mode-map (kbd "M-l") #'windmove-right)
  (define-key c-mode-map (kbd "M-k") #'windmove-up)
  (define-key c-mode-map (kbd "M-j") #'windmove-down)
  (when (projectile-project-root)
    (setq-local flycheck-gcc-include-path (list (concat (projectile-project-root) "include")))))
(add-hook 'c-mode-hook 'colonq/c-setup)

(provide 'colonq-c)
;;; colonq-c ends here
