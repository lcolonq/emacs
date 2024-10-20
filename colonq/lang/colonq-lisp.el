;;; colonq-lisp --- Common Lisp support -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'colonq-package)
(require 'colonq-hydra)
(require 'colonq-dispatcher)
(require 'colonq-flycheck)

(defun colonq/completion-backend-lisp ()
  "Build a new completion backend for Common Lisp."
  (o/make-backend
   :name "Common Lisp"
   :function
   (lambda (prefix)
     (let ((comps
            (with-temp-buffer
              (insert prefix)
              (sly-complete-symbol))))
       (--map
        (o/make-candidate :string (substring-no-properties it))
        (all-completions prefix (caddr comps)))))))

(use-package sly
  :load-path "~/src/sly"
  :custom
  (inferior-lisp-program "sbcl")
  (sly-command-switch-to-existing-lisp 'always)
  (sly-complete-symbol-function #'sly-simple-completions)
  (sly-net-coding-system 'utf-8-unix)
  :config
  (eval-after-load 'sly-mrepl
    '(progn
       (evil-define-key 'insert sly-mrepl-mode-map
         (kbd "<up>") #'sly-mrepl-previous-input-or-button
         (kbd "<down>") #'sly-mrepl-next-input-or-button)
       (define-key sly-mrepl-mode-map (kbd "C-l") #'sly-mrepl-clear-repl)
       (define-key sly-mrepl-mode-map (kbd "C-d") #'kill-this-buffer)
       (defun colonq/lisp-repl-setup ()
         (o/mode)
         (setq o/backends '(colonq/completion-backend-lisp))
         (setq-local colonq/contextual-lookup #'sly-documentation))
       (add-hook 'sly-mrepl-mode-hook #'colonq/lisp-repl-setup)))
  
  (defhydra colonq/ide-lisp (:color teal :hint nil)
    "Dispatcher > Lisp IDE"
    ("<f12>" keyboard-escape-quit)
    ("S" sly-restart-inferior-lisp "start")
    ("e" sly-eval-defun "eval")
    ("i" sly-eval-buffer "buffer")
    ("r" sly "repl"))
  (defun colonq/lisp-setup ()
    (o/mode)
    (setq o/backends '(colonq/completion-backend-lisp))
    (setq-local colonq/contextual-lookup #'sly-documentation)
    (setq-local colonq/contextual-ide 'colonq/ide-lisp/body))
  (add-hook 'sly-mode-hook #'colonq/lisp-setup))

(provide 'colonq-lisp)
;;; colonq-lisp.el ends here
