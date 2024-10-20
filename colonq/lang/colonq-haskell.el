;;; colonq-haskell --- Haskell support -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'colonq-package)
(require 'colonq-hydra)
(require 'colonq-dispatcher)
(require 'colonq-flycheck)

(use-package haskell)

(use-package lsp-haskell
  :config
  (add-to-list 'lsp-file-watch-ignored "[/\\\\]\\dist-newstyle$" t)
  (add-to-list 'lsp-file-watch-ignored "[/\\\\]\\deps$" t)
  ;; (flycheck-add-next-checker 'lsp '(warning . haskell-hlint))
  (defun colonq/haskell-visit-package-yaml ()
    (interactive)
    (find-file (f-join (haskell-cabal-find-dir) "package.yaml")))
  (defhydra colonq/ide-haskell (:color teal :hint nil)
    "Dispatcher > Haskell IDE"
    ("<f12>" keyboard-escape-quit)
    ("S" lsp-workspace-restart "start")
    ("i" lsp-execute-code-action "poke")
    ("D" xref-find-definitions "goto def")
    ("R" xref-find-references "goto refs")
    ("I" haskell-navigate-imports "goto imports")
    ("C" haskell-cabal-visit-file "goto package.yaml"))
  (defun colonq/haskell-setup ()
    (setq-local xref-prompt-for-identifier nil)
    (setq-local colonq/contextual-ide 'colonq/ide-haskell/body)
    (setq-local flycheck-check-syntax-automatically '(save mode-enabled))
    ;; (lsp)
    (flycheck-mode -1)
    ;; (company-mode -1)
    )
  (add-hook 'haskell-mode-hook #'colonq/haskell-setup))

;; (use-package haskell-mode :mode "\\.hs\\'"
;;   :config
;;   (defhydra colonq/ide-haskell (:color teal :hint nil)
;;     "Dispatcher > Haskell IDE"
;;     ("<escape>" keyboard-escape-quit)
;;     ;; ("S" dante-restart "restart")
;;     ("D" xref-find-definitions "goto def")
;;     ("R" xref-find-references "goto refs")
;;     ("i" haskell-compile "compile")
;;     ("I" haskell-navigate-imports "goto imports")
;;     ("C" haskell-cabal-visit-file "goto cabal"))
;;   (defun colonq/haskell-setup ()
;;     (setq-local xref-prompt-for-identifier nil)
;;     (setq-local colonq/contextual-ide #'colonq/ide-haskell/body)
;;     ;; (setq-local colonq/contextual-lookup #'dante-doc)
;;     (setq-local flycheck-check-syntax-automatically '(save mode-enabled))
;;     (flycheck-mode -1)
;;     ;; (dante-mode)
;;     )
;;   (add-hook 'haskell-mode-hook #'colonq/haskell-setup)
;;   )

;; (defhydra colonq/ide-haskell (:color teal :hint nil)
;;   "Dispatcher > Haskell IDE"
;;   ("<f12>" keyboard-escape-quit)
;;   ("S" eglot-reconnect "start")
;;   ("i" eglot-code-actions "poke")
;;   ("D" xref-find-definitions "goto def")
;;   ("R" xref-find-references "goto refs")
;;   ("I" haskell-navigate-imports "goto imports")
;;   ("C" haskell-cabal-visit-file "goto cabal"))
;; (defun colonq/haskell-setup ()
;;   (setq-local eldoc-idle-delay 0.5)
;;   (setq-local eldoc-documentation-strategy 'eldoc-documentation-compose)
;;   (setq-local xref-prompt-for-identifier nil)
;;   (setq-local colonq/contextual-ide 'colonq/ide-haskell/body)
;;   (setq-local
;;    eldoc-display-functions
;;    '(colonq/eldoc-display-quick-peek))
;;   ;; (eglot-ensure)
;;   (flycheck-mode -1)
;;   (company-mode -1)
;;   (add-hook 'post-command-hook #'quick-peek-hide nil 'local)
;;   )
;; (add-hook 'haskell-mode-hook #'colonq/haskell-setup)

(provide 'colonq-haskell)
;;; colonq-haskell.el ends here
