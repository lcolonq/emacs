;;; colonq-web --- web development language support -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'colonq-package)
(require 'colonq-hydra)

(use-package js2-mode :mode "\\.js\\'")

(use-package json-mode :mode "\\.json\\'")

(use-package web-mode :mode ("\\.html\\'" "\\.php\\'"))

;; (defhydra colonq/ide-typescript (:color teal :hint nil)
;;   "Dispatcher > Typescript IDE"
;;   ("<escape>" keyboard-escape-quit)
;;   ("S" eglot-reconnect "start")
;;   ("i" eglot-code-actions "poke")
;;   ("D" xref-find-definitions "goto defs")
;;   ("R" xref-find-references "goto refs"))
;; 
;; (defun colonq/typescript-setup ()
;;   "Setup Typescript configuration."
;;   (setq-local colonq/contextual-ide #'colonq/ide-typescript/body)
;;   (eglot-ensure)
;;   (flycheck-mode -1)
;;   (setq-local company-idle-delay 0.5)
;;   (setq-local flymake-no-changes-timeout 0.5)
;;   (setq-local eldoc-documentation-functions '(eglot-signature-eldoc-function eglot-hover-eldoc-function flymake-eldoc-function))
;;   (setq-local eldoc-display-functions '(colonq/eldoc-display-quick-peek))
;;   (add-hook 'post-command-hook #'quick-peek-hide nil 'local))
;; 
;; (add-hook 'typescript-ts-mode-hook #'colonq/typescript-setup)

(provide 'colonq-web)
;;; colonq-web.el ends here
