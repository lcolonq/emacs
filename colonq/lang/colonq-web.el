;;; colonq-web --- web development language support -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'colonq-package)
(require 'colonq-hydra)

(use-package js2-mode :mode "\\.js\\'"
  :custom
  (js-indent-level 2))

(use-package json-mode :mode "\\.json\\'"
  :config
  (defun colonq/json-setup ()
    (setq tab-width 2))
  (add-hook 'json-mode-hook #'colonq/json-setup))

(use-package web-mode :mode ("\\.html\\'" "\\.php\\'"))

(defhydra colonq/ide-typescript (:color teal :hint nil)
  "Dispatcher > Typescript IDE"
  ("<f12>" keyboard-escape-quit)
  ("S" lsp-workspace-restart "start")
  ("i" lsp-execute-code-action "poke")
  ("e" flycheck-next-error "goto err")
  ("r" lsp-rename "rename")
  ("D" xref-find-definitions "goto defs")
  ("R" xref-find-references "goto refs"))

(defun colonq/typescript-setup ()
  "Setup Typescript configuration."
  (lsp-mode)
  (setq-local lsp-eldoc-hook nil)
  (setq-local colonq/contextual-ide #'colonq/ide-typescript/body)
  (setq-local flycheck-check-syntax-automatically '(save mode-enabled)))

(use-package lsp-javascript
  :config
  (lsp-register-client
   (make-lsp-client
    :new-connection
    (lsp-stdio-connection
     (lambda ()
       `(,(lsp-package-path 'typescript-language-server)
         ;; had to remove a --tsserver here that prevented it from working :(
         ,@lsp-clients-typescript-server-args)))
    :activation-fn 'lsp-typescript-javascript-tsx-jsx-activate-p
    :priority -2
    :completion-in-comments? t
    :initialization-options
    (lambda ()
      (append
       (when lsp-clients-typescript-disable-automatic-typing-acquisition
         (list :disableAutomaticTypingAcquisition lsp-clients-typescript-disable-automatic-typing-acquisition))
       (when lsp-clients-typescript-log-verbosity
         (list :logVerbosity lsp-clients-typescript-log-verbosity))
       (when lsp-clients-typescript-max-ts-server-memory
         (list :maxTsServerMemory lsp-clients-typescript-max-ts-server-memory))
       (when lsp-clients-typescript-npm-location
         (list :npmLocation lsp-clients-typescript-npm-location))
       (when lsp-clients-typescript-plugins
         (list :plugins lsp-clients-typescript-plugins))
       (when lsp-clients-typescript-preferences
         (list :preferences lsp-clients-typescript-preferences))
       (when lsp-clients-typescript-tsserver
         (list :tsserver lsp-clients-typescript-tsserver))))
    :initialized-fn
    (lambda (workspace)
      (with-lsp-workspace workspace
        (lsp--set-configuration
         (ht-merge (lsp-configuration-section "javascript")
                   (lsp-configuration-section "typescript")
                   (lsp-configuration-section "completions")
                   (lsp-configuration-section "diagnostics"))))
      (let ((caps (lsp--workspace-server-capabilities workspace))
            (format-enable (or lsp-javascript-format-enable lsp-typescript-format-enable)))
        (lsp:set-server-capabilities-document-formatting-provider? caps format-enable)
        (lsp:set-server-capabilities-document-range-formatting-provider? caps format-enable)))
    :ignore-messages '("readFile .*? requested by TypeScript but content not available")
    :server-id 'ts-ls
    :request-handlers (ht ("_typescript.rename" #'lsp-javascript--rename))
    :download-server-fn
    (lambda (_client callback error-callback _update?)
      (lsp-package-ensure
       'typescript
       (-partial #'lsp-package-ensure
                 'typescript-language-server
                 callback
                 error-callback)
       error-callback)))))

(use-package typescript-ts-mode :mode ("\\.ts\\'")
  :config
  (add-hook 'typescript-ts-mode-hook #'colonq/typescript-setup))

(provide 'colonq-web)
;;; colonq-web.el ends here
