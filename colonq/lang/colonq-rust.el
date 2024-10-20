;;; colonq-rust --- Rust support -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'colonq-package)
(require 'colonq-hydra)

(defun colonq/rust-debug-trunk ()
  "Launch Chrome to debug a Trunk WASM project."
  (interactive)
  (dap-delete-all-sessions)
  (when (= 1 (shell-command "pgrep chrome"))
    (start-process "chromedebug" nil "chromedebug" "http://localhost:8080"))
  (dap-debug
   (list :type "pwa-chrome"
         :cwd nil
         :request "attach"
         :port 9222
         :webRoot nil
         :url "http://localhost:8080"
         :name "Chrome Attach")))

(add-to-list 'load-path "~/src/xterm-color")
(add-to-list 'load-path "~/src/rust-mode")
(add-to-list 'load-path "~/src/rustic")
(use-package rustic
  :mode ("\\.rs\\'" . rustic-mode)
  :config
  (add-to-list 'safe-local-variable-values '(lsp-rust-analyzer-cargo-target . "wasm32-unknown-unknown"))
  (defhydra colonq/ide-rust (:color teal :hint nil)
    "Dispatcher > Rust IDE"
    ("<f12>" keyboard-escape-quit)
    ("S" lsp-workspace-restart "start")
    ("i" lsp-execute-code-action "poke")
    ("X" colonq/rust-debug-trunk "debugger")
    ("d" colonq/dap-dispatcher/body "debug")
    ("e" flycheck-next-error "goto err")
    ("r" lsp-rename "rename")
    ("D" xref-find-definitions "goto def")
    ("R" xref-find-references "goto refs"))
  (defun colonq/rust-setup ()
    (company-mode -1)
    (flycheck-mode -1)
    (o/mode)
    (setq o/backends '(o/backend-lsp))
    (setq-local lsp-eldoc-hook nil)

    (setq-local colonq/contextual-ide 'colonq/ide-rust/body)
    (setq-local flycheck-check-syntax-automatically '(save mode-enabled)))
  (add-hook 'rust-mode-hook #'colonq/rust-setup))

(provide 'colonq-rust)
;;; colonq-rust.el ends here
