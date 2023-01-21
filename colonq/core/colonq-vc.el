;;; colonq-vc --- version control -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'colonq-package)
(require 'colonq-hydra)

(use-package vc
  :custom
  (vc-handled-backends nil)
  :config
  (remove-hook 'find-file-hook 'vc-refresh-state))

(use-package magit
  :custom
  (magit-no-message '("Turning on"))
  (magit-completing-read-function #'selector-completing-read)
  :config
  (defhydra colonq/vc-dispatcher (:color teal :hint nil :body-pre (setq exwm-input-line-mode-passthrough t) :post (setq exwm-input-line-mode-passthrough nil))
    "Dispatcher > Version Control"
    ("<escape>" keyboard-escape-quit)
    ("v" magit-status "status")
    ("h" magit-log-buffer-file "history")
    ("l" magit-log-current "log")
    ("f" magit-log-buffer-file "file")
    ("d" (magit-diff-range "master") "diff")
    ("s" magit-checkout "switch")
    ("S" magit-branch-and-checkout)
    ("t" magit-stash-list "stash")
    ("b" magit-blame-addition "blame")
    ("q" magit-blame-quit "quit-blame")
    ("T" magit-stash-worktree)
    ("c" (magit-commit-create '("-S")) "commit")
    ("p" magit-push-current-to-upstream "push")
    ("u" magit-pull-from-upstream "pull"))
  (setq
   magit-mode-map (make-keymap)
   magit-status-mode-map (make-keymap)
   magit-diff-mode-map (make-keymap)
   magit-stashes-mode-map (make-keymap))
  (evil-define-key 'motion magit-mode-map
    (kbd "RET") #'magit-visit-thing
    (kbd "TAB") #'magit-section-cycle
    (kbd "R") #'magit-refresh
    (kbd "s") #'magit-stage
    (kbd "u") #'magit-unstage
    (kbd "x") #'magit-discard
    (kbd "zo") #'magit-section-show
    (kbd "zc") #'magit-section-hide
    (kbd "zR") #'magit-section-show-level-4-all
    (kbd "zM") #'magit-section-hide-children)
  )

(use-package with-editor
  :config
  (defun colonq/with-editor-setup ()
    (setq-local colonq/contextual-write 'with-editor-finish)
    (setq-local colonq/contextual-kill 'with-editor-cancel))
  (add-hook 'with-editor-mode-hook 'colonq/with-editor-setup))

(provide 'colonq-vc)
;;; colonq-vc ends here
