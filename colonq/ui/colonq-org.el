;;; colonq-org --- outlining and productivity -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'colonq-package)
(require 'colonq-hydra)

(use-package org
  :custom
  (org-directory "~/notes")
  (org-default-notes-file "~/notes/scratch.org")
  (org-agenda-files '("~/notes"))
  (org-log-done 'time)
  (org-goto-interface 'outline-path-completion)
  (org-outline-path-complete-in-steps nil)
  (org-todo-keywords '((sequence "TODO" "STUCK" "|" "DONE" "DROPPED")))
  (org-src-preserve-indentation t)
  (org-src-window-setup 'current-window)
  (org-export-date-timestamp-format "%Y/%m/%d")
  (org-confirm-babel-evaluate nil)
  (org-refile-targets '((nil :maxlevel . 9)
                        (org-agenda-files :maxlevel . 9)))
  (org-babel-results-keyword "results")
  (org-image-actual-width nil)
  (org-list-allow-alphabetical t)
  (org-html-htmlize-output-type 'inline-css)
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (gnuplot . t)
     (latex . t)
     (python . t)
     (C . t)
     (shell . t)
     (sqlite . t)))

  (evil-define-key 'normal org-mode-map
    (kbd "H") 'org-shiftleft
    (kbd "L") 'org-shiftright
    (kbd "M-H") 'org-metaleft
    (kbd "M-J") 'org-metadown
    (kbd "M-K") 'org-metaup
    (kbd "M-L") 'org-metaright
    (kbd "C-h") 'org-shiftmetaleft
    (kbd "C-j") 'org-shiftmetadown
    (kbd "C-k") 'org-shiftmetaup
    (kbd "C-l") 'org-shiftmetaright
    (kbd "RET") 'org-open-at-point)

  (defhydra colonq/ide-org (:color teal :hint nil)
    "Dispatcher > Org IDE"
    ("<escape>" keyboard-escape-quit)
    ("i" org-ctrl-c-ctrl-c "poke")
    ("o" org-goto "goto")
    ("h" org-html-export-to-html "html")
    ("H" (lambda () (interactive) (browse-url (org-html-export-to-html))))
    ("v" org-latex-export-to-pdf "pdf")
    ("V" (lambda () (interactive) (find-file (org-latex-export-to-pdf))))
    ("w" org-babel-tangle "tangle")
    ("x" org-export-dispatch "export")
    ("r" (command-execute (if org-capture-mode 'org-capture-refile 'org-refile)) "refile")
    ("e" org-edit-special "edit")
    ("s" org-schedule "schedule")
    ("p" org-set-property "prop")
    ("t" org-todo "todo")
    ("c" org-toggle-checkbox "check")
    ("l" org-toggle-link-display "links")
    ("f" org-toggle-latex-fragment "tex")
    ("m" org-toggle-inline-images "images"))
  (defun colonq/org-setup ()
    (set-face-attribute 'org-todo nil :box nil)
    (set-face-attribute 'org-done nil :box nil)
    (set-face-attribute 'org-checkbox-statistics-todo nil :box nil)
    (set-face-attribute 'org-checkbox-statistics-done nil :box nil)
    (setq-local evil-auto-indent nil)
    (setq-local colonq/contextual-ide 'colonq/ide-org/body)
    ;; (setq-local colonq/contextual-kill 'bury-buffer)
    (setq-local colonq/contextual-hyper-state-bindings
                '(("k" . org-previous-visible-heading)
                  ("j" . org-next-visible-heading)))
    (local-unset-key (kbd "M-h"))
    (org-indent-mode))
  (add-hook 'org-mode-hook 'colonq/org-setup)
  (defun colonq/org-src-setup ()
    (when (eq major-mode 'emacs-lisp-mode)
      ;(setq-local buffer-file-name "dummy.el") ; an ugly hack, doesn't really work :(
      (setq-local flycheck-disabled-checkers '(emacs-lisp-checkdoc)))
    (setq-local colonq/contextual-write 'org-edit-src-save)
    (setq-local colonq/contextual-quit 'org-edit-src-exit)
    (setq-local colonq/contextual-kill 'org-edit-src-exit)
    (setq-local header-line-format (concat "Editing a source block:")))
  (add-hook 'org-src-mode-hook 'colonq/org-src-setup))

(use-package org-attach
  :custom
  (org-attach-id-dir "~/notes/attach"))

(use-package ox-extra
  :config
  (ox-extras-activate '(ignore-headlines)))

(set-face-attribute 'org-todo nil :box nil)
(set-face-attribute 'org-done nil :box nil)
(set-face-attribute 'org-checkbox-statistics-todo nil :box nil)
(set-face-attribute 'org-checkbox-statistics-done nil :box nil)
(set-face-attribute 'org-scheduled-today nil :foreground nil)
(set-face-attribute 'org-agenda-date-today nil :foreground nil :background nil :weight 'normal :slant 'normal)

(provide 'colonq-org)
;;; colonq-org ends here
