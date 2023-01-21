;;; colonq-pdf --- PDF reader -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'colonq-package)
(require 'colonq-hydra)
(require 'colonq-evil)

(use-package pdf-tools
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :config
  (define-key pdf-view-mode-map (kbd "q") nil)
  (evil-define-key 'motion pdf-view-mode-map
    "h" 'scroll-right
    "l" 'scroll-left
    "j" 'pdf-view-next-line-or-next-page
    "k" 'pdf-view-previous-line-or-previous-page
    "J" 'pdf-view-scroll-up-or-next-page
    "K" 'pdf-view-scroll-down-or-previous-page
    "]" 'pdf-view-next-page-command
    "[" 'pdf-view-previous-page-command
    "-" 'pdf-view-shrink
    "+" 'pdf-view-enlarge
    "gj" 'pdf-view-next-page-command
    "gk" 'pdf-view-previous-page-command
    "gg" 'pdf-view-first-page
    "G" 'pdf-view-last-page

    [down-mouse-1] 'pdf-view-mouse-set-region
    "d" 'pdf-view-kill-ring-save
    "y" 'pdf-view-kill-ring-save)
  (add-hook 'doc-view-mode-hook 'pdf-view-mode)
  (add-hook 'pdf-view-mode-hook
            (lambda ()
              (set (make-local-variable
                    'evil-normal-state-cursor)
                   (list nil)))))

(defun colonq/pdf-find-file-wrapper (f &rest args)
  "Wrapper around F (find-file), passing ARGS."
  (if (string= (file-name-extension (car args)) "pdf")
      (progn
        (colonq/firefox (car args))
        (recentf-add-file (car args))
        nil)
    (apply f args)))
(advice-add 'find-file :around 'colonq/pdf-find-file-wrapper)

(provide 'colonq-pdf)
;;; colonq-pdf ends here
