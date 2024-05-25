;;; colonq-term --- terminal emulation -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package vterm
  :config
  (setq vterm-keymap-exceptions nil)

  (defun colonq/maybe-kill-vterm (n e)
    (ignore e)
    (when n (kill-buffer)))

  (add-hook 'vterm-exit-functions #'colonq/maybe-kill-vterm)
  (define-key vterm-mode-map [return] #'vterm-send-return)
  (define-key vterm-mode-map [insert] #'evil-exit-emacs-state)
  (evil-define-key 'insert vterm-mode-map
    (kbd "<f12>") #'evil-normal-state
    (kbd "<escape>") #'vterm--self-insert
    (kbd "C-e") #'vterm--self-insert
    (kbd "C-f") #'vterm--self-insert
    (kbd "C-a") #'vterm--self-insert
    (kbd "C-v") #'vterm--self-insert
    (kbd "C-b") #'vterm--self-insert
    (kbd "C-w") #'vterm--self-insert
    (kbd "C-u") #'vterm--self-insert
    (kbd "C-d") #'vterm--self-insert
    (kbd "C-n") #'vterm--self-insert
    (kbd "C-m") #'vterm--self-insert
    (kbd "C-p") #'vterm--self-insert
    (kbd "C-j") #'vterm--self-insert
    (kbd "C-k") #'vterm--self-insert
    (kbd "C-r") #'vterm--self-insert
    (kbd "C-t") #'vterm--self-insert
    (kbd "C-g") #'vterm--self-insert
    (kbd "C-c") #'vterm--self-insert
    (kbd "C-SPC") #'vterm--self-insert)

  (evil-define-key 'normal vterm-mode-map
    (kbd "C-d") #'vterm--self-insert
    (kbd "M-h") #'windmove-left
    (kbd "M-l") #'windmove-right
    (kbd "M-k") #'windmove-up
    (kbd "M-j") #'windmove-down
    (kbd "p") #'vterm-yank
    (kbd "u") #'vterm-undo
    (kbd "<insert>") #'evil-emacs-state
    (kbd "<return>") #'evil-resume))

(defun colonq/term-here ()
  "Open a terminal emulator in the current directory."
  (interactive)
  (vterm))

(provide 'colonq-term)
;;; colonq-term ends here
