;;; colonq-gpg --- GPG interface -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'colonq-package)
(require 'colonq-hydra)
(require 'colonq-exwm)

(use-package pinentry
  :config
  (pinentry-start))

(defun colonq/password-store-list ()
  "List password-store entries."
  (mapcar (lambda (file)
            (f-no-ext (f-relative file "~/.password-store/")))
          (f-files "~/.password-store" (lambda (file) (equal (f-ext file) "gpg")) t)))

(defun colonq/read-password (id)
  "Read the password-store entry corresponding to ID."
  (let ((find-file-hook (remq 'recentf-track-opened-file find-file-hook)))
    (find-file (concat "~/.password-store/" id ".gpg"))
    (goto-char 1)
    (kill-new (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
    (message (concat "Copied password for " id " to kill ring"))
    (kill-buffer (current-buffer))))

(defun colonq/selector-passwords ()
  "Selector source for password-store passwords."
  (selector-source-create
   "Passwords"
   :candidates
   (colonq/password-store-list)
   :actions
   (list #'colonq/read-password)))

(defun colonq/password ()
  "Interactively select a password-store password."
  (interactive)
  (selector (list (colonq/selector-passwords))))

(provide 'colonq-gpg)
;;; colonq-gpg ends here
