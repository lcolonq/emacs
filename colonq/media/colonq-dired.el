;;; colonq-dired --- file manager -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'colonq-package)
(require 'colonq-hydra)

(use-package dired
  :custom
  (dired-dwim-target t)
  :config
  (setq dired-listing-switches "-lvah")
  (defun colonq/dired-find-file ()
    (interactive)
    (if (file-directory-p (dired-get-file-for-visit))
        (dired-find-alternate-file)
      (dired-find-file)))
  (defun colonq/dired-up-directory ()
    (interactive)
    (let ((buf (current-buffer)))
      (dired-up-directory)
      (kill-buffer buf)))
  (defun colonq/dired-thumbnails ()
    (interactive)
    (let ((thumbnail-buf (concat "*thumbnails " (dired-current-directory) "*")))
      (if (get-buffer thumbnail-buf)
          (switch-to-buffer thumbnail-buf)
        (let ((buf (current-buffer)))
          (image-dired (dired-current-directory))
          (rename-buffer thumbnail-buf)
          (with-current-buffer buf
            (dired-unmark-all-marks))))))
  (setq dired-mode-map (make-keymap))
  (evil-set-initial-state 'dired-mode 'motion)
  (evil-define-key 'motion dired-mode-map
    (kbd "q") 'colonq/dispatcher
    (kbd "SPC") 'dired-mark
    (kbd "RET") 'colonq/dired-find-file
    (kbd "u") 'colonq/dired-up-directory
    (kbd "o") 'dired-create-directory
    (kbd "x") 'dired-do-delete
    (kbd "d") 'dired-do-rename
    (kbd "y") 'dired-do-copy
    (kbd "i") 'colonq/dired-thumbnails
    (kbd "R") 'revert-buffer)

   (defun colonq/dired-setup ()
     (hl-line-mode))
   (add-hook 'dired-mode-hook 'colonq/dired-setup))

(use-package image-dired
  :config
  (setq image-dired-show-all-from-dir-max-files 1000)
  (defun colonq/image-dired-find-file ()
    (interactive)
    (find-file (image-dired-original-file-name)))
  (defun colonq/image-dired-delete ()
    (interactive)
    (let ((path (image-dired-original-file-name)))
      (when (yes-or-no-p (concat "Really delete " path "?"))
        (delete-file path))))
  (setq image-dired-thumbnail-mode-map (make-keymap))
  (evil-set-initial-state 'image-dired-thumbnail-mode 'motion)
  (evil-define-key 'motion image-dired-thumbnail-mode-map
    (kbd "h") 'image-dired-backward-image
    (kbd "b") 'image-dired-backward-image
    (kbd "l") 'image-dired-forward-image
    (kbd "w") 'image-dired-forward-image
    (kbd "e") 'image-dired-forward-image
    (kbd "k") 'image-dired-previous-line
    (kbd "j") 'image-dired-next-line
    (kbd "x") 'colonq/image-dired-delete
    (kbd "RET") 'colonq/image-dired-find-file))

(provide 'colonq-dired)
;;; colonq-dired ends here
