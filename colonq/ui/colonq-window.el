;;; colonq-window --- window configuration -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'colonq-package)
(require 'colonq-hydra)

(setq max-mini-window-height 0.25)

(use-package shackle
  :custom
  (shackle-default-rule '(:same t))
  (pop-up-windows nil)
  (even-window-heights nil)
  :config
  (setq shackle-default-rule '(:same t :inhibit-window-quit t))
  (setq
   shackle-rules
   '((c-mode
      :select nil
      :custom
      (lambda (buf alist plist)
        (let ((win (colonq/get-gdb-source-window)))
          (set-window-buffer win buf)
          win)))
     (janet-mode
      :custom
      (lambda (buf alist plist)
        (let ((win (colonq/get-stream-primary-window)))
          (set-window-buffer win buf)
          win)))
     )
   )
  (shackle-mode))

(use-package eyebrowse
  :config
  (define-key eyebrowse-mode-map (kbd "C-c") nil)
  (eyebrowse-mode))

;; remove the ignore-window-parameters cludge
(defun eyebrowse--load-window-config (slot)
  "Restore the window config from SLOT."
  (-when-let (match (assq slot (eyebrowse--get 'window-configs)))
    ;; KLUDGE: workaround for #36
    ;; see also http://debbugs.gnu.org/cgi/bugreport.cgi?bug=20848
    (when (version< emacs-version "25")
      (delete-other-windows)
      (set-window-dedicated-p nil nil))
    ;; KLUDGE: workaround for visual-fill-column foolishly
    ;; setting the split-window parameter
    (let (;; (ignore-window-parameters t)
          (window-config (cadr match)))
      (eyebrowse--fixup-window-config window-config)
      (window-state-put window-config (frame-root-window) 'safe))))

(use-package buffer-move
  :config
  (global-set-key (kbd "M-H") #'buf-move-left)
  (global-set-key (kbd "M-L") #'buf-move-right)
  (global-set-key (kbd "M-K") #'buf-move-up)
  (global-set-key (kbd "M-J") #'buf-move-down))

(provide 'colonq-window)
;;; colonq-window ends here
