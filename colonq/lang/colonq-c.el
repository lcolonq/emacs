;;; colonq-c --- C/C++ support -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'colonq-package)
(require 'colonq-hydra)
(require 'colonq-flycheck)

(defun colonq/c-stop-debugging-gba ()
  "Exit GDB and kill mGBA."
  (interactive)
  (call-process "pkill" nil nil nil "mgba")
  (with-current-buffer gud-comint-buffer
    (colonq/comint-eof)))

(defun colonq/c-debug-gba ()
  "Launch GDB to debug a GBA project via mGBA."
  (interactive)
  (selector
   (list
    (selector-source-create
     "GBA ROM"
     :candidates
     (-map
      #'selector-candidate-create
      (--filter
       (-contains? '("gba" "elf") (f-ext it))
       (directory-files default-directory)))
     :actions
     (list
      (lambda (path)
        (when (= 1 (shell-command "pgrep mgba"))
          (start-process "mgba" nil "mgba" "-g" path))
        ;; the way this function splits arguments is absurd - be very careful
        (gdb (s-concat "gdb -i=mi -ex \"target remote localhost:2345\" " path))
        (with-current-buffer gud-comint-buffer
          (setq-local colonq/contextual-kill #'colonq/c-stop-debugging-gba))))))))

(define-key gud-mode-map (kbd "C-d") #'colonq/kill-this-buffer)

(push '(gdb-source . writable) window-persistent-parameters)
(defun colonq/get-gdb-source-window ()
  "Get the marked GDB source window."
  (-first (lambda (w) (window-parameter w 'gdb-source)) (window-list)))
(defun colonq/set-gdb-source-window ()
  "Set the marked GDB source window to the current window."
  (interactive)
  (set-window-parameter (get-buffer-window (current-buffer)) 'gdb-source t))
(setq
 gdb-non-stop-setting nil
 gdb-max-source-window-count 0
 gdb-debuginfod-enable nil
 gdb-debuginfod-enable-setting nil
 gdb-locals-header nil
 gdb-registers-header nil
 )

(defhydra colonq/ide-c (:color teal :hint nil)
  "Dispatcher > C IDE"
  ("<f12>" keyboard-escape-quit)
  ("i" (compile "make") "build")
  ;; ("i" gud-break "break")
  ;; ("I" gud-remove)
  ;; ("r" gdb "debugger")
  )

(defun colonq/c-setup ()
  "Custom configuration for C programming."
  (c-set-style "linux")
  (hl-line-mode)
  (flycheck-inline-mode)
  ;; (setq-local indent-tabs-mode t)
  (setq-local indent-tabs-mode nil)
  (setq-local tab-width 4)
  (setq-local c-basic-offset 4)
  ;; (setq-local company-backends '((company-dabbrev-code company-keywords)))
  (setq-local colonq/contextual-ide #'colonq/ide-c/body)
  (define-key c-mode-map (kbd "M-h") #'windmove-left)
  (define-key c-mode-map (kbd "M-l") #'windmove-right)
  (define-key c-mode-map (kbd "M-k") #'windmove-up)
  (define-key c-mode-map (kbd "M-j") #'windmove-down)
  (flycheck-mode -1)
  (let ((root (projectile-project-root)))
    (cond
     ((s-equals? root "/home/llll/src/3do-devkit/")
      (flycheck-mode -1))
     ((s-equals? root "/home/llll/src/cyclone/")
      (flycheck-mode -1))
     ((projectile-project-root)
      (setq-local
       flycheck-gcc-include-path
       (list
        (concat (projectile-project-root) "include")))))))
(add-hook 'c-mode-hook 'colonq/c-setup)
(add-hook 'c++-mode-hook 'colonq/c-setup)

(provide 'colonq-c)
;;; colonq-c ends here
