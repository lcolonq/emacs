;;; colonq-exwm --- X window manager -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'colonq-package)
(require 'colonq-hydra)

(setq x-no-window-manager t)

(use-package exwm-randr
  :custom
  (exwm-randr-workspace-monitor-plist '(0 "eDP-1" 1 "HDMI-1"))
  :config
  (exwm-randr-enable))

;; (use-package exwm-systemtray
;;   :config
;;   (exwm-systemtray-enable))

(defun colonq/external-monitor ()
  "Setup external monitor."
  (interactive)
  (start-process-shell-command "xrandr" nil "multi")
  (exwm-workspace-add 1)
  (exwm-workspace-switch 1)
  )

(defvar colonq/saved-eyebrowse-config nil)
(defun colonq/save-eyebrowse-config ()
  "Save the current eyebrowse window config."
  (interactive)
  (let* ((current-slot (eyebrowse--get 'current-slot))
         (window-configs (eyebrowse--get 'window-configs))
         (current-tag (nth 2 (assoc current-slot window-configs))))
    (setq
     colonq/saved-eyebrowse-config
     (eyebrowse--current-window-config current-slot current-tag))))
(defun colonq/reload-eyebrowse-config ()
  "Reload the saved eyebrowse window config."
  (interactive)
  (when colonq/saved-eyebrowse-config
    (let ((current-slot (eyebrowse--get 'current-slot)))
      (eyebrowse--update-window-config-element
       (cons current-slot (cdr colonq/saved-eyebrowse-config)))
      (eyebrowse--load-window-config current-slot))))

(defun colonq/lock-window ()
  "Toggle whether the current window is dedicated."
  (interactive)
  (let ((win (get-buffer-window (current-buffer)))
        (new (not (window-dedicated-p))))
    (message (s-concat "Window dedicated: " (if new "yes" "no")))
    (set-window-dedicated-p win (not (window-dedicated-p win)))))

(defun colonq/other-monitor ()
  "Switch focus to the other monitor."
  (interactive)
  (if (= exwm-workspace-current-index 0)
      (exwm-workspace-switch 1)
    (exwm-workspace-switch 0)))

(use-package exwm
  :custom
  (exwm-workspace-warp-cursor t)
  :config
  (defun colonq/exwm-escape ()
    (interactive)
    (cond ((eq major-mode 'exwm-mode) (call-interactively 'colonq/exwm-normal))
          ((evil-insert-state-p) (call-interactively 'evil-normal-state))
          ((evil-visual-state-p) (call-interactively 'evil-exit-visual-state))
          (t nil)))
  (setq exwm-input-simulation-keys
        (list
         (cons (kbd "B") (kbd "M-<left>"))
         (cons (kbd "N") (kbd "M-<right>"))
         (cons (kbd "H") (kbd "C-<backtab>"))
         (cons (kbd "L") (kbd "C-<tab>"))
         (cons (kbd "g") (kbd "<home>"))
         (cons (kbd "G") (kbd "<end>"))
         (cons (kbd "R") (kbd "C-r"))
         (cons (kbd "p") (kbd "C-v"))
         (cons (kbd "y") (kbd "C-c"))
         (cons (kbd "h") (kbd "<left>"))
         (cons (kbd "l") (kbd "<right>"))
         (cons (kbd "k") (kbd "<up>"))
         (cons (kbd "j") (kbd "<down>"))))
  (setq exwm-manage-configurations
        '((t
           floating nil)))
  (setq exwm-input-global-keys
        (list
         (cons (kbd "<f12>") 'colonq/exwm-escape)
         (cons (kbd "<print>") 'colonq/screenshot)
         (cons (kbd "S-<print>") 'colonq/snip)))

  (defun colonq/exwm-rename ()
    (exwm-workspace-rename-buffer exwm-title))
  (add-hook 'exwm-update-title-hook 'colonq/exwm-rename)
  (defun colonq/exwm-insert ()
    (interactive)
    (message "EXWM Insert")
    (exwm-input--release-keyboard))
  (defun colonq/exwm-normal ()
    (interactive)
    (message "EXWM Normal")
    (exwm-input--grab-keyboard))
  (defun colonq/exwm-setup ()
    (define-key evil-emacs-state-map (kbd "q") nil)
    (define-key exwm-mode-map (kbd "a") 'colonq/nop)
    (define-key exwm-mode-map (kbd "b") 'colonq/nop)
    (define-key exwm-mode-map (kbd "c") 'colonq/nop)
    (define-key exwm-mode-map (kbd "d") 'colonq/nop)
    (define-key exwm-mode-map (kbd "e") 'colonq/nop)
    (define-key exwm-mode-map (kbd "f") 'colonq/nop)
    ;; g is a simulation key
    ;; h is a simulation key
    (define-key exwm-mode-map (kbd "i") 'colonq/exwm-insert)
    ;; j is a simulation key
    ;; k is a simulation key
    ;; l is a simulation key
    (define-key exwm-mode-map (kbd "m") 'colonq/nop)
    (define-key exwm-mode-map (kbd "n") 'colonq/nop)
    (define-key exwm-mode-map (kbd "o") 'colonq/nop)
    ;; p is a simulation key
    (define-key exwm-mode-map (kbd "q") 'colonq/dispatcher-silent)
    (define-key exwm-mode-map (kbd "r") 'colonq/nop)
    (define-key exwm-mode-map (kbd "s") 'colonq/nop)
    (define-key exwm-mode-map (kbd "t") 'colonq/nop)
    (define-key exwm-mode-map (kbd "u") 'colonq/nop)
    (define-key exwm-mode-map (kbd "v") 'colonq/nop)
    (define-key exwm-mode-map (kbd "w") 'colonq/nop)
    (define-key exwm-mode-map (kbd "x") 'colonq/nop)
    ;; y is a simulation key
    (define-key exwm-mode-map (kbd "z") 'colonq/nop)
    (define-key exwm-mode-map (kbd "A") 'colonq/nop)
    ;; B is a simulation key
    (define-key exwm-mode-map (kbd "C") 'colonq/nop)
    (define-key exwm-mode-map (kbd "D") 'colonq/nop)
    (define-key exwm-mode-map (kbd "E") 'colonq/nop)
    (define-key exwm-mode-map (kbd "F") 'colonq/nop)
    ;; G is a simulation key
    ;; H is a simulation key
    (define-key exwm-mode-map (kbd "I") 'colonq/nop)
    (define-key exwm-mode-map (kbd "J") 'colonq/nop)
    (define-key exwm-mode-map (kbd "K") 'colonq/nop)
    ;; L is a simulation key
    (define-key exwm-mode-map (kbd "M") 'colonq/nop)
    ;; N is a simulation key
    (define-key exwm-mode-map (kbd "O") 'colonq/nop)
    (define-key exwm-mode-map (kbd "P") 'colonq/nop)
    (define-key exwm-mode-map (kbd "Q") 'colonq/other-monitor)
    ;; R is a simulation key
    (define-key exwm-mode-map (kbd "S") 'colonq/nop)
    (define-key exwm-mode-map (kbd "T") 'colonq/nop)
    (define-key exwm-mode-map (kbd "U") 'colonq/nop)
    (define-key exwm-mode-map (kbd "V") 'colonq/nop)
    (define-key exwm-mode-map (kbd "W") 'colonq/nop)
    (define-key exwm-mode-map (kbd "X") 'colonq/nop)
    (define-key exwm-mode-map (kbd "Y") 'colonq/nop)
    (define-key exwm-mode-map (kbd "Z") 'colonq/nop)
    (define-key exwm-mode-map (kbd "0") 'colonq/nop)
    (define-key exwm-mode-map (kbd "1") 'colonq/nop)
    (define-key exwm-mode-map (kbd "2") 'colonq/nop)
    (define-key exwm-mode-map (kbd "3") 'colonq/nop)
    (define-key exwm-mode-map (kbd "4") 'colonq/nop)
    (define-key exwm-mode-map (kbd "5") 'colonq/nop)
    (define-key exwm-mode-map (kbd "6") 'colonq/nop)
    (define-key exwm-mode-map (kbd "7") 'colonq/nop)
    (define-key exwm-mode-map (kbd "8") 'colonq/nop)
    (define-key exwm-mode-map (kbd "9") 'colonq/nop)
    (define-key exwm-mode-map (kbd "~") 'colonq/nop)
    ;; ` is a simulation key
    (define-key exwm-mode-map (kbd "!") 'colonq/nop)
    (define-key exwm-mode-map (kbd "@") 'colonq/nop)
    (define-key exwm-mode-map (kbd "#") 'colonq/nop)
    (define-key exwm-mode-map (kbd "$") 'colonq/nop)
    (define-key exwm-mode-map (kbd "%") 'colonq/nop)
    (define-key exwm-mode-map (kbd "^") 'colonq/nop)
    (define-key exwm-mode-map (kbd "&") 'colonq/nop)
    (define-key exwm-mode-map (kbd "*") 'colonq/nop)
    (define-key exwm-mode-map (kbd "(") 'colonq/nop)
    (define-key exwm-mode-map (kbd ")") 'colonq/nop)
    (define-key exwm-mode-map (kbd "_") 'colonq/nop)
    (define-key exwm-mode-map (kbd "-") 'colonq/nop)
    (define-key exwm-mode-map (kbd "+") 'colonq/nop)
    (define-key exwm-mode-map (kbd "=") 'colonq/nop)
    (define-key exwm-mode-map (kbd "{") 'colonq/nop)
    (define-key exwm-mode-map (kbd "[") 'colonq/nop)
    (define-key exwm-mode-map (kbd "}") 'colonq/nop)
    (define-key exwm-mode-map (kbd "]") 'colonq/nop)
    (define-key exwm-mode-map (kbd "|") 'colonq/nop)
    (define-key exwm-mode-map (kbd "\\") 'colonq/nop)
    (define-key exwm-mode-map (kbd ":") 'colonq/nop)
    (define-key exwm-mode-map (kbd ";") 'colonq/nop)
    (define-key exwm-mode-map (kbd "\"") 'colonq/nop)
    (define-key exwm-mode-map (kbd "'") 'colonq/nop)
    (define-key exwm-mode-map (kbd "<") 'colonq/nop)
    (define-key exwm-mode-map (kbd ",") 'colonq/nop)
    (define-key exwm-mode-map (kbd ">") 'colonq/nop)
    (define-key exwm-mode-map (kbd ".") 'colonq/nop)
    (define-key exwm-mode-map (kbd "?") 'colonq/nop)
    (define-key exwm-mode-map (kbd "/") 'colonq/nop)
    (define-key exwm-mode-map (kbd "SPC") 'colonq/nop)
    (define-key exwm-mode-map (kbd "C-t") 'colonq/nop)
    (define-key exwm-mode-map (kbd "C-w") 'colonq/nop)
    (define-key exwm-mode-map (kbd "M-q") 'exwm-floating-toggle-floating)
    (define-key exwm-mode-map (kbd "M-h") 'windmove-left)
    (define-key exwm-mode-map (kbd "M-l") 'windmove-right)
    (define-key exwm-mode-map (kbd "M-k") 'windmove-up)
    (define-key exwm-mode-map (kbd "M-j") 'windmove-down)
    (define-key exwm-mode-map (kbd "M-H") 'buf-move-left)
    (define-key exwm-mode-map (kbd "M-L") 'buf-move-right)
    (define-key exwm-mode-map (kbd "M-K") 'buf-move-up)
    (define-key exwm-mode-map (kbd "M-J") 'buf-move-down)
    (cd "~")
    (colonq/exwm-normal))
  (add-hook 'exwm-mode-hook 'colonq/exwm-setup)
  (exwm-enable))

(provide 'colonq-exwm)
;;; colonq-exwm ends here
