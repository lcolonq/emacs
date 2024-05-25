;;; colonq-dispatcher --- contextual interfaces -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'colonq-package)
(require 'colonq-evil)
(require 'colonq-hydra)

(defvar-local colonq/contextual-ide (lambda () (interactive) (message "No IDE support in current mode")))
(defvar-local colonq/contextual-lookup 'man)
(defvar-local colonq/contextual-quit nil)
(defvar-local colonq/contextual-write nil)
(defvar-local colonq/contextual-kill nil)

(setq evil-lookup-func (lambda () (call-interactively colonq/contextual-lookup)))

(defun colonq/evil-quit-wrapper (f &rest args)
  "Wrapper around F (evil-quit), passing ARGS."
  (if colonq/contextual-quit
      (call-interactively colonq/contextual-quit)
    (apply f args)))
(advice-add 'evil-quit :around 'colonq/evil-quit-wrapper)

(defun colonq/evil-write-wrapper (f &rest args)
  "Wrapper around F (evil-write), passing ARGS."
  (if colonq/contextual-write
      (call-interactively colonq/contextual-write)
    (apply f args)))
(advice-add 'evil-write :around 'colonq/evil-write-wrapper)

(defun colonq/kill-this-buffer ()
  "Kill the current buffer or call colonq/contextual-kill."
  (interactive)
  (if colonq/contextual-kill
      (call-interactively colonq/contextual-kill)
    (kill-this-buffer)))

(defun colonq/contextual-global ()
  "Run contextual global binding."
  (interactive)
  (when colonq/contextual-global
    (call-interactively colonq/contextual-global)))

(defun colonq/switch-to-scratch ()
  "Switch to *scratch* buffer."
  (interactive)
  (if (get-buffer "*scratch*")
      (switch-to-buffer "*scratch*")
    (progn
      (switch-to-buffer (get-buffer-create "*scratch*"))
      (cd "~")
      (eshell-mode))))

(defhydra colonq/repl-dispatcher (:color teal :hint nil :body-pre (setq exwm-input-line-mode-passthrough t) :post (setq exwm-input-line-mode-passthrough nil))
  "Dispatcher > REPLs"
  ("<f12>" 'keyboard-escape-quit)
  ("l" ielm "elisp")
  ("x" nix-repl "nix")
  ("y" (switch-to-buffer (make-comint "Python REPL" "python3" nil)) "python"))

(defhydra colonq/layout-dispatcher (:color teal :hint nil :body-pre (setq exwm-input-line-mode-passthrough t) :post (setq exwm-input-line-mode-passthrough nil))
  "Dispatcher > Layout"
  ("<f12>" 'keyboard-escape-quit)
  ("l" colonq/reload-eyebrowse-config "load")
  ("s" colonq/save-eyebrowse-config "save")
  ("d" colonq/lock-window "dedicated")
  ("g" colonq/set-gdb-source-window "gdb")
  ("i" colonq/setup-stream "initial"))

(defun colonq/toggle-shadow ()
  "Toggle shadow form."
  (interactive)
  (fig//model-toggle "shadow")
  (fig//toggle-vhs))

(defvar colonq//super-saiyan-enabled nil
  "Is Super Saiyan mode enabled?")
(defun colonq/toggle-saiyan ()
  "Toggle shadow form."
  (interactive)
  (fig//toggle-saiyan)
  (if colonq//super-saiyan-enabled
      (fig//model-palette-reset)
    (fig//model-further-beyond))
  (setq colonq//super-saiyan-enabled (not colonq//super-saiyan-enabled)))

(defun colonq/switch-to-new-form ()
  "Switch to the new form."
  (interactive)
  ;; (fig//toggle-explosion)
  (fig//model-toggle "new"))

(defhydra colonq/stream-dispatcher (:color teal :hint nil :body-pre (setq exwm-input-line-mode-passthrough t) :post (setq exwm-input-line-mode-passthrough nil))
  "Dispatcher > Stream"
  ("<f12>" 'keyboard-escape-quit)
  ("c" fig/critical-hit "crit")
  ("n" colonq/switch-to-new-form "new")
  ("p" (fig//toggle-persona4) "persona")
  ("s" colonq/toggle-shadow "shadow")
  ("f" colonq/toggle-saiyan "further")
  ("r" (soundboard//play-clip "timestop.ogg") "clock"))

(defhydra colonq/dispatcher (:color teal :hint nil :body-pre (setq exwm-input-line-mode-passthrough t) :post (setq exwm-input-line-mode-passthrough nil))
  "Dispatcher"
  ("<f12>" 'keyboard-escape-quit)
  ("<tab>" balance-windows)
  ("<print>" colonq/snip)
  (":" selector-M-x)
  ("?" selector-apropos)
  ("!" colonq/run-external-command)
  ("#" colonq/shell-sudo)
  ("\"" evil-window-vsplit)
  ("%" evil-window-split)
  ("SPC" colonq/kill-this-buffer)
  ("^" (eyebrowse-create-window-config))
  ("," eyebrowse-prev-window-config)
  ("." eyebrowse-next-window-config)
  ("<" eyebrowse-prev-window-config)
  (">" eyebrowse-next-window-config)
  ("/" colonq/selector-rg)
  ("+" (text-scale-increase 1) :color red)
  ("=" (text-scale-increase 1) :color red)
  ("-" (text-scale-increase -1) :color red)
  ("_" (text-scale-increase -1) :color red)
  ("0" colonq/switch-to-scratch)
  ("1" eyebrowse-switch-to-window-config-0)
  ("2" eyebrowse-switch-to-window-config-1)
  ("3" eyebrowse-switch-to-window-config-2)
  ("4" eyebrowse-switch-to-window-config-3)
  ("5" eyebrowse-switch-to-window-config-4)
  ("6" eyebrowse-switch-to-window-config-5)
  ("7" eyebrowse-switch-to-window-config-6)
  ("8" eyebrowse-switch-to-window-config-7)
  ("9" eyebrowse-switch-to-window-config-8)
  ("a" fig/voice-hydra/body "twitch")
  ("A" fig/end-transcribe "twitch")
  ("b" colonq/browser-dispatcher/body "web")
  ("B" colonq/visit-bookmark)
  ("f" selector-for-files "file")
  ("F" (dired "."))
  ("h" colonq/repl-dispatcher/body "repl")
  ("H" ielm)
  ("i" (call-interactively colonq/contextual-ide) "lang")
  ("I" imenu)
  ("j" rename-buffer)
  ("J" flycheck-next-error)
  ("k" evil-quit)
  ("K" eyebrowse-close-window-config)
  ("l" colonq/layout-dispatcher/body)
  ("L" colonq/reload-eyebrowse-config)
  ("m" colonq/music-dispatcher/body)
  ("o" colonq/navigate "buf")
  ("O" selector-for-buffers)
  ("p" projectile-switch-project "proj")
  ("P" colonq/password)
  ("q" colonq/previous-buffer)
  ("Q" eyebrowse-last-window-config)
  ("r" colonq/stream-dispatcher/body "stream")
  ("s" colonq/shell-here "shell")
  ("S" projectile-run-eshell)
  ("t" colonq/term-here "term")
  ("T" (fair/browse-server 'tsuki) "tsuki")
  ("v" colonq/vc-dispatcher/body "vc")
  ("V" magit-status)
  ("w" evil-write)
  ("x" shrink-window-horizontally :color red)
  ("X" enlarge-window-horizontally :color red)
  ("y" shrink-window :color red)
  ("Y" enlarge-window :color red)
  ("z" eyebrowse-switch-to-window-config)
  ("Z" eyebrowse-rename-window-config))

(defun colonq/dispatcher ()
  "Open Dispatcher menu."
  (interactive)
  ;; (let ((hydra-is-helpful t))
  (let ((hydra-is-helpful nil))
    (call-interactively 'colonq/dispatcher/body)))

(defun colonq/dispatcher-silent ()
  "Open Dispatcher menu silently."
  (interactive)
  (let ((hydra-is-helpful nil))
    (call-interactively 'colonq/dispatcher/body)))

(provide 'colonq-dispatcher)
;;; colonq-dispatcher.el ends here
