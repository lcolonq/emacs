;;; colonq-lsp --- language server protocol -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'colonq-package)

(use-package yasnippet
  :config
  (yas-reload-all)
  :hook ((lsp-mode . yas-minor-mode)))

(setq read-process-output-max (* 1024 1024))

(add-to-list 'load-path "~/src/lsp-mode")
(add-to-list 'load-path "~/src/lsp-mode/clients")
(add-to-list 'load-path "~/src/lsp-ui")
(use-package lsp-mode
  :custom
  (lsp-lens-enable nil)
  (lsp-prefer-flymake nil)
  (lsp-enable-snippet t)
  (lsp-eldoc-render-all t)
  :config
  (defun colonq/lsp-setup ()
    (setq-local colonq/contextual-lookup #'eldoc)
    (setq-local eldoc-display-functions '(eldoc-display-in-buffer)))
  (add-hook 'lsp-mode-hook #'colonq/lsp-setup))

(use-package lsp-ui
  :custom
  (lsp-ui-doc-enable t)
  (lsp-ui-sideline-enable nil)
  (lsp-ui-doc-delay 0)
  (lsp-ui-doc-use-childframe t)
  ;; (lsp-ui-doc-alignment 'frame)
  (lsp-ui-doc-alignment 'window)
  (lsp-ui-doc-position 'bottom)
  ;; (lsp-ui-doc-position 'top)
  (lsp-ui-doc-show-with-cursor t)
  (lsp-ui-doc-show-with-mouse t)
  (lsp-flycheck-live-reporting nil)
  :hook (lsp-mode . lsp-ui-mode)
  :config
  (ef-themes-with-colors
    (set-face-attribute 'lsp-ui-doc-background nil :background bg-main)))

(defvar lsp-ui-doc-frame-parameters
  '((override-redirect . t)
    (left . -1)
    (no-focus-on-map . t)
    (min-width  . 0)
    (width  . 0)
    (min-height  . 0)
    (height  . 0)
    (internal-border-width . 1)
    (vertical-scroll-bars . nil)
    (horizontal-scroll-bars . nil)
    (right-fringe . 0)
    (menu-bar-lines . 0)
    (tool-bar-lines . 0)
    (tab-bar-lines . 0)
    (tab-bar-lines-keep-state . 0)
    (line-spacing . 0)
    (unsplittable . t)
    (undecorated . t)
    (top . -1)
    (visibility . nil)
    (mouse-wheel-frame . nil)
    (no-other-frame . t)
    (inhibit-double-buffering . t)
    (drag-internal-border . t)
    (no-special-glyphs . t)
    (desktop-dont-save . t))
  "Frame parameters used to create the frame.")

(use-package lsp-ui-doc)

(defun lsp-ui-doc--move-frame (frame)
  "Place our FRAME on screen."
  (-let* (((left top right _bottom) (window-edges nil t nil t))
          (window (frame-root-window frame))
          (char-h (frame-char-height frame))
          (char-w (frame-char-width frame))
          ((width . height) (window-text-pixel-size window nil nil 10000 10000 t))
          (width (+ width (* char-w 1))) ;; margins
          (height (min (- (* lsp-ui-doc-max-height char-h) (/ char-h 2)) height))
          (width (min width (* lsp-ui-doc-max-width char-w)))
          (frame-right (pcase lsp-ui-doc-alignment
                         ('frame (frame-pixel-width))
                         ('window right)))
          (frame-right (* 1920))
          ((left . top) (if (eq lsp-ui-doc-position 'at-point)
                            (lsp-ui-doc--mv-at-point width height left top)
                          (cons (max (- frame-right width char-w) 10)
                                (pcase lsp-ui-doc-position
                                  ('top (+ top char-w))
                                  ('bottom (- (lsp-ui-doc--line-height 'mode-line)
                                              height
                                              10))))))
          (top 350)
          (frame-resize-pixelwise t)
          (move-frame-functions nil)
          (window-size-change-functions nil)
          (window-state-change-hook nil)
          (window-state-change-functions nil)
          (window-configuration-change-hook nil)
          (inhibit-redisplay t))
    ;; Dirty way to fix unused variable in emacs 26
    (and window-state-change-functions
         window-state-change-hook)
    ;; Make frame invisible before moving/resizing it to avoid flickering:
    ;; We set the position and size in 1 call, modify-frame-parameters, but
    ;; internally emacs makes 2 different calls, which can be visible
    ;; to the user
    (and (frame-visible-p frame)
         (lsp-ui-doc--size-and-pos-changed frame left top width height)
         (make-frame-invisible frame))
    (modify-frame-parameters
     frame
     `((parent-id . nil)
       (parent-frame . nil)
       (width . (text-pixels . ,width))
       (height . (text-pixels . ,height))
       (user-size . t)
       (left . (+ ,left))
       (top . (+ ,top))
       (user-position . t)
       (lsp-ui-doc--window-origin . ,(selected-window))
       (lsp-ui-doc--buffer-origin . ,(current-buffer))
       (lsp-ui-doc--no-focus . t)
       (right-fringe . 0)
       (left-fringe . 0)))
    ;; Insert hr lines after width is computed
    (lsp-ui-doc--fix-hr-props)
    (unless (frame-visible-p frame)
      (make-frame-visible frame))))

(add-to-list 'load-path "~/src/dap-mode")
(use-package dap-mode
  :custom
  (dap-auto-configure-features nil)
  (dap-ui-buffer-configurations nil)
  :config
  (require 'dap-ui)
  (require 'dap-js-debug)
  (require 'dap-gdb-lldb)
  (dap-mode 1)
  (dap-auto-configure-mode 1))
(use-package treemacs
  :config
  (setq treemacs-mode-map (make-keymap))
  (evil-define-key 'motion treemacs-mode-map
    (kbd "RET") #'treemacs-RET-action
    (kbd "TAB") #'treemacs-TAB-action
    )
  )
(defhydra colonq/dap-dispatcher (:color teal :hint nil)
  "Dispatcher > Debugger"
  ("<f12>" keyboard-escape-quit)
  ("b" dap-breakpoint-toggle "breakpoint")
  ("c" dap-continue "continue")
  ("l" dap-go-to-output-buffer "log")
  ("L" dap-ui-locals "locals")
  ("B" dap-ui-breakpoints "breakpoints")
  ("n" dap-next "next" :color red)
  ("s" dap-step-in "step" :color red))
;; let's not randomly pop up side windows
(defun dap-ui--show-buffer (buf)
  "Show BUF according to defined rules."
  (display-buffer buf))
(defun dap--create-output-buffer (session-name)
  "Creates an output buffer with with name SESSION-NAME."
  (with-current-buffer (get-buffer-create (concat "*" session-name " out*"))
    (special-mode)
    (set (make-local-variable 'window-point-insertion-type) t)
    (setq-local colonq/contextual-kill 'bury-buffer)
    (current-buffer)))
(defun dap-go-to-output-buffer (&optional no-select)
  "Go to output buffer."
  (interactive)
  (when (called-interactively-p 'any)
    (display-buffer (dap--debug-session-output-buffer (dap--cur-session-or-die)))))

;; (use-package eglot
;;   :custom (eglot-stay-out-of '(eldoc-documentation-strategy)))

(provide 'colonq-lsp)
;;; colonq-lsp.el ends here
