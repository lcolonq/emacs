;;; init --- initialization -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; Speed up initial startup
(setq
 gc-cons-threshold 402653184
 gc-cons-percentage 0.6)
(defvar colonq/file-name-handler-alist)
(setq colonq/file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

;; Inhibit the echo area message on startup (must be exactly this form)
(setq inhibit-startup-echo-area-message "llll")

(setq-default
 indent-tabs-mode nil ;; Default to indenting with spaces
 bidi-display-reordering nil
 cursor-in-non-selected-windows nil
 )

;; Set non-package-specific global variables
(setq
 max-specpdl-size 5000
 shell-file-name "/run/current-system/sw/bin/bash" ;; Full path to shell (NixOS)
 make-backup-files nil ;; Don't pullute folders with backups
 auto-save-default nil ;; Don't auto-save
 initial-major-mode 'eshell-mode ;; Start the *scratch* buffer in eshell-mode
 initial-scratch-message nil ;; Don't print a bunch of text in *scratch*
 completion-at-point-functions nil
 user-full-name "LLLL Colonq" ;; My full name
 custom-file "/dev/null" ;; Don't save customizations, just delete them
 recentf-max-saved-items nil ;; Save the entire recent files list
 recentf-keep '(recentf-keep-default-predicate remote-file-p) ;; Exclude remote files from the recent files list
 x-wait-for-event-timeout nil
 confirm-kill-emacs 'y-or-n-p ;; Ask before exiting Emacs
 disabled-command-function nil
 password-cache t ;; Cache passwords
 password-cache-expiry 3600 ;; Expire after one hour
 inhibit-startup-message t
 visible-cursor nil ;; Reduce cursor annoyance
 scroll-step 1 ;; Don't jump around so much while scrolling
 custom-theme-directory "~/.emacs.d/themes/" ;; Directory for custom themes (mostly unused lately)
 focus-follows-mouse nil ;; Prevent lsp-ui from stealing mouse focus
 sentence-end-double-space nil ;; Don't whine about spaces after periods
 )

(menu-bar-mode -1) ;; Don't display menu bar
(tool-bar-mode -1) ;; Don't display tool bar
(scroll-bar-mode -1) ;; Don't display scroll bar
(blink-cursor-mode -1) ;; Don't blink the cursor
(show-paren-mode -1) ;; Don't highlight parentheses
(tooltip-mode -1) ;; Don't display tooltips as popups, use the echo area instead
(global-hl-line-mode) ;; Highlight the current line in all buffers

(column-number-mode) ;; Display column number in the mode line
(recentf-mode) ;; Enable recording recently-visited files

;; Prevent C-g from closing windows
(defadvice keyboard-escape-quit
    (around keyboard-escape-quit-dont-close-windows activate)
  (let ((buffer-quit-function (lambda () ())))
    ad-do-it))

;; Human-friendly mode line configuration
(defun mode-line-render (left right)
  "Return a string of `window-width' length containing LEFT and RIGHT aligned respectively."
  (let* ((available-width (- (window-width) (length left) 3)))
    (format (format " %%s %%%ds " available-width) left right)))
(setq-default
 mode-line-format
 `((:eval
    (mode-line-render
     (concat
      (propertize (format-mode-line (buffer-name)) 'face 'bold)
      " - "
      (format-mode-line mode-name)
      " - "
      (colonq/replace-home default-directory))
     (format-mode-line '(line-number-mode (" line %l" (column-number-mode " column %c"))))))))

;; Load the rest of the configuration
(add-to-list 'load-path "~/.emacs.d/colonq")
(require 'colonq-core)
(require 'colonq-ui)
(require 'colonq-editor)
(require 'colonq-media)
(require 'colonq-lang)

;; Customization for comint
(setq comint-prompt-read-only t)

(evil-define-key 'insert comint-mode-map
  (kbd "<up>") #'comint-previous-input
  (kbd "<down>") #'comint-next-input)

(defun colonq/comint-eof ()
  "Send EOF and kill buffer."
  (interactive)
  (comint-send-eof)
  (let ((kill-buffer-query-functions nil))
    (kill-buffer)))

(define-key comint-mode-map (kbd "C-l") #'comint-clear-buffer)
(define-key comint-mode-map (kbd "C-d") #'colonq/comint-eof)
;; (define-key comint-mode-map (kbd "<tab>") #'company-complete)

;; After everything is loaded, stop some pesky modes from binding q
(define-key special-mode-map (kbd "q") 'colonq/dispatcher)
(define-key compilation-mode-map (kbd "q") 'colonq/dispatcher)
(define-key grep-mode-map (kbd "q") 'colonq/dispatcher)

(use-package hexl
  :config
  (define-key hexl-mode-map (kbd "M-h") #'windmove-left)
  (define-key hexl-mode-map (kbd "M-l") #'windmove-right)
  (define-key hexl-mode-map (kbd "M-k") #'windmove-up)
  (define-key hexl-mode-map (kbd "M-j") #'windmove-down))

;; Windmove bindings that should work in every mode, always
(global-set-key (kbd "M-h") #'windmove-left)
(global-set-key (kbd "M-l") #'windmove-right)
(global-set-key (kbd "M-k") #'windmove-up)
(global-set-key (kbd "M-j") #'windmove-down)

;; Let shell scripts know that we're inside Emacs
(setenv "INSIDE_EMACS" (format "%s,comint" emacs-version))

;; Start the server to enable emacsclient
(server-start)

;; Re-enable garbage collection after startup
(setq
 gc-cons-threshold 16777216
 gc-cons-percentage 0.1
 file-name-handler-alist colonq/file-name-handler-alist)

(provide 'init)
;;; init.el ends here
