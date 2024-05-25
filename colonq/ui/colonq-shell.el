;;; colonq-shell --- shell -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'colonq-package)
(require 'colonq-hydra)

(use-package auth-source
  :custom
  (auth-source-save-behavior nil))

(defvar colonq/eshell-truncate-timer nil)

(defun colonq/shell (dir)
  "Open EShell in DIR."
  (interactive)
  (let ((bufname (generate-new-buffer-name eshell-buffer-name)))
    (switch-to-buffer bufname)
    (cd dir)
    (eshell-mode)))

(defun colonq/shell-here ()
  "Open EShell in the current directory."
  (interactive)
  (colonq/shell default-directory))

(defun colonq/truncate-eshell-buffers ()
  "Truncate all EShell buffers."
  (interactive)
  (save-current-buffer
    (dolist (buffer (buffer-list t))
      (set-buffer buffer)
      (when (eq major-mode 'eshell-mode)
	(eshell-truncate-buffer)))))

(defun colonq/eshell-clear ()
  "Clear EShell buffer."
  (interactive)
  (let ((inhibit-read-only t))
    (delete-all-overlays)
    (set-text-properties (point-min) (point-max) nil)
    (erase-buffer))
  (eshell-emit-prompt))

(defun colonq/eshell-eof ()
  "Send EOF or kill buffer."
  (interactive)
  (if eshell-process-list
      (eshell-send-eof-to-process)
    (kill-this-buffer)))

(use-package esh-module)
(add-to-list 'eshell-modules-list 'eshell-tramp)

(use-package em-banner
  :config
  (setq eshell-banner-message ""))

(use-package em-prompt
  :custom
  (eshell-prompt-regexp (rx bol "In " (one-or-more anything) ":\n"))
  :config
  (ef-themes-with-colors
    (set-face-attribute 'eshell-prompt nil
                        :foreground fg-main
                        :background bg-alt
                        :weight 'bold
                        :extend t))
  (defun colonq/eshell-prompt ()
    (let ((branch (magit-get-current-branch)))
      (concat "In " (colonq/replace-home default-directory)
              (when branch
                (concat " (branch " branch ")"))
              ":\n")))
  (defun colonq/eshell-previous-prompt ()
    (interactive)
    (eshell-previous-prompt 1)
    (forward-line -1))
  (defun colonq/eshell-next-prompt ()
    (interactive)
    (eshell-next-prompt 1)
    (forward-line 1))
  (setq eshell-prompt-function #'colonq/eshell-prompt)
  (add-hook 'eshell-after-prompt-hook
            (lambda ()
              (setq buffer-undo-list nil buffer-undo-tree nil)
              (set-buffer-modified-p nil))))

(use-package em-hist
  :config
  (setq eshell-save-history-on-exit t))

(use-package em-cmpl
  :config
  (defun eshell-complete-parse-arguments ()
    "Parse the command line arguments for `pcomplete-argument'."
    (when (and eshell-no-completion-during-jobs
               (eshell-interactive-process))
      ;(insert-and-inherit "\t")
      (throw 'pcompleted t))
    (let ((end (point-marker))
          (begin (save-excursion (eshell-bol) (point)))
          (posns (list t))
          args delim)
      (when (memq this-command '(pcomplete-expand
                                 pcomplete-expand-and-complete))
        (run-hook-with-args 'eshell-expand-input-functions begin end)
        (if (= begin end)
            (end-of-line))
        (setq end (point-marker)))
      (if (setq delim
                (catch 'eshell-incomplete
                  (ignore
                   (setq args (eshell-parse-arguments begin end)))))
          (cond ((memq (car delim) '(?\{ ?\<))
                 (setq begin (1+ (cadr delim))
                       args (eshell-parse-arguments begin end)))
                ((eq (car delim) ?\()
                 (eshell-complete-lisp-symbol)
                 (throw 'pcompleted t))
                (t
                 ;(insert-and-inherit "\t")
                 (throw 'pcompleted t))))
      (when (get-text-property (1- end) 'comment)
        ;(insert-and-inherit "\t")
        (throw 'pcompleted t))
      (let ((pos begin))
        (while (< pos end)
          (if (get-text-property pos 'arg-begin)
              (nconc posns (list pos)))
          (setq pos (1+ pos))))
      (setq posns (cdr posns))
      (cl-assert (= (length args) (length posns)))
      (let ((a args)
            (i 0)
            l)
        (while a
          (if (and (consp (car a))
                   (eq (caar a) 'eshell-operator))
              (setq l i))
          (setq a (cdr a) i (1+ i)))
        (and l
             (setq args (nthcdr (1+ l) args)
                   posns (nthcdr (1+ l) posns))))
      (cl-assert (= (length args) (length posns)))
      (when (and args (eq (char-syntax (char-before end)) ? )
                 (not (eq (char-before (1- end)) ?\\)))
        (nconc args (list ""))
        (nconc posns (list (point))))
      (cons (mapcar
             (function
              (lambda (arg)
                (let ((val
                       (if (listp arg)
                           (let ((result
                                  (eshell-do-eval
                                   (list 'eshell-commands arg) t)))
                             (cl-assert (eq (car result) 'quote))
                             (cadr result))
                         arg)))
                  (if (numberp val)
                      (setq val (number-to-string val)))
                  (or val ""))))
             args)
            posns))))

(use-package eshell
  :config
  (setq eshell-buffer-maximum-lines 5000
        eshell-output-filter-functions '(eshell-postoutput-scroll-to-bottom
                                         eshell-handle-control-codes
                                         eshell-handle-ansi-color
                                         eshell-watch-for-password-prompt))
  (defun colonq/eshell-setup ()
    "Custom configuration for Eshell."
    (show-paren-mode -1)
    (company-mode)
    (setq-local company-backends '(company-pcomplete))
    (setq-local company-idle-delay nil)
    (defun eshell/clear ()
      (interactive)
      (let ((inhibit-read-only t))
        (delete-all-overlays)
        (set-text-properties (point-min) (point-max) nil)
        (erase-buffer)))
    (defun eshell/open (file)
      (interactive)
      (find-file file))
    (setenv "TERM" "dumb")
    (setenv "PAGER" "cat")
    (setenv "EDITOR" "emacsclient")
    (define-key eshell-mode-map (kbd "<tab>") 'company-complete)
    (define-key eshell-mode-map (kbd "<backtab>") 'colonq/nop)
    (define-key eshell-mode-map (kbd "C-l") 'colonq/eshell-clear)
    (define-key eshell-mode-map (kbd "C-c") 'eshell-interrupt-process)
    (evil-define-key 'normal eshell-mode-map (kbd "C-d") nil)
    (evil-define-key 'motion eshell-mode-map (kbd "C-d") nil)
    (evil-define-key 'insert eshell-mode-map (kbd "C-d") nil)

    (evil-define-key 'normal eshell-mode-map (kbd "C-k") #'colonq/eshell-previous-prompt)
    (evil-define-key 'motion eshell-mode-map (kbd "C-k") #'colonq/eshell-previous-prompt)
    (evil-define-key 'insert eshell-mode-map (kbd "C-k") #'colonq/eshell-previous-prompt)

    (evil-define-key 'normal eshell-mode-map (kbd "C-j") #'colonq/eshell-next-prompt)
    (evil-define-key 'motion eshell-mode-map (kbd "C-j") #'colonq/eshell-next-prompt)
    (evil-define-key 'insert eshell-mode-map (kbd "C-j") #'colonq/eshell-next-prompt)

    (define-key eshell-mode-map (kbd "C-d") 'colonq/eshell-eof)
    (erase-buffer)
    (eshell-emit-prompt))
  (add-hook 'eshell-mode-hook 'colonq/eshell-setup)
  (setq colonq/eshell-truncate-timer (run-with-idle-timer 5 t 'colonq/truncate-eshell-buffers)))

(use-package fish-completion
  :config
  (global-fish-completion-mode))

(use-package direnv
  :config
  (direnv-mode))

(provide 'colonq-shell)
;;; colonq-shell ends here
