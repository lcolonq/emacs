;;; colonq-selector --- selection and winnowing -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'colonq-package)

(use-package selector
  :load-path "~/lisp"
  :custom
  (completing-read-function #'selector-completing-read)
  (read-file-name-function #'selector-read-file-name)
  :config
  (global-set-key (kbd "M-x") 'selector-M-x)
  (evil-define-key 'normal selector-minibuffer-map (kbd "q") 'selector-quit)

  (defun colonq/selector-active-buffers ()
    (selector-source-create
     "IRC (Active)"
     :candidates
     (--map (selector-candidate-create it :face 'colonq/notify) (cl-remove-if-not #'colonq/buffer-active-p (colonq/buffer-list)))
     :actions
     selector-buffer-actions))
  
  (defun colonq/selector-irc-buffers ()
    (selector-source-create
     "IRC"
     :candidates
     (cl-remove-if-not #'colonq/buffer-irc-p (colonq/buffer-list))
     :actions
     selector-buffer-actions))

  (defun colonq/selector-eshell-buffers ()
    (selector-source-create
     "Shells"
     :candidates
     (mapcar (lambda (buf)
               (selector-candidate-create
                (concat "In " (colonq/replace-home (colonq/buffer-directory buf)) " (named " buf ")")
                :value buf))
             (cl-remove-if-not #'colonq/buffer-eshell-p (colonq/buffer-list)))
     :actions
     selector-buffer-actions))

  (defun colonq/selector-exwm-buffers ()
    (selector-source-create
     "Windows"
     :candidates
     (cl-remove-if-not #'colonq/buffer-exwm-p (colonq/buffer-list))
     :actions
     selector-buffer-actions))

  (defun colonq/selector-unaffiliated-buffers ()
    (selector-source-create
     "Buffers"
     :candidates
     (colonq/unaffiliated-buffers)
     :actions
     selector-buffer-actions))

  (defun colonq/selector-project-buffers ()
    (selector-source-create
     "Project Buffers"
     :candidates
     (when (projectile-project-p) (cl-remove-if #'colonq/buffer-boring-p (projectile-project-buffer-names)))
     :actions
     selector-buffer-actions))

  (defun colonq/selector-twitter-buffers ()
    (selector-source-create
     "Twitter"
     :candidates
     (cl-remove-if-not 'colonq/buffer-twitter-p (colonq/buffer-list))
     :actions
     selector-buffer-actions))

  (defun colonq/selector-project-files ()
    (selector-source-create
     "Project Files"
     :candidates
     (when (projectile-project-p)
       (cl-loop with root = (projectile-project-root)
                for display in (projectile-current-project-files)
                collect (selector-candidate-create display :value (expand-file-name display root))))
     :actions
     selector-file-actions))

  (defun colonq/selector-create-file-or-buffer ()
    (selector-source-create
     "Other"
     :candidates
     (list (selector-candidate-create
            "Create buffer"
            :type 'dummy
            :action (lambda (_) (switch-to-buffer (selector-input))))
           (selector-candidate-create
            "Create file"
            :type 'dummy
            :action (lambda (_) (find-file (selector-input)))))))

  (defun colonq/navigate ()
    (interactive)
    (selector (list (colonq/selector-active-buffers)
                 (colonq/selector-project-buffers)
                 (colonq/selector-exwm-buffers)
                 (colonq/selector-unaffiliated-buffers)
                 (colonq/selector-eshell-buffers)
                 (colonq/selector-project-files)
                 (colonq/selector-bookmarks)
                 (selector-recentf-source)
                 (colonq/selector-search)
                 (colonq/selector-create-file-or-buffer))))

  (defvar colonq/external-commands-list nil)
  (defun colonq/run-external-command ()
    (interactive)
    ;; (unless colonq/external-commands-list
      (setq colonq/external-commands-list
            (cl-loop
             for dir in (split-string (getenv "PATH") path-separator)
             when (and (file-exists-p dir) (file-accessible-directory-p dir))
             for lsdir = (cl-loop for i in (directory-files dir t)
                                  for bn = (file-name-nondirectory i)
                                  when (and (not (member bn completions))
                                            (not (file-directory-p i))
                                            (file-executable-p i))
                                  collect bn)
             append lsdir into completions
             finally return (sort completions 'string-lessp)))
      ;; )
    (selector (list (selector-source-create
                  "Commands"
                  :candidates
                  colonq/external-commands-list
                  :actions
                  (list (lambda (cmd) (start-process cmd nil cmd)))))
           :initial "^"))

  (defun colonq/selector-rg ()
    "Sources for lines found via grep (or a clone)."
    (interactive)
    (let ((query (read-string "rg: ")))
      (defun conv (x)
        (cons (car x) (cons (- (string-to-number (cadr x)) 1) (caddr x))))
      (defun all-in-file (key list)
        (--map (to-candidate (cdr it)) (--filter (s-equals? key (car it)) list)))
      (defun to-candidate (x)
        (selector-candidate-create (cdr x) :value (car x)))
      (let* ((result (with-temp-buffer
                       (call-process "rg" nil t nil "-n" query ".")
                       (buffer-string)))
             (lines (--map (conv (s-split-up-to ":" it 2)) (--filter (not (s-blank? it)) (s-split "\n" result))))
             (files (-uniq (-map #'car lines)))
             (sources (--map (selector-source-create
                              it
                              :candidates (all-in-file it lines)
                              :actions (selector-file-contents-actions it))
                             files)))
        (when (not (null sources))
          (selector sources)))))
  )

(provide 'colonq-selector)
;;; colonq-selector.el ends here
