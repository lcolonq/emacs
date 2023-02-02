;;; colonq-utility --- miscellaneous utility functions -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'colonq-package)

(use-package cl-lib)
(use-package s)
(use-package dash)
(use-package f)

(defvar colonq/boring-buffer-regexp-list
  '("\\` "
    "\\`\\*dhall"
    "\\`\\*poetry"
    "\\`\\*tramp"
    "\\`\\*Org PDF"
    "\\`\\*Org Preview"
    "\\`\\magit-process"
    "\\`\\*Echo Area"
    "\\`\\*Minibuf"
    "\\`\\*eldoc"
    "*direnv*"
    "*Pinentry*"
    "*Shell Command Output*"
    "*poetry*"))

(defvar-local colonq/buffer-notify nil)

(defface colonq/notify
  '((t :underline t
       :foreground "red"
       :weight bold))
  "Face used for alerts in `colonq/navigate'.")

(defvar colonq/home (getenv "HOME"))

(defun colonq/buffer-active-p (buf)
  "Check if BUF is active."
  (buffer-local-value 'colonq/buffer-notify (get-buffer buf)))

(defun colonq/buffer-org-p (buf)
  "Check if BUF is an `org-mode' buffer."
  (member
   (buffer-local-value 'major-mode (get-buffer buf))
   '(org-mode)))

(defun colonq/buffer-irc-p (buf)
  "Check if BUF is a Circe IRC buffer."
  (member
   (buffer-local-value 'major-mode (get-buffer buf))
   '(circe-server-mode circe-channel-mode circe-query-mode)))

(defun colonq/buffer-eshell-p (buf)
  "Check if BUF is an EShell buffer."
  (member
   (buffer-local-value 'major-mode (get-buffer buf))
   '(eshell-mode)))

(defun colonq/buffer-exwm-p (buf)
  "Check if BUF is an EXWM buffer."
  (member
   (buffer-local-value 'major-mode (get-buffer buf))
   '(exwm-mode)))

(defun colonq/minor-modes ()
  "Return all the minor modes of the current buffer."
  (cl-remove-if (lambda (x) (not (and (symbolp x) (symbol-value x)))) (mapcar 'car minor-mode-alist)))

(defun colonq/buffer-directory (buf)
  "Return the `default-directory' of BUF."
  (buffer-local-value 'default-directory (get-buffer buf)))

(defun colonq/buffer-boring-p (buffer)
  "Return non-nil if BUFFER is boring."
  (cl-reduce (lambda (x y) (or x y)) (mapcar (lambda (r) (string-match r buffer)) colonq/boring-buffer-regexp-list)))

(defun colonq/buffer-list ()
  "Return list of non-boring buffers."
  (cl-remove-if 'colonq/buffer-boring-p (mapcar 'buffer-name (buffer-list))))

(defun colonq/unaffiliated-buffers ()
  "Return a list of unaffiliated buffer names."
  (cl-remove-if (lambda (b) (or (colonq/buffer-irc-p b)
                                (colonq/buffer-eshell-p b)
                                (colonq/buffer-exwm-p b)))
                (colonq/buffer-list)))

(defun colonq/previous-buffer ()
  "Switch to the previous buffer."
  (interactive)
  (switch-to-buffer (cadr (colonq/buffer-list))))

(defun colonq/replace-home (dir)
  "Replace home in DIR with tilde."
  (interactive)
  (if (file-remote-p dir) dir
    (s-replace colonq/home "~" dir)))

(defun colonq/dirname (path)
  "Return the innermost directory in PATH."
  (file-name-nondirectory (directory-file-name (file-name-directory path))))

(defun colonq/read-file (path)
  "Read the first s-expression in the file at PATH."
  (with-temp-buffer
    (insert-file-contents path)
    (read (current-buffer))))

(defun colonq/nop ()
  "Do nothing."
  (interactive)
  nil)

(defun colonq/git-dirty (dir)
  "Return non-nil if Git repository DIR has uncommitted edits."
  (let ((out (process-lines "git" "-C" dir "diff-index" "--quiet" "HEAD" "--")))
    (not (null out))))

(defun colonq/screenshot ()
  "Take a screenshot."
  (interactive)
  (start-process "scrot" nil "scrot" "-e" "mv $f ~/shots/; emacsclient -n ~/shots/$f"))

(defun colonq/snip ()
  "Copy an area of the screen to an image."
  (interactive)
  (start-process "scrot" nil "scrot" "-s" "-e" "mv $f ~/shots/; emacsclient -n ~/shots/$f"))

(defun colonq/pop-mark ()
  "Pop off mark ring into the buffer's actual mark.
Does not set point.  Does nothing if mark ring is empty."
  (when mark-ring
    ;; (setq mark-ring (nconc mark-ring (list (copy-marker (mark-marker)))))
    (set-marker (mark-marker) (car mark-ring))
    (set-marker (car mark-ring) nil)
    (unless (mark t) (ding))
    (pop mark-ring))
  (deactivate-mark))

(defun colonq/pop-to-mark-command ()
  "Jump to mark, and pop a new position for mark off the ring.
\(Does not affect global mark ring)."
  (interactive)
  (if (null (mark t))
      (user-error "No mark set in this buffer")
    (if (= (point) (mark t))
	(message "Mark popped"))
    (goto-char (mark t))
    (colonq/pop-mark)))

(defun colonq/unpop-to-mark-command ()
  "Unpop off mark ring.  Does nothing if mark ring is empty."
  (interactive)
  (when mark-ring
    ;; (setq mark-ring (cons (copy-marker (mark-marker)) mark-ring))
    (set-marker (mark-marker) (car (last mark-ring)) (current-buffer))
    (when (null (mark t)) (ding))
    (setq mark-ring (nbutlast mark-ring))
    (goto-char (marker-position (car (last mark-ring))))))

(defsubst colonq/dir-file-name (file dir)
  (expand-file-name
   (substring file 0 (1- (length file))) dir))

(defsubst colonq/dir-name-p (str)
  (char-equal (aref str (1- (length str))) ?/))

(cl-defun colonq/walk-directory (directory &key (path 'basename)
                                           directories
                                           match noerror)
  "Walk through DIRECTORY tree.
Argument PATH can be one of basename, relative, full, or a
function called on file name, default to basename.
Argument DIRECTORIES when t return also directories names,
otherwise skip directories names, with a value of `only' returns
only subdirectories, i.e. files are skipped.
Argument MATCH is a regexp matching files or directories.
Argument NOERROR when t will skip directories which are not
accessible."
  (let ((fn (cl-case path
               (basename 'file-name-nondirectory)
               (relative 'file-relative-name)
               (full     'identity)
               (t        path)))) ; A function.
    (cl-labels ((ls-rec (dir)
                  (unless (file-symlink-p dir)
                    (cl-loop for f in (sort (file-name-all-completions "" dir)
                                            'string-lessp)
                             unless (member f '("./" "../"))
                             if (and (colonq/dir-name-p f)
                                     (colonq/dir-file-name f dir))
                             nconc
                             (unless (or (and noerror
                                              (not (file-accessible-directory-p it))))
                               (if (and directories
                                        (or (null match)
                                            (string-match match f)))
                                   (nconc (list (concat (funcall fn it) "/"))
                                          (ls-rec it))
                                   (ls-rec it)))
                             ;; A regular file.
                             else nconc
                             (when (and (null (eq directories 'only))
                                        (or (null match) (string-match match f)))
                               (list (funcall fn (expand-file-name f dir))))))))
      (ls-rec directory))))

(provide 'colonq-utility)
;;; colonq-utility.el ends here
