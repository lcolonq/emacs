;;; selector.el --- Efficient selection and navigation -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:
(require 'dash)
(require 'f)
(require 'subr-x)
(require 'recentf)
(require 'evil)

(defgroup selector nil
  "Efficient selection and navigation."
  :group 'convenience)

(defgroup selector-faces nil
  "Faces for `selector'."
  :group 'selector
  :group 'faces)

(defface selector-source-name
  '((default :underline t :inherit bold)
    (((class color) (background dark)) :foreground "white"))
  "Face used to highlight source names.")

(defface selector-highlight
  '((t :inherit highlight))
  "Face used to highlight the current selection.")

(defvar selector-minibuffer-lines 20
  "Number of lines to display in the minibuffer.")

(defvar selector-exit-hook nil
  "Hook run when exiting minibuffer selection.")

(defvar selector--sources '())
(defvar selector--last nil)
(defvar selector--matching '())
(defvar selector--source 0)
(defvar selector--index 0)
(defvar selector--action nil)
(defvar selector--result nil)

(defvar selector-minibuffer-map (make-sparse-keymap))
(define-key selector-minibuffer-map (kbd "\\") (lambda () (interactive) nil))
(define-key selector-minibuffer-map (kbd "C-g") 'selector-quit)
(define-key selector-minibuffer-map (kbd "C-c") 'selector-quit)
(define-key selector-minibuffer-map (kbd "<return>") 'selector-do)
(define-key selector-minibuffer-map (kbd "<backtab>") 'selector-previous)
(define-key selector-minibuffer-map (kbd "<tab>") 'selector-next)
(define-key selector-minibuffer-map (kbd "<up>") 'selector-previous)
(define-key selector-minibuffer-map (kbd "<down>") 'selector-next)
(define-key selector-minibuffer-map (kbd "<prior>") 'selector-previous-source)
(define-key selector-minibuffer-map (kbd "<next>") 'selector-next-source)

(evil-define-key 'normal selector-minibuffer-map
  (kbd "k") 'selector-previous
  (kbd "j") 'selector-next
  (kbd "K") 'selector-previous-source
  (kbd "J") 'selector-next-source)

(defun selector-minibuffer-line (str)
  "Write STR to the minibuffer."
  (goto-char (point-max))
  (insert (concat "\n" str)))

(defun selector-minibuffer-line-face (str face)
  "Write STR to the minibuffer in FACE."
  (let ((before (point)))
    (selector-minibuffer-line str)
    (goto-char before)
    (forward-line)
    (put-text-property (line-beginning-position) (point-max) 'face face)))

(defun selector-minibuffer-clear ()
  "Clear minibuffer."
  (save-excursion
    (goto-char (minibuffer-prompt-end))
    (delete-region (line-end-position) (point-max))))

(defun selector-minibuffer-input ()
  "Get current minibuffer input."
  (buffer-substring-no-properties
   (minibuffer-prompt-end)
   (line-end-position)))

(cl-defstruct (selector-candidate (:constructor selector-candidate--create)
                               (:copier nil)
                               (:conc-name selector-candidate--))
  type (display nil :type string) face value action)

(cl-defun selector-candidate-create
    (display &key
             value
             (type 'normal)
             (face 'default)
             (action '()))
  "Create a candidate.
DISPLAY is the string to display (using FACE) / match against.
VALUE is the value to pass to actions when the candidate is selected.
TYPE is either normal or dummy - dummy candidates always appear in the
results list regardless of the input pattern.
ACTION is an alist mapping keybindings to candidate-specific actions."
  (selector-candidate--create
   :type type
   :display display
   :face face
   :value (if value value display)
   :action action))

(defun selector-candidate-display (candidate)
  "Return the display string for CANDIDATE."
  (cond ((selector-candidate-p candidate) (selector-candidate--display candidate))
        (t candidate)))

(defun selector-candidate-value (candidate)
  "Return the value of CANDIDATE."
  (cond ((selector-candidate-p candidate) (selector-candidate--value candidate))
        (t candidate)))

(defun selector-candidate-type (candidate)
  "Return the candidate type of CANDIDATE."
  (cond ((selector-candidate-p candidate) (selector-candidate--type candidate))
        (t 'normal)))

(defun selector-candidate-face (candidate)
  "Return the display face for CANDIDATE."
  (cond ((selector-candidate-p candidate) (selector-candidate--face candidate))
        (t 'default)))

(defun selector-candidate-action (candidate)
  "Return the actions for CANDIDATE."
  (cond ((selector-candidate-p candidate) (selector-candidate--action candidate))
        (t '())))

(defun selector-candidate-display-string (candidate)
  "Return the display of CANDIDATE as a string."
  (let ((display (selector-candidate-display candidate)))
    (cond ((stringp display) display)
          (t (error "Invalid candidate display %s for candidate %s (of type %s)"
                    display candidate (type-of display))))))

(defun selector-highlight-candidate (candidate)
  "Return a copy of CANDIDATE with the face set to selector-highlight."
  (selector-candidate--create
   :type (selector-candidate-type candidate)
   :display (selector-candidate-display candidate)
   :face 'selector-highlight
   :value (selector-candidate-value candidate)
   :action (selector-candidate-action candidate)))

(defun selector-match (candidate regex)
  "Determine whether CANDIDATE is a match for REGEX."
  (let ((type (selector-candidate-type candidate)))
    (cond ((eq 'dummy type) t)
          (t (string-match-p regex (selector-candidate-display-string candidate))))))

(cl-defstruct (selector-source (:constructor selector-source--create)
                            (:copier nil))
  name candidates actions keymap)

(defun selector-action-function (action)
  "Return the function associated with ACTION."
  (if (functionp action)
      action
    (cdr action)))

(defun selector-actions-keymap (actions)
  "Return a keymap for ACTIONS."
  (let ((keymap (make-sparse-keymap)))
    (set-keymap-parent keymap selector-minibuffer-map)
    (mapc
     (lambda (a)
       (unless (functionp a)
         (define-key keymap (car a)
           (lambda () (interactive)
             (selector-do (cdr a))))))
     actions)
    keymap))

(cl-defun selector-source-create (name &key candidates (actions '()))
  "Create a new source named NAME with the given CANDIDATES and ACTIONS.

CANDIDATES is either:
- A list of candidates
- A function returning a list of candidates given a regex
ACTIONS is a list of actions, which can be:
- functions taking candidate values as arguments
- pairs of key strings and such functions"
  (selector-source--create
   :name name
   :candidates
   (if (functionp candidates) candidates
     (--map (if (selector-candidate-p it) it (selector-candidate-create it))
            candidates))
   :actions actions
   :keymap (selector-actions-keymap actions)))

(defun selector-matching-candidates (candidates pattern)
  "Return the candidates in CANDIDATES matching PATTERN."
  (cond ((functionp candidates) (funcall candidates pattern))
        (t (let ((regex (selector-pattern-regex pattern)))
             (--filter (selector-match it regex) candidates)))))

(defun selector-filter-source (source pattern)
  "Return a copy of SOURCE including only the candidates matching PATTERN."
  (selector-source--create
   :name (selector-source-name source)
   :candidates (selector-matching-candidates (selector-source-candidates source) pattern)
   :actions (selector-source-actions source)
   :keymap (selector-source-keymap source)))

(defun selector-pattern-regex (pattern)
  "Convert PATTERN into a regular expression."
  (apply #'string-join
         (--map (concat "\\(" it "\\)") (split-string pattern))
         '(".*")))

(defun selector-matching-sources (sources pattern)
  "Return the sources in SOURCES matching PATTERN."
  (let* ((matches (--map (selector-filter-source it pattern) sources)))
    (-filter #'selector-source-candidates matches)))

(defun selector-display-source (source)
  "Display SOURCE."
  (when source
    (selector-minibuffer-line-face (selector-source-name source) 'selector-source-name)
    (--map (selector-minibuffer-line-face
            (selector-candidate-display-string it)
            (selector-candidate-face it))
           (selector-source-candidates source))))

(defun selector-nearby (sources)
  "Filter SOURCES to only include candidates close to the selected candidate."
  (let* ((adjacent
          (--map
           (cond ((and (< (cdr it) (+ selector--source selector-minibuffer-lines))
                       (> (cdr it) selector--source))
                  (cons (car it) 'g))
                 ((= (cdr it) selector--source)
                  (cons (car it) 'e))
                 (t nil))
           (-zip-pair sources (number-sequence 0 (length sources))))))
    (--map
     (when it
       (let* ((candidates (selector-source-candidates (car it))))
         (selector-source--create
          :name (selector-source-name (car it))
          :candidates
          (cond ((eq (cdr it) 'g)
                 (-take selector-minibuffer-lines candidates))
                (t
                 (cl-loop for i from (max (- selector--index
                                             (- (/ selector-minibuffer-lines 2) 1))
                                          0)
                          for j in (-take
                                    selector-minibuffer-lines
                                    (-drop
                                     (- selector--index
                                        (- (/ selector-minibuffer-lines 2) 1))
                                     candidates))
                          collect (if (= i selector--index)
                                      (selector-highlight-candidate j)
                                    j))))
          :actions (selector-source-actions (car it))
          :keymap (selector-source-keymap (car it)))))
     adjacent)))

(defun selector-update-transient-map ()
  "Update the transient keymap to match the current source."
  (let ((source (car (nthcdr selector--source selector--matching))))
    (when source
      (set-transient-map (selector-source-keymap source)))))

(defun selector-minibuffer-render ()
  "Draw matching candidates to minibuffer."
  (save-excursion
    (let ((pattern (selector-minibuffer-input)))
      (unless (string= pattern selector--last)
        (setq selector--last pattern
              selector--index 0
              selector--source 0
              selector--matching (selector-matching-sources selector--sources pattern))))
    (-map #'selector-display-source (selector-nearby selector--matching))
    (goto-char (minibuffer-prompt-end))
    (put-text-property (line-end-position) (point-max) 'readonly t))
  (selector-update-transient-map))

(defun selector-minibuffer-setup (initial)
  "Ready minibuffer for completion with INITIAL as initial input."
  (add-hook 'pre-command-hook 'selector-minibuffer-clear nil t)
  (add-hook 'post-command-hook 'selector-minibuffer-render nil t)
  (setq-local max-mini-window-height selector-minibuffer-lines)
  (when initial
    (save-excursion
      (minibuffer-prompt-end)
      (insert initial)))
  (end-of-line)
  (selector-update-transient-map)
  (when (fboundp 'evil-insert-state)
    (evil-insert-state)))

(defun selector-previous-source ()
  "Move to the previous source."
  (interactive)
  (setq selector--index 0)
  (setq selector--source (if (= selector--source 0)
                          (- (length selector--matching) 1)
                        (- selector--source 1))))

(defun selector-next-source ()
  "Move to the next source."
  (interactive)
  (setq selector--index 0)
  (setq selector--source (% (+ selector--source 1) (length selector--matching))))

(defun selector-previous ()
  "Move to the previous candidate."
  (interactive)
  (let* ((new-source-index (if (= selector--source 0)
                               (- (length selector--matching) 1)
                             (- selector--source 1)))
         (source (car (nthcdr new-source-index selector--matching))))
    (setq selector--index (- selector--index 1))
    (when (< selector--index 0)
      (setq selector--index (- (length (selector-source-candidates source)) 1)
            selector--source new-source-index))))

(defun selector-next ()
  "Move to the next candidate."
  (interactive)
  (let* ((source (car (nthcdr selector--source selector--matching))))
    (setq selector--index (+ selector--index 1))
    (when (= selector--index (length (selector-source-candidates source)))
      (setq selector--index 0
            selector--source (% (+ selector--source 1) (length selector--matching))))))

(defun selector-quit ()
  "Quit the selection interface without running an action."
  (interactive)
  (run-hooks 'selector-exit-hook)
  (keyboard-escape-quit))

(defun selector-do (&optional action-function)
  "Act upon selected candidate.
If ACTION-FUNCTION is given use it, otherwise use the first action for the candidate."
  (interactive)
  (if (null selector--matching)
      (progn
        (setq selector--action (lambda (x) x)
              selector--result nil))
    (progn
      (let* ((source (car (nthcdr selector--source selector--matching)))
             (candidate (car (nthcdr selector--index (selector-source-candidates source)))))
        (setq selector--action (cond (action-function
                                   action-function)
                                  ((selector-candidate-action candidate)
                                   (selector-candidate-action candidate))
                                  (t
                                   (let ((actions (selector-source-actions source)))
                                     (if actions
                                         (selector-action-function (car actions))
                                       (lambda (x) x)))))
              selector--result (selector-candidate-value candidate)))))
  (run-hooks 'selector-exit-hook)
  (exit-minibuffer))

;;;###autoload
(cl-defun selector (sources &key prompt initial)
  "Select a candidate and run an action using SOURCES.
Display PROMPT as the prompt, or \"pattern: \" if not given.
Use INITIAL as the initial input."
  (setq selector--sources sources
        selector--last nil
        selector--matching (selector-matching-sources sources "")
        selector--source 0
        selector--index 0
        selector--action nil
        selector--result nil)
  (let ((inhibit-message t))
    (minibuffer-with-setup-hook
        (apply-partially 'selector-minibuffer-setup initial)
      (read-from-minibuffer (or prompt "pattern: ") nil selector-minibuffer-map)))
  (funcall selector--action selector--result))

;;;###autoload
(defun selector-completing-read (prompt collection &optional predicate require-match
                                     initial-input hist def inherit-input-method)
  "Replacement for `completing-read'.
PROMPT, COLLECTION, PREDICATE, REQUIRE-MATCH, INITIAL-INPUT, HIST, DEF, and
INHERIT-INPUT-METHOD have the same meaning as in `completing-read'."
  (ignore predicate hist def inherit-input-method)
  (let ((unspecified-source
         (if require-match
             '()
           (list
            (selector-source-create
             "Other"
             :candidates
             (list (selector-candidate-create
                    "Specify"
                    :type 'dummy
                    :action (lambda (_) (selector-input)))))))))
    (or
     (cond ((functionp collection)
            (selector (cons (selector-source-create "Completions" :candidates (funcall collection "" nil t)) unspecified-source)
                   :prompt prompt
                   :initial initial-input))
           ((hash-table-p collection)
            (selector (cons (selector-source-create "Completions" :candidates (hash-table-keys collection)) unspecified-source)
                   :prompt prompt
                   :initial initial-input))
           ((obarrayp collection)
            (let ((candidates (list)))
              (mapatoms (lambda (x) (push (selector-candidate-create (symbol-name x)) candidates)) collection)
              (selector (cons (selector-source-create "Completions" :candidates candidates) unspecified-source)
                     :prompt prompt
                     :initial initial-input)))
           (t (selector (cons (selector-source-create
                            "Completions"
                            :candidates
                            (--map (if (consp it)
                                       (selector-candidate-create (car it))
                                     it)
                                   collection))
                           unspecified-source)
                     :prompt prompt
                     :initial initial-input)))
     (selector-input))))

(defun selector-input () "Return last minibuffer input." selector--last)

(defvar selector-extended-command-actions
  (list (lambda (c)
          (add-to-list 'extended-command-history c)
          (command-execute (intern-soft c)))
        (cons (kbd "C-h") (lambda (c) (describe-function (intern-soft c))))))

;;;###autoload
(defun selector-extended-commands-source ()
  "Source for extended commands (`M-x')."
  (selector-source-create
   "Commands"
   :candidates
   (-map
    #'selector-candidate-create
    (all-completions "" obarray #'commandp))
   :actions
   selector-extended-command-actions))

;;;###autoload
(defun selector-extended-command-history-source ()
  "Source for extended command history."
  (selector-source-create
   "Command History"
   :candidates
   (-map
    #'selector-candidate-create
    extended-command-history)
   :actions
   selector-extended-command-actions))

;;;###autoload
(defun selector-apropos-command-source ()
  "Source for command lookup."
  (selector-source-create
   "Commands"
   :candidates
   (lambda (r) (-map #'selector-candidate-create (all-completions r obarray #'commandp)))
   :actions
   (list (lambda (c) (describe-function (intern-soft c))))))

;;;###autoload
(defun selector-apropos-function-source ()
  "Source for function lookup."
  (selector-source-create
   "Functions"
   :candidates
   (lambda (r) (-map #'selector-candidate-create (all-completions r obarray #'fboundp)))
   :actions
   (list (lambda (c) (describe-function (intern-soft c))))))

;;;###autoload
(defun selector-apropos-variable-source ()
  "Source for variable lookup."
  (selector-source-create
   "Variables"
   :candidates
   (lambda (r) (-map #'selector-candidate-create
                     (all-completions
                      r obarray
                      (lambda (x) (let ((sym (intern-soft x)))
                                    (and (boundp sym) (not (keywordp sym))))))))
   :actions
   (list (lambda (c) (describe-variable (intern-soft c))))))

(defvar selector-buffer-actions
  (list 'switch-to-buffer
        (cons (kbd "M-D") 'kill-buffer)))

;;;###autoload
(defun selector-buffers-source (&optional sort-pred)
  "Source for open buffers.
An optional SORT-PRED may be provided to sort the buffers (see `sort')."
  (selector-source-create
   "Buffers"
   :candidates
   (--map (selector-candidate-create (buffer-name it))
          (if sort-pred (sort (buffer-list) sort-pred) (buffer-list)))
   :actions
   selector-buffer-actions))

;;;###autoload
(defun selector-create-buffer-source ()
  "Dummy source to create a buffer."
  (selector-source-create
   "Other"
   :candidates
   (list (selector-candidate-create
          "Create buffer"
          :type 'dummy
          :action (lambda (_) (switch-to-buffer (selector-input)))))))

(defvar selector-file-actions
  (list 'find-file
        (cons (kbd "M-D") (lambda (f)
                            (when (y-or-n-p (concat "Delete file " f "? "))
                              (delete-file f))))))

;;;###autoload
(defun selector-files-source ()
  "Source for files in current directory."
  (selector-source-create
   "Files"
   :candidates
   (-map #'selector-candidate-create (directory-files default-directory))
   :actions
   selector-file-actions))

;;;###autoload
(defun selector-create-file-source ()
  "Dummy source to create a file."
  (selector-source-create
   "Other"
   :candidates
   (list (selector-candidate-create
          "Create file"
          :type 'dummy
          :action (lambda (_) (find-file (selector-input)))))))

;;;###autoload
(defun selector-recentf-source ()
  "Source for recentf."
  (selector-source-create
   "Recent Files"
   :candidates
   (-map #'selector-candidate-create recentf-list)
   :actions
   selector-file-actions))

;;;###autoload
(defun selector-M-x ()
  "Preconfigured `selector' interface to replace `execute-external-command'."
  (interactive)
  (selector (list (selector-extended-command-history-source)
               (selector-extended-commands-source))))

;;;###autoload
(defun selector-apropos (&optional initial)
  "Preconfigured `selector' interface to replace `apropos'.
INITIAL is the initial text to match."
  (interactive)
  (selector (list (selector-apropos-command-source)
               (selector-apropos-function-source)
               (selector-apropos-variable-source))
         :initial (if initial initial (thing-at-point 'symbol t))))

;;;###autoload
(defun selector-for-buffers ()
  "Preconfigured `selector' interface for open buffers and recentf."
  (interactive)
  (selector (list (selector-buffers-source)
               (selector-recentf-source)
               (selector-create-buffer-source))))

;;;###autoload
(defun selector-for-files ()
  "Preconfigured `selector' interface for files in the current directory."
  (interactive)
  (selector (list (selector-files-source)
               (selector-create-file-source))))

;;;###autoload
(defun selector-read-file-name (prompt &optional dir default-filename mustmatch initial predicate)
  "Replacement for `read-file-name'.
PROMPT, DIR, DEFAULT-FILENAME, MUSTMATCH, INITIAL and PREDICATE have the same
meaning as in `read-file-name'."
  (ignore default-filename mustmatch predicate)
  (let ((d (if dir dir default-directory)))
    (concat d (selector (list (selector-source-create
                            "Files"
                            :candidates (-map #'selector-candidate-create (directory-files d)))
                           (selector-source-create
                            "Other"
                            :candidates (list (selector-candidate-create
                                               "New file"
                                               :type 'dummy
                                               :action (lambda (_) (selector-input))))))
                     :prompt prompt
                     :initial initial))))

(defun selector-file-contents-actions (file)
  "Actions for candidate values corresponding to lines in FILE."
  (list
   (lambda (index)
     (find-file file)
     (goto-char (point-min))
     (forward-line index)
     (pulse-momentary-highlight-one-line (point)))))

(defun selector-file-contents-source (file)
  "Source for lines in FILE."
  (selector-source-create
   file
   :candidates
   (-map-indexed
    (lambda (index l)
      (selector-candidate-create l :value index))
    (split-string (f-read-text file) "\n"))
   :actions
   (selector-file-contents-actions file)))

(provide 'selector)
;;; selector.el ends here
