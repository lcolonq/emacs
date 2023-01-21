;;; colonq-browser --- web browser -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'colonq-package)
(require 'colonq-hydra)

(defvar colonq/bookmarks
  '(("twitch" . "https://twitch.tv")
     ))

(use-package shr
  :defer t
  :config
  (setq shr-use-fonts nil)
  (setf (cdr shr-map) nil))

(defun colonq/firefox (url &rest args)
  "Launch Firefox with URL ignoring ARGS."
  (interactive)
  (ignore args)
  (start-process "firefox" nil "firefox" "--new-window" url))

(setq browse-url-browser-function #'colonq/firefox)

(defun colonq/browse-url ()
  "Browse URL wrapper fixing prompt behavior."
  (interactive)
  (let ((query (read-string "URL: " "https://")))
    (browse-url query)))

(defun colonq/search-google (&optional query)
  "Search for QUERY using Google."
  (interactive)
  (let ((query (if (called-interactively-p 'any)
                   (read-string "Query: " query)
                 query)))
    (browse-url (concat "https://www.google.com/search?q=" query))))

(defun colonq/search-wikipedia (&optional query)
  "Search for QUERY on Wikipedia."
  (interactive)
  (let ((query (if (called-interactively-p 'any)
                   (read-string "Query: " query)
                 query)))
    (browse-url (concat "https://en.wikipedia.org/w/index.php?search=" query))))

(defun colonq/search-scryfall (&optional query)
  "Search for QUERY on Scryfall."
  (interactive)
  (let ((query (if (called-interactively-p 'any)
                   (read-string "Query: " query)
                 query)))
    (browse-url (concat "https://scryfall.com/search?q=" query))))

(defun colonq/selector-bookmarks ()
  "Selector source for `colonq/bookmarks'."
  (selector-source-create
   "Bookmarks"
   :candidates
   (-map
    (lambda (b) (selector-candidate-create (car b) :value (cdr b)))
    colonq/bookmarks)
   :actions
   '(browse-url)))

(defun colonq/selector-search ()
  "Selector source for web search."
  (selector-source-create
   "Browser"
   :candidates
   (list (selector-candidate-create
          "Search Google"
          :type 'dummy
          :action (lambda (_) (colonq/search-google (selector-input))))
         (selector-candidate-create
          "Search Wikipedia"
          :type 'dummy
          :action (lambda (_) (colonq/search-wikipedia (selector-input))))
         (selector-candidate-create
          "Search Scryfall"
          :type 'dummy
          :action (lambda (_) (colonq/search-scryfall (selector-input))))
         (selector-candidate-create
          "Browse URL"
          :type 'dummy
          :action (lambda (_) (browse-url (selector-input)))))))

(defun colonq/visit-bookmark ()
  "Select and `browse-url' a bookmark."
  (interactive)
  (unwind-protect
      (selector
       (list
        (colonq/selector-bookmarks)
        (colonq/selector-search)))))

(defhydra colonq/browser-dispatcher (:color teal :hint nil :body-pre (setq exwm-input-line-mode-passthrough t) :post (setq exwm-input-line-mode-passthrough nil))
  "Dispatcher > Browser"
  ("<escape>" keyboard-escape-quit)
  ("b" colonq/visit-bookmark "bookmarks")
  ("u" colonq/browse-url "url")
  ("s" colonq/search-web "search"))

(use-package image-mode
  :config
  (setq image-mode-map (make-keymap)
        image-animate-loop t)
  (evil-define-key 'motion image-mode-map (kbd "h") 'image-backward-hscroll)
  (evil-define-key 'motion image-mode-map (kbd "l") 'image-forward-hscroll)
  (evil-define-key 'motion image-mode-map (kbd "k") 'image-previous-line)
  (evil-define-key 'motion image-mode-map (kbd "j") 'image-next-line)
  (evil-define-key 'motion image-mode-map (kbd "SPC") 'image-toggle-animation))

(provide 'colonq-browser)
;;; colonq-browser ends here
