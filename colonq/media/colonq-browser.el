;;; colonq-browser --- web browser -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'colonq-package)
(require 'colonq-hydra)

(defvar colonq/bookmarks
  '(("twitch" . "https://twitch.tv")
    ("twitch chat" . "https://www.twitch.tv/popout/lcolonq/chat?popout=")
    ("twitter" . "https://twitter.com")
    ("youtube" . "https://youtube.com")
    ("reddit /r/lcolonq" . "https://old.reddit.com/r/lcolonq")
    ("fugi reactive" . "https://discord-reactive-images.fugi.tech")
    ("orange website hn hacker news hackernews" . "https://news.ycombinator.com")
    ("learnxinyminutes learn x in y" . "https://learnxinyminutes.com")
    ("pubnix" . "https://pub.colonq.computer")
    ("pubnix ring" . "https://pub.colonq.computer/~llll")
    ("bells docs" . "https://pub.colonq.computer/~bezelea/bells")
    ("bells songs dbzkai" . "https://pub.colonq.computer/~prod/toy/dbkai")
    ("AFTER DARK PUSH" . "https://vdo.ninja?push=lcolonqafterdark")
    ("oub" . "https://oub.colonq.computer")
    ("nixos package search" . "https://search.nixos.org/packages")
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

(defun colonq/search-ddg (&optional query)
  "Search for QUERY using DuckDuckGo."
  (interactive)
  (let ((query (if (called-interactively-p 'any)
                   (read-string "Query: " query)
                 query)))
    (browse-url (concat "https://www.duckduckgo.com/?q=" query))))

(defun colonq/search-wikipedia (&optional query)
  "Search for QUERY on Wikipedia."
  (interactive)
  (let ((query (if (called-interactively-p 'any)
                   (read-string "Query: " query)
                 query)))
    (browse-url (concat "https://en.wikipedia.org/w/index.php?search=" query))))

(defun colonq/search-bulbapedia (&optional query)
  "Search for QUERY on Bulbapedia."
  (interactive)
  (let ((query (if (called-interactively-p 'any)
                   (read-string "Query: " query)
                 query)))
    (browse-url (concat "https://bulbapedia.bulbagarden.net/w/index.php?search=" query))))

(defun colonq/search-yugipedia (&optional query)
  "Search for QUERY on Yugipedia."
  (interactive)
  (let ((query (if (called-interactively-p 'any)
                   (read-string "Query: " query)
                 query)))
    (browse-url (concat "https://yugipedia.com/index.php?search=" query))))

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
          "Search DuckDuckGo"
          :type 'dummy
          :action (lambda (_) (colonq/search-ddg (selector-input))))
         (selector-candidate-create
          "Search Wikipedia"
          :type 'dummy
          :action (lambda (_) (colonq/search-wikipedia (selector-input))))
         (selector-candidate-create
          "Search Bulbapedia"
          :type 'dummy
          :action (lambda (_) (colonq/search-bulbapedia (selector-input))))
         (selector-candidate-create
          "Search Yugipedia"
          :type 'dummy
          :action (lambda (_) (colonq/search-yugipedia (selector-input))))
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
  ("<f12>" keyboard-escape-quit)
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
