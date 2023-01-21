;;; colonq-video --- video player -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'colonq-package)
(require 'colonq-hydra)

(defvar colonq/video-file-extensions '("mkv" "mp4" "webm" "mp3" "wav" "m4a"))

(defun colonq/open-video (path)
  "Open the file at PATH with MPV."
  (let* ((full (expand-file-name path))
         (remote (file-remote-p full))
         (local (file-local-name full))
         (prefix (if (and remote (string-match "/\\(\\w*\\):\\(\\w*\\):" remote))
                     (if (string= (match-string 1 remote) "ssh")
                         (concat "sftp://" (match-string 2 remote) ":")
                       "")
                   ""))
         (final (if (string-match (rx "http" (opt "s") "://") path)
                    path
                    (concat prefix local))))
    (message (concat "Opening video at " final))
    (start-process (concat "mpv (" final ")") nil "mpv" "--force-window=yes" final)))

(defun colonq/video-find-file-wrapper (f &rest args)
  "Wrapper around F (find-file), passing ARGS."
  (if (member (file-name-extension (car args)) colonq/video-file-extensions)
      (progn
        (colonq/open-video (car args))
        (recentf-add-file (car args))
        nil)
    (apply f args)))
(advice-add 'find-file :around 'colonq/video-find-file-wrapper)

(provide 'colonq-video)
;;; colonq-video ends here
