;;; colonq-music --- music player -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'colonq-package)
(require 'colonq-hydra)

(use-package emms)

(use-package emms-playlist-mode :after emms)

(use-package emms-history :after emms
  :config
  (emms-history-load))

(use-package emms-source-file :after emms
  :config
  (setq emms-source-file-default-directory (expand-file-name "~/music")
        emms-source-file-directory-tree-function 'emms-source-file-directory-tree-find))

(use-package emms-player-mpv :after emms
  :config
  (setq emms-player-list '(emms-player-mpv))
  (add-to-list 'emms-player-mpv-parameters "--no-audio-display"))

(use-package emms-info :after emms)

(use-package emms-info-libtag :after (emms emms-info)
  :config
  (setq emms-info-functions '(emms-info-libtag)))

(use-package emms-cache :after emms
  :config
  (emms-cache 1)
  (defun colonq/music ()
    "Select and play music using EMMS."
    (interactive)
    (selector (list (selector-source-create
                  "Tracks"
                  :candidates
                  (mapcar (lambda (v)
                            (selector-candidate-create (concat (or (assoc-default 'info-artist v) "unknown")
                                                            " - "
                                                            (or (assoc-default 'info-album v) "unknown")
                                                            " - "
                                                            (or (assoc-default 'info-title v) "unknown")
                                                            (when (assoc-default 'info-composer v)
                                                                (concat " (" (assoc-default 'info-composer v) ")")
                                                              ""))
                                                    :value (assoc-default 'name v)))
                          (remove-if (lambda (v) (string-match "^\\(http\\|mms\\):" (assoc-default 'name v)))
                                     (hash-table-values emms-cache-db)))
                  :actions
                  '(emms-play-file))
                 (selector-source-create
                  "Directories"
                  :candidates
                  (colonq/walk-directory
                   emms-source-file-default-directory
                   :directories 'only
                   :path 'full)
                  :actions
                  '(emms-play-directory))))))

(use-package soundboard
  :load-path "~/src/soundboard")

(defhydra colonq/music-dispatcher (:color teal :hint nil :body-pre (setq exwm-input-line-mode-passthrough t) :post (setq exwm-input-line-mode-passthrough nil))
  "Dispatcher > Music"
  ("<f12>" keyboard-escape-quit)
  ("s" soundboard/hydra/body "soundboard")
  ("SPC" emms-pause "pause")
  ("<" emms-previous "prev")
  (">" emms-next "next")
  ("m" colonq/music "search")
  ("q" emms-stop "stop")
  ("p" emms "playlist")
  ("c" emms-playlist-current-clear "clear")
  ("+" emms-volume-raise "louder" :color red)
  ("=" emms-volume-raise :color red)
  ("-" emms-volume-lower "quieter" :color red)
  ("_" emms-volume-lower :color red))

(provide 'colonq-music)
;;; colonq-music.el ends here
