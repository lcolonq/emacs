;;; colonq-clojure --- Clojure support -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'colonq-package)
(require 'colonq-hydra)

;; (use-package cider
;;   :config
;;   (evil-define-key 'insert cider-repl-mode-map
;;     (kbd "<up>") #'cider-repl-previous-input
;;     (kbd "<down>") #'cider-repl-next-input)
;;   (define-key cider-repl-mode-map (kbd "C-l") #'cider-repl-clear-buffer)
;;   (define-key cider-repl-mode-map (kbd "<tab>") #'company-complete)
;;   (defun colonq/clojure-repl ()
;;     "Start Clojure REPL."
;;     (interactive)
;;     (if-let ((buf (cider-current-repl)))
;;         (switch-to-buffer buf)
;;       (cider-connect `(:host "localhost" :port 7888 :project-dir ,(f-dirname buffer-file-name)))))
;;   (defhydra colonq/ide-clojure (:color teal :hint nil)
;;     "Dispatcher > Clojure IDE"
;;     ("<f12>" keyboard-escape-quit)
;;     ("S" colonq/clojure-repl "start"))
;;   (defun colonq/clojure-setup ()
;;     (setq-local colonq/contextual-ide 'colonq/ide-clojure/body))
;;   (add-hook 'clojure-mode-hook #'colonq/clojure-setup))

(provide 'colonq-clojure)
;;; colonq-clojure.el ends here
