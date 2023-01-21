;;; colonq-web --- web development language support -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'colonq-package)
(require 'colonq-hydra)

(use-package js2-mode :mode "\\.js\\'")

(use-package json-mode :mode "\\.json\\'")

(use-package web-mode :mode ("\\.html\\'" "\\.php\\'"))

(provide 'colonq-web)
;;; colonq-web ends here
