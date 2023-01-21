;;; colonq-core --- core functionality -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(add-to-list 'load-path "~/.emacs.d/colonq/core")
(require 'colonq-package)
(require 'colonq-utility)
(require 'colonq-evil)
(require 'colonq-hydra)
(require 'colonq-dispatcher)
(require 'colonq-projectile)
(require 'colonq-selector)
(require 'colonq-vc)

(provide 'colonq-core)
;;; colonq-core ends here
