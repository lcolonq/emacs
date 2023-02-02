;;; colonq-lang --- language support -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(add-to-list 'load-path "~/.emacs.d/colonq/lang")

(require 'colonq-asm)
(require 'colonq-c)
(require 'colonq-elisp)
(require 'colonq-fennel)
(require 'colonq-nix)
(require 'colonq-python)
(require 'colonq-rust)
(require 'colonq-web)
(require 'colonq-xml)
(require 'colonq-yaml)

(provide 'colonq-lang)
;;; colonq-lang.el ends here
