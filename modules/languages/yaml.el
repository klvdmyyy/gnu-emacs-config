;;; yaml.el --- YAML -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;; Code:

;; FIXME: See emacs/lisp/textmodes/yaml-ts-mode.el
;;
;; YAML Tree Sitter mode for GNU Emacs don't have properly
;; indentation. (yaml-ts-mode derived from text-mode !?)
;;
;; Just use external yaml-mode which provide better experience
(use-package yaml-mode
  :mode "\\.ya?ml\\'")

;;; yaml.el ends here
