;;; python.el --- The best scripting language -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;; Code:

(add-to-list 'auto-mode-alist '("\\.py\\'" . python-ts-mode))

(defalias 'python-mode 'python-ts-mode
  "Python mode powered by tree-sitter.")

(defalias 'python-mode-hook 'python-ts-mode-hook
  "Hook for python powered by tree-sitter.")

;;; python.el ends here
