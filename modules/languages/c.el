;;; c.el --- C Language module -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;; Code:

(add-to-list 'auto-mode-alist '("\\.\\(c\\|h\\)\\'" . c-ts-mode))

(defalias 'c-mode 'c-ts-mode
  "C mode powered by tree-sitter.")

(defalias 'c-mode-hook 'c-ts-mode-hook
  "Hook for C language powered by tree-sitter.")

;;; c.el ends here
