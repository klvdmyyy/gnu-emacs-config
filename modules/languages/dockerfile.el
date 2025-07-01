;;; dockerfile.el --- Dockerfile Language module -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;; Code:

(add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-ts-mode))

(defalias 'dockerfile-mode 'dockerfile-ts-mode
  "Dockerfile mode powered by tree-sitter.")

(defalias 'dockerfile-mode-hook 'dockerfile-ts-mode-hook
  "Hook for dockerfile powered by tree-sitter.")

;;; dockerfile.el ends here
