;;; go.el --- Go Language module -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;; Code:

(setq-default go-ts-mode-indent-offset 4)

(add-to-list 'auto-mode-alist '("\\.go\\'" . go-ts-mode))
(add-to-list 'auto-mode-alist '("go.mod\\'" . go-mod-ts-mode))

(use-package ob-go
  :after (:any org org-babel org-roam)
  :demand t)

(defalias 'go-mode 'go-ts-mode
  "Go language mode powered by tree-sitter.")

(defalias 'go-mode-hook 'go-ts-mode-hook
  "Hook for golang powered by tree-sitter.")

;;; go.el ends here
