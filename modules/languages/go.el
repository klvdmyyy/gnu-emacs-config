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

;;; go.el ends here
