;;; go.el --- Golang for Emacs -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;; Code:

(use-package go-ts-mode
  :ensure nil
  :after treesit
  :mode "\\.go\\'"
  :custom
  (go-ts-mode-indent-offset 4))

(use-package go-mod-ts-mode
  :ensure nil
  :after treesit
  :mode "go.mod\\'")

(use-package ob-go
  :after (:any org-mode org-roam)
  :demand t)

;;; go.el ends here
