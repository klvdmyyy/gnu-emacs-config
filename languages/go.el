;;; go.el --- Golang for Emacs -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;; Code:

(use-package go-ts-mode
  :ensure nil
  :after treesit
  :mode "\\.go\\'"
  :init
  (when (not (treesit-language-available-p 'go))
    (treesit-install-language-grammar 'go))
  (when (not (treesit-language-available-p 'gomod))
    (treesit-install-language-grammar 'gomod))
  :custom
  (go-ts-mode-indent-offset 4))

(use-package ob-go
  :after (:any org-mode org-roam)
  :demand t)

;;; go.el ends here
