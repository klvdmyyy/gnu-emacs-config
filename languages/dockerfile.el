;;; dockerfile.el --- Dockerfile support for GNU Emacs -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;; Code:

(use-package dockerfile-ts-mode
  :ensure nil
  :after treesit
  :mode "Dockerfile\\'")

;;; dockerfile.el ends here
