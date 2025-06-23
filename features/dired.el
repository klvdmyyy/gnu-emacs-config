;;; dired.el --- Dired configuration -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;; Code:

(use-package dired
  :ensure nil
  :hook (dired-mode . dired-omit-mode)
  :custom
  (dired-omit-files "\\`\\'"))

;;; dired.el ends here
