;;; treemacs.el --- Treemacs configuration -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;;; Code:

(use-package treemacs)

(use-package treemacs-icons-dired
  :hook (dired-mode . treemacs-icons-dired-enable-once))

;;; treemacs.el ends here
