;;; all-the-icons.el --- Icons -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;; Code:

(use-package all-the-icons
  :demand t
  :config
  ;; (when is-first-startup
  ;;   (all-the-icons-install-fonts))
  )

(use-package all-the-icons-completion
  :hook ((emacs-startup . all-the-icons-completion-mode)
         (marginalia-mode . all-the-icons-completion-marginalia-setup)))

;;; all-the-icons.el ends here
