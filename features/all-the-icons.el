;;; all-the-icons.el
;;
;;; Commentary:
;;
;;; Code:

(use-package all-the-icons
  :demand t
  :config
  (when (is-first-startup)
    (all-the-icons-install-fonts)))

(use-package all-the-icons-completion
  :after all-the-icons
  :demand t
  :hook (marginalia-mode . all-the-icons-completion-marginalia-setup)
  :config
  (all-the-icons-completion-mode 1))

(provide 'features/all-the-icons)

;;; all-the-icons.el ends here
