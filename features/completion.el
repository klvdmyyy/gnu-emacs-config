;;; completion.el
;;
;;; Commentary:
;;
;;; Code:

(use-package orderless
  :after consult
  :custom
  (completion-styles '(orderless basic)))

(use-package marginalia
  :after vertico
  :init
  (marginalia-mode 1)
  :custom
  (marginalia-max-relative-age 0)
  (marginalia-align 'left))

(use-package all-the-icons
  :after vertico
  :config
  (when (is-first-startup)
    (all-the-icons-install-fonts)))

(use-package all-the-icons-completion
  :after marginalia
  :hook (marginalia-mode . all-the-icons-completion-marginalia-setup)
  :config
  (all-the-icons-completion-mode 1))

(use-package consult
  :bind (("C-c a" . consult-org-agenda)
	 ("C-s" . consult-line)
	 ("C-x b" . consult-buffer)
	 ("s-B" . consult-buffer)))

(provide 'features/completion)

;;; completion.el ends here
