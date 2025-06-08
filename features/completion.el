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
  :bind (;; Without `C-c c' prefix
         ("C-s" . consult-line)
         ("s-B" . consult-buffer)
         ("M-g g" . consult-goto-line)
         ("C-x r b" . consult-bookmark)

         ;; With `C-c c' prefix
         ("C-c c d" . consult-flymake)
         ("C-c c i" . consult-imenu)
         ("C-c c g" . consult-git-grep)
         ("C-c c r" . consult-ripgrep)
         ("C-c c f" . project-find-file) ; Use project find file instead of `consult-find' or other consult file related things

         ;; Specific maps
         :map eshell-hist-mode-map
         ("M-r" . consult-history))
  :custom
  (consult-narrow-key "<"))

(use-package mini-frame
  :demand t)

(provide 'features/completion)

;;; completion.el ends here
