;;; completion.el
;;
;;; Commentary:
;;
;;; Code:

(use-package vertico
  :demand t
  :hook (after-init . vertico-mode)
  :config
  (vertico-mode 1))

(use-package orderless
  :after vertico
  :demand t
  :custom
  (completion-styles '(orderless basic)))

(use-package marginalia
  :after vertico
  :demand t
  :config
  (marginalia-mode 1)
  :custom
  (marginalia-max-relative-age 0)
  (marginalia-align 'left))

(use-package consult
  :bind (;; Without `C-c c' prefix
         ("C-s" . consult-line)
         ("s-B" . consult-buffer)
         ("M-g g" . consult-goto-line)
         ("C-x r b" . consult-bookmark)

         ;; With `C-c c' prefix
         ("C-c c a" . consult-org-agenda)
         ("C-c c d" . consult-flymake)
         ("C-c c i" . consult-imenu)
         ("C-c c g" . consult-git-grep)
         ("C-c c r" . consult-ripgrep)
         ("C-c c f" . project-find-file) ; Use project find file instead of `consult-find' or other consult file related things
         )
  :custom
  (consult-narrow-key "<"))

;; TODO Setup it maybe
(use-package consult-org-roam
  :disabled t
  :after org-roam
  :demand t)

;;; completion.el ends here
