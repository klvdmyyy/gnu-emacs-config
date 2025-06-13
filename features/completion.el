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

;; TODO More convinient consult keybindings
(use-package consult
  :bind (;; Without `C-c c' prefix
         ("C-s" . consult-line)
         ("s-B" . consult-buffer)
         ("M-g g" . consult-goto-line)
         ("C-x r b" . consult-bookmark)

         ;; With `C-c c' prefix
         :map mode-specific-map
         ("c d" . consult-flymake)
         ("c i" . consult-imenu)
         ("c g" . consult-git-grep)
         ("c r" . consult-ripgrep))
  :custom
  (consult-narrow-key "<"))

;; TODO Setup it maybe
(use-package consult-org-roam
  :disabled t
  :after org-roam
  :demand t)

;;; completion.el ends here
