;;; completion.el
;;
;;; Commentary:
;;
;;; Code:

(use-package vertico
  :hook emacs-startup)

(use-package orderless
  :after vertico
  :custom
  (completion-styles '(orderless basic))
  ;; FIXME One of these variables provide vertico error on `find-file' when we write unexisten filename in completion
  ;; (completion-category-overrides
  ;;  '((project-files (styles . (orderless partial-completion basic)))
  ;;    (file (styles . (orderless partial-comletion basic)))))
  ;; (completion-category-defaults nil)
  ;; (enable-recursive-minibuffers t)
  )

(use-package marginalia
  :after vertico
  :hook vertico-mode
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

;;; completion.el ends here
