;;; consult.el --- Consult Completion module -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;; Code:

;; TODO: More convinient consult keybindings
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

;;; consult.el ends here
