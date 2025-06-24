;;; elisp.el --- Emacs Lisp development -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;; Code:

(use-package elisp-mode
  :ensure nil
  ;; FIXME c-ts-mode by default !?!?!?!?
  :mode ("\\.\\(el\\|elc\\)" . emacs-lisp-mode))

;;; elisp.el ends here
