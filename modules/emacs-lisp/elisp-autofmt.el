;;; elisp-autofmt.el --- Emacs Lisp Auto Formatting -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;; Code:

;; FIXME: Wrong formatting with use-package
;; FIXME: Wrong indentation
(use-package elisp-autofmt
  :disabled t
  :hook (emacs-lisp-mode . elisp-autofmt-mode))

;;; elisp-autofmt.el ends here
