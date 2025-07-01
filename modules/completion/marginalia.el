;;; marginalia.el --- Marginalia Completion module -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;; Code:

(use-package marginalia
  :after vertico
  :hook vertico-mode
  :custom
  (marginalia-max-relative-age 0)
  (marginalia-align 'left))

;;; marginalia.el ends here
