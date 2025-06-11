;;; go.el
;;
;;; Commentary:
;;
;;; Code:

(use-package go-mode
  :mode ("\\.go\\'" . go-mode))

(use-package ob-go
  :after (:any org-mode org-roam)
  :demand t)

;;; go.el ends here
