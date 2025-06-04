;;; vertico.el
;;
;;; Commentary:
;;
;;; Code:

(use-package vertico
  :demand t
  :hook (after-init . vertico-mode)
  :config
  (vertico-mode 1))

(provide 'features/vertico)

;;; vertico.el ends here
