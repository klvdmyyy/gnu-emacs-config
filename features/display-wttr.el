;;; display-wttr.el
;;
;;; Commentary:
;;
;;; Code:

(use-package display-wttr
  :disabled t
  :defer 0.2
  :custom
  ;; (display-wttr-locations '("Buzuluk"
  ;;                           "Moscow"
  ;;                           "Sant-Petersburg"))
  (display-wttr-locations '("Buzuluk"))
  :config
  (display-wttr-mode 1))

;;; display-wttr.el
