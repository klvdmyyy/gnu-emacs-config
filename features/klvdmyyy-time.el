;;; time.el
;;
;;; Commentary:
;;
;;; Code:

(use-package time
  :disabled t
  :ensure nil
  :defer 0.3
  :custom
  (display-time-interval 1)
  (display-time-format "%A %d %B %R:%S %Z")
  :config
  (display-time-mode 1))

(provide 'features/klvdmyyy-time)

;;; time.el ends here
