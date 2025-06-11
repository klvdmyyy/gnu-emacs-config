;;; time.el
;;
;;; Commentary:
;;
;;; Code:

(use-package time
  :ensure nil
  :defer 0.3
  :custom
  (display-time-interval 1)
  (display-time-format "%A %Y-%m-%d %R:%S %Z")
  :config
  (display-time-mode 1))

;;; time.el ends here
