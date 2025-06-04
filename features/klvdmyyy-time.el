;;; time.el
;;
;;; Commentary:
;;
;;; Code:

(use-package time
  :ensure nil
  :defer 0.3
  :custom
  (display-time-format "%A %d %B %R %Z")
  :config
  (display-time-mode 1))

(provide 'features/klvdmyyy-time)

;;; time.el ends here
