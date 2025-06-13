;;; time.el
;;
;;; Commentary:
;;
;;; Code:

(use-package time
  :ensure nil
  :after doom-modeline
  :hook (doom-modeline-mode . display-time-mode)
  :custom
  (display-time-interval 1)
  ;; (display-time-format "%A %Y-%m-%d %R:%S %Z")
  (display-time-format "%a %d %R:%S"))

;;; time.el ends here
