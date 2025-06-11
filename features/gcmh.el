;;; gcmh.el
;;
;;; Commentary:
;;
;;; Code:

(use-package gcmh
  :demand t
  :custom
  (gcmh-cons-threshold (* 128 1024 1024))
  (gcmh-cons-percentage 0.6)
  :config
  (gcmh-mode 1))

;;; gcmh.el ends here
