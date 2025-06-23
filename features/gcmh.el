;;; gcmh.el --- GC -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;; Code:

(use-package gcmh
  :hook emacs-startup
  :custom
  (gcmh-cons-threshold (* 16 1024 1024))
  ;; (gcmh-cons-percentage 0.6)
  )

;;; gcmh.el ends here
