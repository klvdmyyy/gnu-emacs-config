;;; kubernetes.el --- Kubernetes Integration module -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;; Code:

(use-package kubernetes
  :commands (kubernetes-overview)
  :custom
  (kubernetes-poll-frequency 3600)
  (kubernetes-redraw-frequency 3600))

;; Other packages associated with Kubernetes that I can use:

(use-package kubel
  :disabled t)

(use-package kele
  :disabled t)

;;; kubernetes.el ends here
