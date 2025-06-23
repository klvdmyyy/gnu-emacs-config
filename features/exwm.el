;;; exwm.el --- Emacs X Window Manager -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;; Code:

(use-package exwm
  :disabled t
  :hook emacs-startup)

(use-package exwm-config
  :after exwm)

(use-package exwm-randr
  :after exwm)

(use-package exwm-systemtray
  :after exwm)

;; TODO Other exwm things

;;; exwm.el ends here
