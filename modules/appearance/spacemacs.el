;;; spacemacs.el --- Spacemacs Appearance module -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;; Code:

(use-package spacemacs-theme
  :demand (not (daemonp))
  :hook
  ((emacs-startup
    . (lambda ()
        (when (not (daemonp))
          (load-theme 'spacemacs-dark t nil))))
   (after-make-frame-functions
    . (lambda (frame)
        (with-selected-frame frame
          (load-theme 'spacemacs-dark t nil))))))

;;; spacemacs.el ends here
