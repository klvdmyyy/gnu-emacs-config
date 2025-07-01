;;; kaolin.el --- Kaolin Themes -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;; Code:

(use-package kaolin-themes
  :ensure t
  :hook
  ((emacs-startup
    . (lambda ()
        (when (not (daemonp))
          (load-theme 'kaolin-light t nil))))
   (after-make-frame-functions
    . (lambda (frame)
        (with-selected-frame frame
          (load-theme 'kaolin-light t nil))))))

;;; kaolin.el ends here
