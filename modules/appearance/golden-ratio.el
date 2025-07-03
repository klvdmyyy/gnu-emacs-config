;;; golden-ratio.el --- Golden Ratio Appearance module -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;; Code:

(use-package golden-ratio
  :hook emacs-startup
  :config
  (with-eval-after-load 'ace-window
    (define-advice ace-window
        (:after (&rest _))
      (golden-ratio))))

;;; golden-ration.el ends here
