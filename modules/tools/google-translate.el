;;; google-translate.el --- Google Translate API module -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;; Code:

(use-package google-translate
  :bind (("C-c t" . google-translate-smooth-translate))
  :init
  (require 'google-translate-smooth-ui)
  (setq-default google-translate-translation-directions-alist
                '(("ru" . "en")
                  ("en" . "ru"))))

;;; google-translate.el ends here
