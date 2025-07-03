;;; google-translate.el --- Google Translate API module -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;; Code:

(use-package google-translate
  :bind (("C-c t" . google-translate-smooth-translate))
  :config
  :custom
  (google-translate-translation-directions-alist
   '(("ru" . "en")
     ("en" . "ru"))))

;;; google-translate.el ends here
