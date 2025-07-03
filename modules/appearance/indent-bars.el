;;; indent-bars.el --- Indent Bars Appearance module -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;; Code:

(use-package indent-bars
  :hook (python-mode
         python-ts-mode
         c-mode
         c-ts-mode
         c++-mode
         c++-ts-mode
         go-mode
         go-ts-mode))

;;; indent-bars.el ends here
