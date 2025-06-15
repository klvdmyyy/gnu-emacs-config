;;; web.el --- Web Mode configuration (HTML/CSS languages) -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;; TODO Maybe move it to `features/'
;;
;;; Code:

(use-package web-mode
  :mode "\\.html\\'"
  ;; NOTE Just toggle smartparens mode (turn off)
  :hook ((web-mode . smartparens-mode)))

;;; web.el ends here
