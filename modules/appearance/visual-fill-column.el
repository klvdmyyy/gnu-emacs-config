;;; visual-fill-column.el --- Visual Fill Column Appearance module -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;; Code:

(use-package visual-fill-column
  :hook (org-mode
         markdown-mode
         Info-mode
         eww-mode
         elfeed-show-mode)
  :custom
  (visual-fill-column-enable-sensible-window-split t) ; Split windows vertically
  (visual-fill-column-width 120))

;;; visual-fill-column.el ends here
