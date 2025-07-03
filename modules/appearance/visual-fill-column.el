;;; visual-fill-column.el --- Visual Fill Column Appearance module -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;; Code:

(use-package visual-fill-column
  :hook (prog-mode
         text-mode)
  :custom
  (visual-fill-column-center-text nil)
  (visual-fill-column-enable-sensible-window-split t) ; Split windows vertically
  (visual-fill-column-width 140))

;;; visual-fill-column.el ends here
