;;; orderless.el --- Orderless Completion module -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;;; Code:

(use-package orderless
  :after (:any vertico)
  :custom
  (completion-styles '(orderless basic)))

;;; orderless.el ends here
