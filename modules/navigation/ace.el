;;; ace.el --- Ace things -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;; Code:

(use-package ace-window
  :bind (("M-o" . ace-window)))

;; Conflicting keybindings
(use-package ace-link
  :disabled t
  :bind (("M-o" . ace-link-addr)
         :map org-mode-map
         ("M-o" . ace-link-org)
         :map gnus-summary-mode-map
         ("M-o" . ace-link-gnus)
         :map gnus-article-mode-map
         ("M-o" . ace-link-gnus)))

;;; ace.el ends here
