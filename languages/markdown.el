;;; markdown.el
;;
;;; Commentary:
;;
;;; Code:

(use-package markdown-mode
  :bind (:map
	     markdown-mode-map
	     ("C-s" . consult-outline))
  :mode (("\\.md\\'" . markdown-mode)))

;;; markdown.el ends here
