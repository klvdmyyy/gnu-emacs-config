;;; productivity.el
;;
;;; Commentary:
;;
;;; Code:

(use-package org
  :ensure nil
  :hook ((org-mode . org-indent-mode))
  :bind (:map
	 org-mode-map
	 ;; or `consult-outline'
	 ("C-s" . consult-org-heading)))

(provide 'features/productivity)

;;; productivity.el ends here
