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

(use-package org-agenda
  :ensure nil
  :commands (org-agenda)
  :custom
  (org-directory "~/org")
  (org-agenda-files (directory-files-recursively org-directory "\\.agenda.org\\'")))

(provide 'features/productivity)

;;; productivity.el ends here
