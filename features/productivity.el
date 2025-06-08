;;; productivity.el
;;
;;; Commentary:
;;
;; Org related things which boost my productivity.
;;
;;; Code:

(use-package org
  :ensure nil
  :hook ((org-mode . org-indent-mode))
  :bind (("C-c o t s" . org-timer-start)
         ("C-c o t e" . org-timer-stop)
         ("C-c o t p" . org-timer-pause-or-continue)
         ("C-c o t t" . org-timer-set-timer)
         :map org-mode-map
	     ;; or `consult-outline'
	     ("C-s" . consult-org-heading))
  :custom
  (org-clock-sound
   (concat user-init-dir "org-clock-sound.wav"))
  (org-directory "~/org"))

(use-package org-agenda
  :ensure nil
  :bind (("C-c o a" . org-agenda))
  :commands (org-agenda)
  :custom
  (org-agenda-files
   (directory-files-recursively
    org-directory
    ;; *.agenda.org
    "\\.agenda.org\\'")))

(provide 'features/productivity)

;;; productivity.el ends here
