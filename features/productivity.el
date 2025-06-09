;;; productivity.el
;;
;;; Commentary:
;;
;; Org related things which boost my productivity.
;;
;; MAYBE `org-trello' extension
;; TODO `org-roam' configuration
;;
;;; Code:

(use-package hl-todo
  :defer 0.5
  :config
  (global-hl-todo-mode 1))

(use-package org
  :ensure nil
  :init
  (setq org-directory "~/org")
  (defun my-org-open-file ()
    (interactive)
    (require 'consult)
    (let ((org-files (directory-files-recursively
                      org-directory "\\.org\\'")))
      (find-file (consult--read org-files))))
  
  (defun my-org-create-file (filename)
    (interactive "sFilename (without .org): ")
    (find-file (concat org-directory "/" filename ".org")))
  :hook ((org-mode . org-indent-mode))
  :bind (("C-c o t s" . org-timer-start)
         ("C-c o t e" . org-timer-stop)
         ("C-c o t p" . org-timer-pause-or-continue)
         ("C-c o t t" . org-timer-set-timer)

         ;; TEMP Dummy keybindings and functions
         ;; MAYBE Remove/change
         ("C-c o o" . my-org-open-file)
         ("C-c o c" . my-org-create-file)
         
         ;; Specific maps
         :map org-mode-map
         ("C-s" . consult-org-heading)  ; MAYBE `consult-outline'
         )
  :custom
  (org-deadline-warning-days 60)
  (org-clock-sound
   (concat user-init-dir "org-clock-sound.wav")))

(upcase (file-name-base "~/org/minds.org"))

;; TODO Setup `org-roam'
(use-package org-roam
  :disabled t)

(use-package org-agenda
  :ensure nil
  :bind (("C-c o a" . org-agenda))
  :commands (org-agenda)
  :custom
  ;; (org-agenda-files
  ;;  (directory-files-recursively
  ;;   org-directory
  ;;   ;; *.agenda.org
  ;;   "\\.agenda.org\\'"))
  (org-agenda-files
   (file-expand-wildcards
    (concat org-directory "/agenda/*.org") nil)))

(provide 'features/productivity)

;;; productivity.el ends here
