;;; org.el --- Org Mode configuration -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;;; Code:

(use-package org
  :ensure nil
  :hook ((org-mode . org-indent-mode))
  :bind (("C-c o t s" . org-timer-start)
         ("C-c o t e" . org-timer-stop)
         ("C-c o t p" . org-timer-pause-or-continue)
         ("C-c o t t" . org-timer-set-timer)

         ("C-c o e" . org-babel-execute-src-block-maybe)

         ("C-c o c" . org-capture)
         
         ;; Specific maps
         :map org-mode-map
         ("C-s" . consult-org-heading)  ; MAYBE `consult-outline'
         )
  :custom
  (org-directory "~/org")
  (org-capture-templates
   `(("e" "Emacs")
     ("ei" "Emacs Issue" entry
      (file ,(concat user-init-dir "ISSUES.org"))
      "* %?\n%a"
      :empty-lines 1)
     ("w" "Work Tasks")
     ("wt" "Task" entry
      (file+headline "~/org/agenda/Work.org" "Tasks")
      "* TODO %?\nSCHEDULED: <%<%Y-%m-%d %a>>\n%a"
      :empty-lines 1
      :prepend t)
     ("ws" "Schedule" entry
      (file+headline "~/org/agenda/Work.org" "Tasks")
      "* TODO %?\nSCHEDULED: %^t\n%a"
      :empty-lines 1
      :prepend t)))

  ;; TODO Check it
  (org-confirm-babel-evaluate nil)      ; No confirmation for Org Babel evaluation
  (org-id-locations-file "~/org/cache/.org-id-locations")
  (org-deadline-warning-days 7)
  (org-clock-sound (concat user-init-dir "assets/org-clock-sound.wav")))

;;; org.el ends here
