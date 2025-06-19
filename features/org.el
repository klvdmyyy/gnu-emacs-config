;;; org.el --- Org Mode configuration -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;;; Code:

(use-package org
  :ensure nil
  :config
  (advice-add 'org-refile :after 'org-save-all-org-buffers)
  :init
  (make-directory klv/org-directory t)
  (make-directory klv/org-cache-directory t)
  
  (setq org-directory klv/org-directory)

  :hook ((org-mode . org-indent-mode))
  :bind (("C-c o t s" . org-timer-start)
         ("C-c o t e" . org-timer-stop)
         ("C-c o t p" . org-timer-pause-or-continue)
         ("C-c o t t" . org-timer-set-timer)

         ("C-c o e" . org-babel-execute-src-block-maybe)

         ;; TEMP Dummy keybindings and functions
         ;; MAYBE Remove/change
         ("C-c o o" . my-org-open-file)
         ("C-c o c" . my-org-create-file)
         
         ;; Specific maps
         :map org-mode-map
         ("C-s" . consult-org-heading)  ; MAYBE `consult-outline'
         )
  :custom
  (org-todo-keywords
   '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)")
     (sequence "PLAN(p)" "WAIT(w@/!)" "READY(r)" "REVIEW(v)" "ACTIVE(a)" "|" "COMPLETED(c)" "CANC(k@)")))
  (org-capture-templates
   `(("w" "Work Tasks")
     ("wt" "Task" entry
      (file+headline "~/org/agenda/Work.org" "Tasks")
      "* TODO %?\nSCHEDULED: <%<%Y-%m-%d %a>>\n%a")
     ("ws" "Schedule" entry
      (file+headline "~/org/agenda/Work.org" "Tasks")
      "* TODO %?\nSCHEDULED: %^t\n%a"
      :empty-lines 1
      :prepend t)))

  ;; TODO Check it
  (org-confirm-babel-evaluate nil)      ; No confirmation for Org Babel evaluation
  (org-id-locations-file klv/org-id-locations-file)
  (org-deadline-warning-days 15)
  (org-clock-sound (get-user-asset "org-clock-sound.wav"))
  (org-refile-targets
   '(("Archive.org" :maxlevel . 1))))

;;; org.el ends here
