;;; org-agenda.el --- Org Agenda configuration -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;;; Code:

(use-package org-agenda
  :ensure nil
  :bind (("C-c a" . org-agenda))
  :custom
  (org-agenda-files
   (append (directory-files-recursively "~/org/agenda/" "\\`[a-z0-9A-Z]*.org\\'")
           ;; (list (concat user-emacs-directory "ISSUES.org"))
           '()))
  (org-agenda-start-with-log-mode t)
  (org-log-done 'time)
  (org-log-into-drawer t)
  (org-agenda-custom-commands
   '(("l" "Learning Agenda"
      ((agenda "" ((org-agenda-span 'day)
                   (org-agenda-remove-tags t)
                   (org-deadline-warning-days 7)
                   ;; TODO Filter by @yandexlearning tag instead of file
                   (org-agenda-files '("~/org/agenda/YandexLearning.org"))))
       (tags-todo "+@yandexlearning+PRIORITY=\"A\""
                  ((org-agenda-span 'day)
                   (org-agenda-remove-tags t)
                   (org-agenda-overriding-header "High Priority Tasks")))))
     ("w" "Weekly Review"
      ((agenda ""
               ((org-agenda-overriding-header "Completed Task")
                (org-agenda-skip-function '(org-agenda-skip-entry-if 'nottodo 'done))
                (org-agenda-span 'week)))
       (agenda ""
               ((org-agenda-overriding-header "Unfinished Scheduled Tasks")
                (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                (org-agenda-span 'week))))))))

;;; org-agenda.el ends here
