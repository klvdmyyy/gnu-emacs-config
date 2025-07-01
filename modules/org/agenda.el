;;; agenda.el --- Org Agenda module -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;; Code:

(setq-default
 ;; Logging
 org-agenda-start-with-log-mode t
 org-log-done 'time
 org-log-into-drawer t

 ;; My Agenda views
 org-agenda-custom-commands
 '(("l" "Learning Agenda"
    ((agenda "" ((org-agenda-span 'day)
                 (org-agenda-remove-tags t)
                 (org-deadline-warning-days 7)
                 ;; TODO Filter by @yandexlearning tag instead of file
                 (org-agenda-files '("~/org/agenda/YandexLearning.org"))))
     (tags-todo "+@yandexlearning+PRIORITY=\"A\"+SCHEDULED<=\"<today>\""
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
              (org-agenda-span 'week)))))))

(defun update-org-agenda-files ()
  (interactive)
  (setq org-agenda-files
        (directory-files-recursively
         (expand-file-name "agenda" org-directory)
         "\\`[a-z0-9A-Z]*.org\\'")))

(define-advice org-agenda
    (:before (&rest _))
  (update-org-agenda-files))

(with-eval-after-load 'consult
  (define-advice consult-org-agenda
      (:before (&rest _))
    (update-org-agenda-files)))

(bind-key "a" 'org-agenda mode-specific-map)

;;; agenda.el ends here
