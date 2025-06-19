;;; org-agenda.el --- Org Agenda configuration -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;;; Code:

(use-package org-agenda
  :ensure nil
  :bind (("C-c a" . org-agenda))
  :custom
  (org-agenda-files (directory-files-recursively "~/org/agenda/" "\\.org\\'"))
  (org-agenda-start-with-log-mode t)
  (org-log-done 'time)
  (org-log-into-drawer t)
  (org-agenda-custom-commands
   '(("w" "Programming/Work/Learning"
      ((tags-todo "+@learning|+@programming|+@work/TODO"
                  ((org-agenda-overriding-header "Нужно сделать")
                   (org-agenda-files org-agenda-files)
                   (org-agenda-span 'day)))
       (tags-todo "+@learning|+@programming|+@work/NEXT"
                  ((org-agenda-overriding-header "Следующая задача (после текущей, или позже)")
                   (org-agenda-files org-agenda-files)
                   (org-agenda-span 'day)))
       (tags-todo "+@learning|+@programming|+@work/WAIT"
                  ((org-agenda-overriding-header "Ожидание внешних изменений")
                   (org-agenda-files org-agenda-files)
                   (org-agenda-span 'day)))
       (tags-todo "+@learning|+@programming|+@work/HOLD"
                  ((org-agenda-overriding-header "Ожидание внутренних изменений")
                   (org-agenda-files org-agenda-files)
                   (org-agenda-span 'day)))
       (tags-todo "+@learning|+@programming|+@work/READY"
                  ((org-agenda-overriding-header "Готовая задача")
                   (org-agenda-files org-agenda-files)
                   (org-agenda-span 'day)))
       (tags-todo "+@learning|+@programming|+@work/REVIEW"
                  ((org-agenda-overriding-header "В ожидании ревью")
                   (org-agenda-files org-agenda-files)
                   (org-agenda-span 'day)))
       (tags-todo "+@learning|+@programming|+@work/STOP"
                  ((org-agenda-overriding-header "Остановленные задачи")
                   (org-agenda-files org-agenda-files)
                   (org-agenda-span 'day)))
       (tags-todo "+@learning|+@programming|+@work/NOTE"
                  ((org-agenda-overriding-header "Заметки ?!")
                   (org-agenda-files org-agenda-files)
                   (org-agenda-span 'day)))
       (tags-todo "+@learning|+@programming|+@work/IDEA"
                  ((org-agenda-overriding-header "Идеи")
                   (org-agenda-files org-agenda-files)
                   (org-agenda-span 'day)))
       (tags-todo "+@learning|+@programming|+@work/DONE"
                  ((org-agenda-overriding-header "Сделано")
                   (org-agenda-files org-agenda-files)
                   (org-agenda-span 'day))))))))

;;; org-agenda.el ends here
