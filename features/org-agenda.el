;;; org-agenda.el --- Org Agenda configuration -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;;; Code:

(defconst org-agenda-capture-templates
  '(("t" "Task" entry
     (file "~/org/agenda/Tasks.org")
     "* TODO %?\nSCHEDULED: <%<%Y-%m-%d %a>>\n%i") ; MAYBE use %a !?
    ("h" "Habit" entry
     (file "~/org/agenda/Habits.org")
     "* TODO %?\n%i")
    ("s" "Sport" entry
     (file "~/org/agenda/Sport.org")
     "* TODO %?\nSCHEDULED: <%<%Y-%m-%d +1w>>\n%i")
    ("b" "Birthday" entry
     (file "~/org/agenda/Birthdays.org")
     "* %?\nSCHEDULED: <%<%Y-%m-%d +1y>>\n%i"))
  "Org Capture Templates used with `my-org-agenda-capture'")

(defun my-org-agenda-capture ()
  "TODO Documentation"
  (interactive)
  (let ((org-capture-templates org-agenda-capture-templates))
    (org-agenda-capture)))

;; NOTE Now I using Agenda only with Org Roam
(use-package org-agenda
  :ensure nil
  :bind (("C-c a" . org-agenda))
  :init
  (unless (file-exists-p "~/org/agenda")
    (make-directory "~/org/agenda" t))
  :config                               ; FIXME `org-agenda-files' doesn't setup with `:custom' property
  (setq org-agenda-files `(,klv/org-agenda-directory))
  ;; NOTE Interesting variables =)
  (setq org-agenda-start-with-log-mode t)
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)
  :custom
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
