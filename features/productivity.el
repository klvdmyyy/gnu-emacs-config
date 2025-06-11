;;; productivity.el
;;
;;; Commentary:
;;
;; Org related things which boost my productivity.
;;
;; Org directory structure:
;; ~/org - Main org mode directory
;; ~/org/agenda - Agenda directory
;; ~/org/roam - Roam directory
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
  (unless (file-exists-p "~/org")
    (make-directory "~/org"))
  (unless (file-exists-p "~/org/cache")
    (make-directory "~/org/cache"))'
  
  (setq org-directory "~/org")

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
  (org-id-locations-file (concat org-directory "/cache/.org-id-locations"))
  (org-deadline-warning-days 60)
  (org-clock-sound
   (concat user-init-dir "org-clock-sound.wav")))

;; TODO Setup `org-roam'
(use-package org-roam
  ;; FIXME Why `:demand' is `t' ?!?!
  :demand t
  :bind (:map
         org-mode-map
         ("C-c r r" . org-roam-ref-add)
         ("C-c r R" . org-roam-ref-remove)
         ("C-c r f" . org-roam-ref-find)
         ("C-c r t" . org-roam-tag-add)
         ("C-c r T" . org-roam-tag-remove)
         ("C-c r a" . org-roam-alias-add)
         ("C-c r A" . org-roam-alias-remove)
         ;; TODO See https://git.sr.ht/~abcdw/rde/tree/master/item/src/rde/features/emacs-xyz.scm#L4883
         ;; ("C-c r O" . rde-org-roam-open-ref)
         :map
         mode-specific-map
         ("n n" . org-roam-buffer-toggle)
         ("n f" . org-roam-node-find)
         ("n i" . org-roam-node-insert)
         ("n r" . org-roam-ref-find)
         ("n C" . org-roam-capture))
  :custom
  (org-roam-db-location (concat org-directory "/cache/org-roam.db"))
  (org-roam-db-update-on-save t)
  :init
  (unless (file-exists-p "~/org/roam")
    (make-directory "~/org/roam"))
  :config
  (setq org-roam-directory (file-truename "~/org/roam"))
  (add-to-list 'org-agenda-files org-roam-directory)
  (org-roam-db-autosync-mode))

(use-package org-agenda
  :ensure nil
  :commands (org-agenda)
  :bind (("C-c a" . org-agenda))
  :init
  (unless (file-exists-p "~/org/agenda")
    (make-directory "~/org/agenda"))
  :custom
  ;; (org-agenda-files
  ;;  (directory-files-recursively
  ;;   org-directory
  ;;   ;; *.agenda.org
  ;;   "\\.agenda.org\\'"))
  (org-agenda-files
   (file-expand-wildcards
    (concat org-directory "/agenda/*.org") nil)))

;;; productivity.el ends here
