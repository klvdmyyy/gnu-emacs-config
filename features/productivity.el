;;; productivity.el
;;
;; The core of my GNU Emacs configuration.
;;
;; I trying to learn Org, Org Agenda and Org Roam for using it everywhere.
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
;; MAYBE configure `denote'
;; MAYBE configure `noter'
;; TODO configure `calibredb' for calibre e-books library
;; TODO configure `nov.el' for epub reading
;;
;;; Code:

(use-package hl-todo
  :defer 0.5
  :config
  (global-hl-todo-mode 1))

(use-package org
  :ensure nil
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
  (org-confirm-babel-evaluate nil)
  (org-id-locations-file klv/org-id-locations-file)
  (org-deadline-warning-days 60)
  (org-clock-sound (get-user-asset "org-clock-sound.wav")))

(use-package org-roam
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
         ("n t" . org-roam-dailies-capture-today)
         ("n n" . org-roam-buffer-toggle)
         ("n f" . org-roam-node-find)
         ("n i" . org-roam-node-insert)
         ("n r" . org-roam-ref-find)
         ("n C" . org-roam-capture))
  :custom
  (org-roam-node-display-template
   (concat "${title:*} "
           (propertize "${tags:30}" 'face 'org-tag)))
  (org-roam-dailies-directory "daily/")
  (org-roam-dailies-capture-templates
   '(("d" "default" entry
      "* %?"
      :target (file+head
               "%<%Y-%m-%d>.org"
               "#+TITLE: %<%Y-%m-%d>\n#+filetags: :daily:dailies: %<%Y-%m-%d>\n\n"))))
  (org-roam-capture-templates
   '(("y" "Yandex" plain
      "%?"
      :target (file+head "yandex/${slug}.org"
                         "#+TITLE: ${title}\n#+filetags: :yandex:\n\n")
      :unarrowed t)
     ("a" "Yandex Algorithms" plain
      "#+begin_src go :imports '(\"fmt\")%?\n#+end_src\n"
      :target (file+head "yandex/algorithms/${slug}.org"
                         "#+TITLE: ${title}\n#+filetags: :algorithms:yandex:\n\n")
      :unarrowed t)
     ("p" "Programming" plain
      "* %?"
      :target (file+head "programming/${slug}.org"
                         "#+TITLE: ${title}\n#+filetags: :programming:\n\n")
      :unarrowed t)
     ("b" "Business" plain
      "** Description\n%?\n** Advantages\n**Risks\n"
      :target (file+head "business/${slug}.org"
                         "#+TITLE: ${title}\n#+filetags: :business:\n\n")
      :unarrowed t)))
  (org-roam-db-location klv/org-roam-db-location)
  (org-roam-db-update-on-save t)
  :init
  (make-directory klv/org-roam-directory t)
  (dolist (dir klv/org-roam-subdirectories)
    (make-directory (concat klv/org-roam-directory "/" dir) t))
  :config
  (setq org-roam-directory klv/org-roam-directory)
  (org-roam-db-autosync-mode))

(use-package org-agenda
  :ensure nil
  :commands (org-agenda)
  :bind (("C-c a" . org-agenda))
  :init
  (unless (file-exists-p "~/org/agenda")
    (make-directory "~/org/agenda" t))
  :config
  (with-eval-after-load 'org-roam
    (add-to-list 'org-agenda-files klv/org-roam-directory)
    (dolist (dir klv/org-roam-subdirectories)
      (add-to-list 'org-agenda-files (concat klv/org-roam-directory "/" dir))))
  :custom
  ;; (org-agenda-files
  ;;  (directory-files-recursively
  ;;   org-directory
  ;;   ;; *.agenda.org
  ;;   "\\.agenda.org\\'"))
  
  ;; (org-agenda-files
  ;;  (file-expand-wildcards
  ;;   (concat org-directory "/agenda/*.org") nil))
  (org-agenda-files
   (directory-files-recursively klv/org-agenda-directory "\\.org\\'")))

;;; productivity.el ends here
