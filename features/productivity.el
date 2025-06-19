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
;; TODO Separate `org', `org-agenda' and `org-roam' (also `hl-todo')
;;
;;; Code:

(use-package hl-todo
  :defer 0.3
  :config
  (global-hl-todo-mode 1))

;;; productivity.el ends here
