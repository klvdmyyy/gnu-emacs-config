;;; klv.el -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;; Code:

;; My constants

(defconst klv/assets-directory (concat user-init-dir "assets"))

(defconst klv/org-directory "~/org")

(defconst klv/org-cache-directory "~/org/cache")

(defconst klv/org-agenda-directory "~/org/agenda")

(defconst klv/org-roam-directory "~/org/roam")

(defconst klv/org-roam-subdirectories '("yandex" "programming" "business" "daily")
  "A lot of directories are hard-coded in `org-roam' configuration with
`use-package'")

(defconst klv/org-id-locations-file
  (concat klv/org-cache-directory "/.org-id-locations"))

(defconst klv/org-roam-db-location
  (concat klv/org-cache-directory "/org-roam.db"))

;; Specific constants

(defconst before-pairs-list (list ?\( ?\[ ?\{)
  "Opening pairs list constant")
(defconst after-pairs-list (list ?\) ?\] ?\})
  "Closing pairs list constant")

;; Variables

(defvar is-first-startup nil
  "`t' --- First startup of GNU Emacs
`nil' --- Not first startup of GNU Emacs")

;; Functions

(defun lock-startup (&optional lock-file)
  (let ((lock-file (or lock-file (concat user-init-dir ".startup-lock"))))
    (unless (file-exists-p lock-file)
      (dired-create-empty-file lock-file)
      (setq is-first-startup t))))

(defun get-user-asset (name)
  (concat klv/assets-directory "/" name))

;; Macroses

(defmacro load-feature (name)
  `(load ,(concat user-init-dir
                  "features/"
                  (prin1-to-string name)
                  ".el")))

(defmacro load-language (name)
  `(load ,(concat user-init-dir
                  "languages/"
                  (prin1-to-string name)
                  ".el")))

(defmacro cmd! (&rest args)
  `(lambda () (interactive) ,@args))

(provide 'klv)

;;; klv.el ends here
