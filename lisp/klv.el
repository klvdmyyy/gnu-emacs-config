;;; klv.el -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;; Code:

;; My constants

(defconst klv/assets-directory (concat user-init-dir "assets"))

(defconst klv/snippets
  (seq-map (apply-partially 'concat user-init-dir)
           '("snippets")))

(defconst klv/org-directory "~/org")

(defconst klv/org-cache-directory "~/org/cache")

(defconst klv/org-agenda-directory "~/org/agenda")

(defconst klv/org-roam-directory "~/org/roam")

(defconst klv/org-roam-subdirectories '("yandex" "yandex/algorithms" "programming" "business" "daily")
  "A lot of directories are hard-coded in `org-roam' configuration with
`use-package'")

(defconst klv/org-id-locations-file
  (concat klv/org-cache-directory "/.org-id-locations"))

(defconst klv/org-roam-db-location
  (concat klv/org-cache-directory "/org-roam.db"))

(defconst klv/default-font "Fira Code")
(defconst klv/default-font-src "https://github.com/ryanoasis/nerd-fonts/releases/download/v3.1.1/FiraCode.zip")

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

(defun install-font (name src)
  "Функция для установки шрифта.

(Повседневно я использую шрифт Fira Code в GNU Emacs.)

Шрифт устанавливается в директорию `~/.local/share/fonts'.

[TODO] Сделать установку асинхронной. После установки шрифт подгрузится
автоматически."
  (let ((font-dir "~/.local/share/fonts/"))
    ;; Create font directory if not exists
    (unless (file-exists-p font-dir)
      (make-directory font-dir t))

    (message (concat "Installing font: "
                     name "..."))

    (shell-command
     (concat "curl -L -o /tmp/emacs-font-installation.zip "
             klv/default-font-src
             " && "
             "unzip -o /tmp/emacs-font-installation.zip -d " font-dir
             " && "
             "rm -f /tmp/emacs-font-installation.zip && fc-cache -fv"))))

(defun is-font-available (name)
  "Установка шрифта производится при условии что система Unix,
а так же шрифт отсутствует, или это первый запуск GNU Emacs.
(смотреть `is-first-startup')"
  (and (or (eq system-type 'gnu/linux)
           (eq system-type 'darwin))
       (or is-first-startup
           (not (find-font (font-spec :name name))))))

(defun maybe-install-font (name src)
  "Данная функция устанавливает шрифт или просто возвращает его название."
  (when (and (is-font-available name)
             (window-system))
    (install-font name src))
  name)

(defun get-default-font ()
  (maybe-install-font klv/default-font
                      klv/default-font-src))

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

;; TODO Good implementation
(defmacro use-font! (&optional name &key src)
  (let ((name (or name klv/default-font))
        (src (or src klv/default-font-src)))
    `(setq klv/font ,name
           klv/font-src ,src)))

(provide 'klv)

;;; klv.el ends here
