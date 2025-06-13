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

(defun which-linux-distribution ()
  "Maybe useful for future development of my GNU Emacs configuration.

I don't use it now.

[IMPORTANT] You need a `lsb_release' executable. Without it function doesn't
work and just return Unknown."
  (interactive)
  (when (eq system-type 'gnu/linux)
    (let* ((lsb_release (executable-find "lsb_release"))
           (cmd (if lsb_release (concat lsb_release " -sd") nil))
           (result (if cmd (shell-command-to-string cmd) "Unknown")))
      (message result))))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;; Org Roam ;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun roam-make-header (&key tags title date?)
  "Make a header for Org Roam capture

#+title: ${title}
#+author: `user-full-name'
#+email: `user-mail-address'
#+language: Russian
#+license: CC BY-SA 4.0
#+date: %<%Y-%m-%d>
#+filetags: <tags>"
  (let* ((klv/roam-headers `("#+title: ${title}"
                             ,(concat "#+author: " user-full-name)
                             ,(concat "#+email: " user-mail-address)
                             "#+language: Russian"
                             "#+license: CC BY-SA 4.0"
                             "#+date: %<%Y-%m-%d>"))
         (headers (seq-map (lambda (h) (concat h "\n")) klv/roam-headers))
         (tags (or tags '()))
         (with-tags (append headers
                            (list
                             (concat "#+filetags: :"
                                     (apply #'concat (seq-map
                                                      (lambda (t)
                                                        (concat t ":"))
                                                      tags)))))))
    (apply #'concat (append with-tags '("\n")))))

(defun roam-make-daily-header ()
  "Make a header for daily Org Roam capture

#+title: %<%Y-%m-%d>
#+author: `user-full-name'
#+email: `user-mail-address'
#+language: Russian
#+license: CC BY-SA 4.0
#+filetags: dailies:%<%Y-%m-%d>:daily"
  (let* ((klv/roam-headers `("#+title: %<%Y-%m-%d>"
                             ,(concat "#+author: " user-full-name)
                             ,(concat "#+email: " user-mail-address)
                             "#+language: Russian"
                             "#+license: CC BY-SA 4.0"))
         (headers (seq-map (lambda (h) (concat h "\n")) klv/roam-headers))
         (tags '("dailies" "%<%Y-%m-%d>" "daily"))
         (with-tags (append headers
                            (list
                             (concat "#+filetags: :"
                                     (apply #'concat (seq-map
                                                      (lambda (t)
                                                        (concat t ":"))
                                                      tags)))))))
    (apply #'concat (append with-tags '("\n")))))

(defconst klv/roam-dailies-capture-templates
  `(("d" "default" entry
     "%?"
     :target (file+head "%<%Y-%m-%d>.org"
                        ,(roam-make-daily-header)))))

(defconst klv/roam-capture-templates
  `(("y" "Yandex" plain
     "%?"
     :target (file+head "yandex/${slug}.org"
                        ,(roam-make-header
                          :tags '("yandex")))
     :unarrowed t)
    ("a" "Yandex Algorithms" plain
     "#+begin_src go :noweb yes :noweb-prefix no :imports '(\"fmt\")%?\n#+end_src\n"
     :target (file+head "yandex/algorithms/${slug}.org"
                        ,(roam-make-header
                          :tags '("algorithms"
                                  "yandex")))
     :unarrowed t)
    ("b" "Business" plain
     "%?"
     :target (file+head "business/${slug}.org"
                        ,(roam-make-header
                          :tags '("business")))
     :unarrowed t)))

(provide 'klv)

;;; klv.el ends here
