;;; klv-font.el --- -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;; Code:

(defconst klv/default-font "Fira Code")
(defconst klv/default-font-src
  "https://github.com/ryanoasis/nerd-fonts/releases/download/v3.1.1/FiraCode.zip")

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

;; TODO Good implementation
(defmacro use-font! (&optional name &key src)
  (let ((name (or name klv/default-font))
        (src (or src klv/default-font-src)))
    `(setq klv/font ,name
           klv/font-src ,src)))

(provide 'klv-font)

;;; klv-font.el ends here
