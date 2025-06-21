;;; klv-font.el --- -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;; Code:

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
       (not (find-font (font-spec :name name)))))

(defun maybe-install-font (name src)
  "Данная функция устанавливает шрифт или просто возвращает его название."
  (when (and (is-font-available name)
             (window-system))
    (install-font name src))
  name)

(defun load-face-attributes (name src &optional height)
  (let ((choosen-font (maybe-install-font name src))
        (font-height (or height 130)))
    (set-face-attribute 'default nil :font choosen-font :height font-height)
    (set-face-attribute 'fixed-pitch nil :font choosen-font :height font-height)
    (set-face-attribute 'variable-pitch nil :font choosen-font :height font-height :weight 'regular)))

(defun load-face-attributes-to-frame (name src height frame)
  (select-frame frame)
  (load-face-attributes name src height))

(defun set-and-maybe-install-font (name src &optional height)
  (if (daemonp)
      (add-hook 'after-make-frame-functions (apply-partially #'load-face-attributes-to-frame name src height))
    (load-face-attributes name src height)))

(defun klv--parse-font-url (args &optional url)
  "Parse args and return Font url for downloading.

This implementation is not good enough :>...

[ISSUE] Only GNU/Linux supported...

[IMPORTANT] You must use github releases downloading. Firstly you
should set github repo, only after `:github' field you should
setup the `:release'

[TODO] `use-package' like implementation with a lot of extensibility"
  (pcase args
    ((pred seq-empty-p) url)
    (`(:github ,github . ,next)
     (klv--parse-font-url next (concat "https://github.com/" github "/")))
    (`(:release ,release . ,next)
     (klv--parse-font-url next (concat url "releases/download/" release)))
    (code (klv--parse-font-url (cdr args) url))))

(defmacro use-font (name &rest args)
  (declare (indent defun))
  (let ((name name))
    `(set-and-maybe-install-font
      ,name ,(klv--parse-font-url ',args))))

(provide 'klv-font)

;;; klv-font.el ends here
