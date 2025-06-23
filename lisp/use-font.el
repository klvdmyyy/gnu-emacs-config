;;; use-font.el --- use-package for fonts -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;; NOTE It can be placed as separated package in my github
;;
;;; Code:

(defun install-font (name src)
  "Функция для установки шрифта.

(Повседневно я использую шрифт Fira Code в GNU Emacs.)

Шрифт устанавливается в директорию `~/.local/share/fonts'.

[TODO] Сделать установку асинхронной. После установки шрифт подгрузится
автоматически.

------------------------------------

По факту это максимально тупая функция которая берёт в себя только название шрифта
и ссылку на архив с ним (FIXME Поддерживается только zip-архив)"
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
    (add-hook 'emacs-startup-hook (apply-partially #'load-face-attributes name src height))))

(defun use-font--parse-disabled (args)
  (pcase args
    ((pred seq-empty-p) t)
    (`(:disabled nil . ,next-args) t)
    (`(:disabled . ,next-args) nil)
    (code (use-font--parse-disabled (cdr args)))))

(defun use-font--parse-if-and-unless (args)
  (pcase args
    ((pred seq-empty-p) t)
    (`(:if ,expr . ,next-args)
     (if (eval expr) t nil))
    (`(:unless ,expr . ,next-args)
     (if (eval expr) nil t))
    (code (use-font--parse-if-and-unless (cdr args)))))

(defun use-font--parse-github-url (args)
  (pcase args
    ((pred seq-empty-p) (error "Can't parse github url for use-font"))
    (`(:release ,release . ,next-args)
     release)
    (code (use-font--parse-github-url (cdr args)))))

(defun use-font--parse-url (args)
  (pcase args
    ((pred seq-empty-p) (error "Can't parse use-font macro"))
    (`(:github ,github . ,next-args)
     (concat "https://github.com/" github "/"
             (use-font--parse-github-url next-args)))
    (code (use-font--parse-url (cdr args)))))

;;;###autoload
(defmacro use-font (name &rest args)
  (declare (indent defun))

  (when (not (stringp name))
    (error "use-font error: font name must be a string"))

  (when (and (use-font--parse-if-and-unless args)
             (use-font--parse-disabled args))
    `(set-and-maybe-install-font ,name ,(use-font--parse-url args))))

(provide 'use-font)

;;; use-font.el ends here
