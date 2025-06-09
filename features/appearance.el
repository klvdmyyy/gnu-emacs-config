;;; appearance.el
;;
;;; Commentary:
;;
;;; Code:

(defun get-my-font ()
  "Функция для установки шрифта.

Повседневно я использую шрифт Fira Code в GNU Emacs.

Данная функция устанавливает шрифт и возвращает его название.

Установка шрифта производится при условии что система Unix,
а так же шрифт отсутствует, или это первый запуск GNU Emacs.
(смотреть `is-first-startup')

Шрифт устанавливается из официального репозитория Nerd Fonts
в директорию `~/.local/share/fonts'.

[TODO] Сделать установку асинхронной. После установки шрифт подгрузится
автоматически."
  (when (and (or (eq system-type 'gnu/linux)
		 (eq system-type 'darwin))
	     (or (is-first-startup)
		 (not
		  (find-font (font-spec :name "Fira Code")))))
    (let ((font-dir "~/.local/share/fonts/"))
      (unless (file-exists-p font-dir)
	(make-directory font-dir t))
      
      (message "Installing Fira Code...")

      (shell-command
       (concat "curl -L -o /tmp/FiraCode.zip "
	       "https://github.com/ryanoasis/nerd-fonts/releases/download/v3.1.1/FiraCode.zip"
	       " && "
	       "unzip -o /tmp/FiraCode.zip -d " font-dir " && "
	       "rm /tmp/FiraCode.zip && fc-cache -f")))) ; or `fc-cache -fv'
  
  ;; Just return font-name
  "Fira Code")

(defun load-face-attributes ()
  (let ((my-font (get-my-font))		; "JetBrains Mono", "Fira Code"
	(my-height 130))
    (set-face-attribute 'default nil :font my-font :height my-height)
    (set-face-attribute 'fixed-pitch nil :font my-font :height my-height)
    (set-face-attribute 'variable-pitch nil :font my-font :height my-height :weight 'regular)))

(defun load-face-attributes-to-frame (frame)
  (select-frame frame)
  (load-face-attributes))

(if (daemonp)
    (add-hook 'after-make-frame-functions #'load-face-attributes-to-frame)
  (load-face-attributes))

(use-package doom-themes
  :demand t
  :config
  (load-theme 'doom-one t nil))

(use-package doom-modeline
  :demand t
  :custom
  (doom-modeline-height 30)
  (doom-modeline-bar-width 3)
  (doom-modeline-hud nil)
  (doom-modeline-percent-position '(-3 "%p"))
  
  :config
  (when (is-first-startup)
    (nerd-icons-install-fonts))
  (doom-modeline-mode 1))

(provide 'features/appearance)

;;; appearance.el ends here
