;;; core.el --- Core feature
;;
;;; Commentary:
;;
;;; Code:

(defvar locked-first-startup nil
  "Вспомогательная переменная используемая для хранения
значания о первом запуске GNU Emacs. Используется в
функции `is-first-startup'.

`nil' --- GNU Emacs запущен не в первый раз.
  `t' --- GNU Emacs запущен в первый раз.")

(defun is-first-startup ()
  "Функция использующаяся для определения первого запуска GNU Emacs.

При первом запуске GNU Emacs в директории с конфигурацией создаётся
файл `.startup-lock', а так же переменная `locked-first-startup' устанавливается
как `t' (по умолчанию `nil')."
  (let ((lock-file (concat user-init-dir ".startup-lock")))
    (if (file-exists-p lock-file)
	(if locked-first-startup
	    t
	  nil)
      (progn
	(require 'dired)
	(dired-create-empty-file lock-file)
	(setq locked-first-startup t)
	t))))

(use-package dired
  :ensure nil
  :custom
  (dired-omit-files "\\`\\'")
  :hook (dired-mode . dired-omit-mode))

(use-package emacs
  :ensure nil
  :init
  (require 'dired)
  (add-to-list 'default-frame-alist '(fullscreen . maximized))
  
  ;; MAYBE make frame more transparent ?!
  (add-to-list 'default-frame-alist '(alpha-background . 100))
  
  :hook
  ((elpaca-after-init
    . (lambda () (load custom-file 'noerror)))
   (after-init
    . (lambda () (load custom-file 'noerror)))
   (prog-mode . display-line-numbers-mode)
   (emacs-startup
    . (lambda ()
	(message "Emacs ready in %.2f seconds with %d garbage collections"
		 (float-time (time-subtract after-init-time before-init-time))
		 gcs-done)))
   (prog-mode . hl-line-mode))

  :custom
  (make-backup-files nil)
  (frame-title-format "### %b --- GNU Emacs ###")
  (cursor-type 'box)
  (default-input-method "russian-computer")

  (indent-tabs-mode nil)
  (tab-width 4)

  (custom-file (expand-file-name "customs.el" user-init-dir))
  (gc-cons-threshold (* 128 1024 1024))
  (gc-cons-percentage 0.6)
  (read-process-output-max (* 4 1024 1024))

  :config
  (use-package menu-bar
    :ensure nil
    :config (menu-bar-mode 0))
  
  (use-package tool-bar
    :ensure nil
    :config (tool-bar-mode 0))

  (use-package scroll-bar
    :ensure nil
    :config (scroll-bar-mode 0))

  (use-package fringe
    :ensure nil
    :config (fringe-mode '(8 . 8)))

  (use-package tab-bar
    :disabled t                         ; TODO Setup `tab-bar-mode'
    :ensure nil
    :config (tab-bar-mode 1))

  (use-package which-key
    :ensure nil
    :config (which-key-mode 1))

  (use-package recentf
    :ensure nil
    :config (recentf-mode 1))
  
  (fset #'jsonrpc--log-event #'ignore)

  (column-number-mode 1))

(provide 'features/core)

;;; core.el ends here
