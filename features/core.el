;;; core.el --- Core feature
;;
;;; Commentary:
;;
;;; Code:

(use-package dired
  :ensure nil
  :custom
  (dired-omit-files "\\`\\'")
  :hook ((dired-mode . dired-omit-mode)))

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
  (global-visual-line-mode 1)
  (blink-cursor-mode 0)
  
  (fset #'jsonrpc--log-event #'ignore)

  (column-number-mode 1))

(use-package which-key
  :ensure nil
  :hook (after-init . which-key-mode))

(use-package recentf
  :ensure nil
  :hook (after-init . which-key-mode))

;;; core.el ends here
