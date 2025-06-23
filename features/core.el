;;; core.el --- Core Features -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;; Code:

(use-package emacs
  :ensure nil
  :init
  (add-to-list 'default-frame-alist '(fullscreen . maximized))
  (add-to-list 'default-frame-alist '(alpha-background . 100))

  :hook ((prog-mode . display-line-numbers-mode)
         (after-init
          . (lambda ()
              (let ((file-name-handler-alist nil)
                    (load-suffixes '(".el")))
                (load custom-file 'noerror))))
         (emacs-startup
          . (lambda ()
              (message "Emacs ready in %.3f seconds with %d garbage collections"
                       (float-time (time-subtract after-init-time before-init-time))
                       gcs-done)
              (setq gc-cons-threshold (* 16 1024 1024)))))

  :custom
  (frame-title-format "*KlvEmacs* :: %f")

  (cursor-type '(bar . 2))
  (cursor-in-non-selected-windows nil)

  (default-input-method "russian-computer")

  (make-backup-files nil)
  (custom-file (expand-file-name "customs.el" user-emacs-directory))

  (indent-tabs-mode nil)
  (tab-width 4)

  :config
  (global-visual-line-mode 1)
  (blink-cursor-mode 0)
  (column-number-mode 1)
  (fset #'jsonrpc--log-event #'ignore))

;; TODO Maybe move which-key and recentf from core feature

(use-package which-key
  :ensure nil
  :hook emacs-startup)

(use-package recentf
  :ensure nil
  :hook emacs-startup)

;;; core.el ends here
