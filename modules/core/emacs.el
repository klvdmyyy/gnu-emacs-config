;;; emacs.el --- Emacs Core module -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;; Code:

(eval-when-compile
  (require 'recentf)
  (require 'which-key)
  (require 'hl-line)
  (require 'display-line-numbers)
  (require 'fringe))

(with-eval-after-load 'emacs
  (setq-default frame-title-format "GNU Emacs :: %b"
                make-backup-files nil
                custom-file (expand-file-name "customs.el" user-emacs-directory)
                cursor-type 'box
                ;; cursor-type '(bar . 2)
                cursor-in-non-selected-windows nil
                default-input-method "russian-computer"
                indent-tabs-mode nil
                tab-width 4)
  (blink-cursor-mode 0)

  (add-hook
   'after-init-hook
   (lambda () (load custom-file 'noerror t nil 'must-suffix))))

(with-eval-after-load 'recentf
  (add-hook 'emacs-startup-hook 'recentf-mode))

(with-eval-after-load 'which-key
  (add-hook 'emacs-startup-hook 'which-key-mode))

(with-eval-after-load 'hl-line
  (add-hook 'prog-mode-hook 'hl-line-mode)
  (add-hook 'dired-mode-hook 'hl-line-mode))

(with-eval-after-load 'display-line-numbers
  (add-hook 'prog-mode-hook 'display-line-numbers-mode))

(with-eval-after-load 'fringe
  (fringe-mode '(4 . 4)))

;;; emacs.el ends here
