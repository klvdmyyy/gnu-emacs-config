;;; first-startup.el --- First Startup Checking -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Klementiev Dmitry <klementievd08@yandex.ru>
;;
;; Author:  Klementiev Dmitry
;; Email:   klementievd08@yandex.ru
;; License: None
;; Date:    2025-08-19 20:32
;;
;;; Commentary:
;;
;; Simple module for checking if user
;; runs Emacs at first time.
;;
;;; Code:

(defcustom first-startup--lock-file
  (expand-file-name ".startup-lock" user-emacs-directory)
  "Startup lock file."
  :group 'first-startup
  :type 'file)

(defun first-startup--lock ()
  "Lock startup."
  (require 'dired-aux)
  ;; If we have more than one instances of Emacs it can
  ;; cause a error.
  (unless (file-exists-p first-startup--lock-file)
	(dired-create-empty-file first-startup--lock-file)))

(defun first-startup-p ()
  "Return non-nil if user start Emacs at first time.

Creates .startup-lock file in `user-emacs-directory'.
If you delete it this function returns t at next startup."
  (if (file-exists-p first-startup--lock-file)
	  nil
	(add-hook 'kill-emacs-hook #'first-startup--lock)
	t))

(provide 'first-startup)

;;; first-startup.el ends here
