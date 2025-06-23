;;; early-init.el --- Early Initialization -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;; Code:

(provide 'early-init)

;; Move to bootstrapping
(setq package-enable-at-startup nil
      inhibit-startup-screen t
      frame-inhibit-implied-resize t
      inhibit-startup-echo-area-message user-login-name
      initial-major-mode 'fundamental-mode
      initial-scratch-message nil
      native-comp-async-report-warnings-errors 'silent

      ;; For use-package debugging (i don't need it)
      use-package-compute-statistics nil)

(let (file-name-handler-alist)
  ;; NOTE Setup GC before and after startup
  (setq gc-cons-threshold most-positive-fixnum)

  ;; NOTE What is it !?
  ;; (setq load-prefer-newer noninteractive)

  (let* (;; You must put ".elc" before ".el" in this list
         ;; You should set only to: '(".so" ".elc" ".el") or '(".elc" ".el")
         (load-suffixes '(".elc" ".el"))

         ;; Interesting variable :>
         (file-name-handler-alist nil)
         (use-font-file (expand-file-name "lisp/use-font" user-emacs-directory))
         (klv-loaders-file (expand-file-name "lisp/klv-loaders" user-emacs-directory)))
    ;; NOTE I think it can cause some performance issues with startup-time
    (autoload 'use-font use-font-file)
    (autoload 'this-person klv-loaders-file)
    (autoload 'load-features klv-loaders-file)
    (autoload 'load-languages klv-loaders-file)))

;;; early-init.el
