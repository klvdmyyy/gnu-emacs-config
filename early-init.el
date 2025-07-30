;;; early-init.el --- -*- no-byte-compile: t; lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Klementiev Dmitry <klementievd08@yandex.ru>
;;
;; Author: Klementiev Dmitry
;;
;;; Commentary:
;;
;;; Code:

;;================================================================
;; My early-init configuration file
;; --------------------------------
;; It's heavily inspired by following configurations:
;; - Doom Emacs
;; - minimal-emacs.d
;; - Spacemacs
;; - And other emacs distributions.
;;
;; NOT FOR EDIT. USE config.org INSTEAD
;;================================================================

(setq load-prefer-newer t)

(setq-default default-input-method "russian-computer"
              read-process-output-max (* 2 1024 1024)
              process-adaptive-read-buffering nil
              ffap-machine-p-known 'reject
              inhibit-compacting-font-caches t
              frame-resize-pixelwise t
              frame-inhibit-implied-resize t
              auto-mode-case-fold nil
              inhibit-startup-screen t
              inhibit-startup-echo-area-message user-login-name
              initial-buffer-choice nil
              inhibit-startup-buffer-menu t
              inhibit-x-resources t
              bidi-display-reordering 'left-to-right
              bidi-paragraph-direction 'left-to-right
              bidi-inhibit-bpa t
              initial-major-mode 'fundamental-mode
              initial-scratch-message nil)

;; Remove "For information about GNU Emacs..." message at startup
(advice-add 'display-startup-echo-area-message :override #'ignore)

;; Suppress the vanilla startup screen completely. We've disabled it with
;; `inhibit-startup-screen', but it would still initialize anyway.
(advice-add 'display-startup-screen :override #'ignore)

(defvar emacs-old-file-name-handler-alist (default-toplevel-value
                                           'file-name-handler-alist))

(defun emacs-respect-file-handlers (fn args-left)
  "Respect file handlers.
FN is the function and ARGS-LEFT is the same argument as `command-line-1'.
Emacs processes command-line files very early in startup.  These files may
include special paths like TRAMP paths, so restore `file-name-handler-alist' for
this stage of initialization."
  (let ((file-name-handler-alist (if args-left
                                     emacs-old-file-name-handler-alist
                                   file-name-handler-alist)))
    (funcall fn args-left)))

(defun emacs-restore-file-name-handler-alist ()
  "Restore `file-name-handler-alist'."
  (set-default-toplevel-value
   'file-name-handler-alist
   (delete-dups (append file-name-handler-alist
                        emacs-old-file-name-handler-alist))))

(let (file-name-handler-alist)
  (setq gc-cons-threshold most-positive-fixnum)

  (set-default-toplevel-value
   'file-name-handler-alist
   (if (locate-file-internal "calc-loaddefs.el" load-path)
       nil
     (list (rassq 'jka-compr-handler
                  emacs-old-file-name-handler-alist))))

  ;; Ensure the new value persists through any current let-binding.
  (put 'file-name-handler-alist 'initial-value
       emacs-old-file-name-handler-alist)

  (advice-add 'command-line-1 :around #'emacs-respect-file-handlers)

  (add-hook 'emacs-startup-hook #'emacs-restore-file-name-handler-alist
            101))

;; Run GNU Emacs with maximized frame
;; (push '(fullscreen . maximized) default-frame-alist)

;; In non-server mode I just don't close my GNU Emacs. (I don't need to open it maximized)
;; at start.
;;
;; However with emacs server we can use it. Because slowness startup time in
;; daemon mode it's not problem (~0.02 seconds difference)
(when (daemonp)
  (push '(fullscreen . maximized) default-frame-alist))

(push '(menu-bar-lines . 0) default-frame-alist)
(setq menu-bar-mode nil)

(push '(tool-bar-lines . 0) default-frame-alist)
(setq menu-bar-mode nil)

(push '(vertical-scroll-bars) default-frame-alist)
(push '(horizontal-scroll-bars) default-frame-alist)

(setq scroll-bar-mode nil)
(setq horizontal-scroll-bar-mode 0)

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(provide 'early-init)

;;; early-init.el ends here
