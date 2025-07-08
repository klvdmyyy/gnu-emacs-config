;;; init.el --- Initialization -*- no-byte-compile: t; lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;;================================================================
;; My init configuration file
;; --------------------------
;; It's heavily inspired by following configurations:
;; - Doom Emacs
;; - minimal-emacs.d
;; - Spacemacs
;; - And other emacs distributions.
;;
;; NOT FOR EDIT. USE Emacs.org INSTEAD
;;================================================================

(defun emacs-restore-gc-cons-threshold ()
  "Restore `gc-cons-threshold'."
  (setq gc-cons-threshold (* 16 1024 1024)))

(defun emacs-display-init-time ()
  "Display Emacs init time."
  (message "Emacs initialized in %.3f with %d garbage collections."
           (float-time
            (time-subtract
             after-init-time
             before-init-time))
           gcs-done))

(add-hook 'emacs-startup-hook #'emacs-restore-gc-cons-threshold 105)
(add-hook 'emacs-startup-hook #'emacs-display-init-time 105)

;; Load `org-babel-load-file' function before usage
(autoload 'org-babel-load-file "org")

;; Other autoloads
(autoload 'dired-remove-file "dired")
(autoload 'dired-make-relative-symlink "dired")

;;; PERF: We don't need to always load a Emacs.org file.
;;; Just check if Emacs.elc is up-to-date. Keep good startup
;;; time with org-babel based configuration.
(let* ((load-suffixes '(".elc" ".el"))
       (Emacs.org (expand-file-name "Emacs.org" user-emacs-directory))
       (README.org (expand-file-name "README.org" user-emacs-directory))
       (Emacs.el (expand-file-name "Emacs.el" user-emacs-directory))
       (Emacs.elc (expand-file-name "Emacs.elc" user-emacs-directory)))
  (if (and (file-exists-p Emacs.elc)
           (file-newer-than-file-p Emacs.elc Emacs.el)
           (file-newer-than-file-p Emacs.elc Emacs.org))
      ;; Load compiled file
      (load Emacs.elc :no-error :no-message :no-suffix :must-suffix)
    ;; Set second argument to t mean we byte-compile the file before loading
    (org-babel-load-file Emacs.org t))
  
  ;; Make relative symbolic link: README.org -> Emacs.org
  (unless (and (file-exists-p README.org)
               (file-symlink-p README.org))
    (dired-remove-file README.org)
    (dired-make-relative-symlink Emacs.org README.org)))

(provide 'init)

;;; init.el ends here
