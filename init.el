;;; init.el --- Initialization -*- no-byte-compile: t; lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Klementiev Dmitry <klementievd08@yandex.ru>
;;
;; Author: Klementiev Dmitry <klementievd08@yandex.ru>
;;
;;; Commentary:
;;
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
;; NOT FOR EDIT. USE config.org INSTEAD
;;================================================================

(defun emacs-restore-gc-cons-threshold ()
  "Restore `gc-cons-threshold'."
  (setq gc-cons-threshold (* 16 1024 1024)))

(defun emacs-display-init-time ()
  "Display Emacs init time."
  (message "Emacs initialized in %.3f with %d garbage collections."
           (float-time
            (time-subtract
             elpaca-after-init-time
             before-init-time))
           gcs-done))

(add-hook 'elpaca-after-init-hook #'emacs-restore-gc-cons-threshold)
(add-hook 'elpaca-after-init-hook #'emacs-display-init-time)

;;; Bootstrap Elpaca:

(defvar elpaca-installer-version 0.11)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                        :ref nil :depth 1 :inherit ignore
                        :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                        :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (<= emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let* ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                  ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                  ,@(when-let* ((depth (plist-get order :depth)))
                                                      (list (format "--depth=%d" depth) "--no-single-branch"))
                                                  ,(plist-get order :repo) ,repo))))
                  ((zerop (call-process "git" nil buffer t "checkout"
                                        (or (plist-get order :ref) "--"))))
                  (emacs (concat invocation-directory invocation-name))
                  ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                        "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                  ((require 'elpaca))
                  ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (let ((load-source-file-function nil)) (load "./elpaca-autoloads"))))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

;;; Count external packages loaded by Elpaca usage:

;; TODO: This section need some fixes

(defvar external-packages-list '())
(defvar external-packages-loaded-count 0)
(defvar external-packages-loaded '())

(when (file-exists-p (expand-file-name "elpaca/builds" user-emacs-directory))
  (let* ((external-start-time (current-time))
	 (elpaca-load-path 
          (cddr (directory-files (expand-file-name "elpaca/builds" user-emacs-directory) t))))
    (dolist (path elpaca-load-path)
      (let* ((files (directory-files path nil ".el\\'"))
             (files (seq-map (lambda (file) (substring file 0 (- (length file) 3))) files))
             (packages (seq-map #'intern files)))
	(setq external-packages-list
              (append
               external-packages-list
               packages))))))

(defun external-packages-require (package &rest _)
  (when (and (member package external-packages-list)
             (not (member package external-packages-loaded)))
    (add-to-list 'external-packages-loaded package)
    (setq external-packages-loaded-count
          (+ external-packages-loaded-count 1))))

(advice-add 'require :after #'external-packages-require)
(advice-add 'load :after #'external-packages-require)

(add-hook 'elpaca-after-init-hook
          (lambda ()
            (message "Loaded %d external packages at startup: %s"
                     external-packages-loaded-count
                     external-packages-loaded)))

;;; Compile and Load configuration file (Org Babel -> Emacs Lisp):

;; Load `org-babel-load-file' function before usage
(autoload 'org-babel-load-file "org")

;; Other autoloads
(autoload 'dired-remove-file "dired")
(autoload 'dired-make-relative-symlink "dired")

(defun init-emacs ()
  (interactive)
  ;; PERF: We don't need to always load a Emacs.org file.
  ;;   Just check if Emacs.elc is up-to-date. Keep good startup
  ;;   time with org-babel based configuration.
  (let* ((load-suffixes '(".elc" ".el"))
         (config.org (expand-file-name "config.org" user-emacs-directory))
         (README.org (expand-file-name "README.org" user-emacs-directory))
         (config.el (expand-file-name (file-name-with-extension config.org "el") user-emacs-directory))
         (config.elc (expand-file-name (file-name-with-extension config.el "elc") user-emacs-directory))
         (byte-compile-verbose nil)
         (byte-compile-warnings nil))
    (if (and (file-exists-p config.elc)
             (file-newer-than-file-p config.elc config.el)
             (file-newer-than-file-p config.el config.org))
        ;; Load compiled file
        (load config.elc :no-error :no-message :no-suffix :must-suffix)
      ;; Set second argument to t mean we byte-compile the file before loading
      (org-babel-load-file config.org t))
    
    ;; Make relative symbolic link: README.org -> Emacs.org
    (unless (and (file-exists-p README.org)
                 (file-symlink-p README.org))
      (dired-remove-file README.org)
      (dired-make-relative-symlink config.org README.org))))

;; Just initialize GNU Emacs
(init-emacs)

(provide 'init)

;;; init.el ends here
