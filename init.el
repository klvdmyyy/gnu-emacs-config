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
