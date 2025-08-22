;;; bootstrap.el --- Bootstraping GNU Emacs -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;; Code:

;; TODO: Parse themes from variable(not constant) like this one.
(defconst user-themes '((default-light . modus-operandi)
						(default-dark  . modus-vivendi))
  "User themes.")

;; TODO: Move it from `bootstrap.el'
(defsubst process-await (process)
  "Sleep untile PROCESS is exit."
  (while (process-live-p process)
	t))

(defsubst install-adwaita-mono-nerd ()
  "."
  (let ((name "Adwaita Mono")
		(src "https://github.com/nazmulidris/adwaita-mono-nerd-font")
		(git (executable-find "git"))
		(curl (executable-find "curl"))
		(unzip (executable-find "unzip"))
		(font-dir "~/.local/share/fonts/"))
	(unless (file-exists-p font-dir)
	  (mkdir font-dir t))

	(setq
	 adwaitamono-nerd-font-installation-process
	 (start-process-shell-command
	  "adwaitamono-nerd-font-installation"
	  "adwaitamono-nerd-font-installation"
	  (concat git " clone " src " /tmp/emacs-font-installation"
			  " --depth 1"
			  " && "
			  "mv /tmp/emacs-font-installation/* " font-dir
			  " && "
			  "rm -rf /tmp/emacs-font-installation && fc-cache -fv")))))

(defcustom bootstrap-no-startup-screen t
  "Don't show any startup screen.

Shows empty scratch buffer at startup with fundamental-mode."
  :type 'boolean
  :group 'bootstrap)

(defcustom bootstrap-no-package-startup t
  "Don't load package manager at startup."
  :type 'boolean
  :group 'bootstrap)

(defcustom bootstrap-no-menu-bar t
  "Hide menu bar."
  :type 'boolean
  :group 'bootstrap)

(defcustom bootstrap-no-tool-bar t
  "Hide tool bar."
  :type 'boolean
  :group 'bootstrap)

(defcustom bootstrap-no-scroll-bar t
  "Hide scroll bar."
  :type 'boolean
  :group 'bootstrap)

(defcustom bootstrap-maximize-frame-at-startup nil
  "Maximize frame at startup.

It can slow down startup time."
  :type 'boolean
  :group 'bootstrap)

(defcustom bootstrap-optimize-gc t
  "A little GC optimizations."
  :type 'boolean
  :group 'bootstrap)

(defcustom bootstrap-optimize-file-name-handler-alist t
  "Enable optimizations for `file-name-handler-alist'."
  :type 'boolean
  :group 'bootstrap)

(defcustom bootstrap-optimize-loading nil
  "Optimize `load' function with some advices."
  :type 'boolean
  :group 'bootstrap)

;;; Startup screen:

(defun bootstrap-startup-screen ()
  (when bootstrap-no-startup-screen
    ;; Remove "For information about GNU Emacs..." message at startup
    (advice-add 'display-startup-echo-area-message :override #'ignore)

    ;; Suppress the vanilla startup screen completely. We've disabled it with
    ;; `inhibit-startup-screen', but it would still initialize anyway.
    (advice-add 'display-startup-screen :override #'ignore)

    (setq-default
     ;; Startup screen
     inhibit-startup-screen t
     inhibit-startup-echo-area-message user-login-name
     inhibit-startup-buffer-menu t

     ;; Scratch buffer
     initial-major-mode 'fundamental-mode
     initial-scratch-message nil
     initial-buffer-choice nil

     ;; Other
     inhibit-x-resources t)))

;;; Package:

(defun bootstrap-startup-package ()
  (when bootstrap-no-package-startup
    (setq-default package-enable-at-startup nil)))

;;; UI:

(defvar bootstrap-theme 'modus-vivendi)
(defvar bootstrap-font "AdwaitaMono Nerd Font")

(defun load-face-attributes (name height)
  (let ((choosen-font name)
        (font-height (or height 130)))
    (set-face-attribute 'default nil :font choosen-font :height font-height)
    (set-face-attribute 'fixed-pitch nil :font choosen-font :height font-height)
    (set-face-attribute 'variable-pitch nil :font choosen-font :height font-height :weight 'regular)))

(defun bootstrap-startup-ui ()
  (if (daemonp)
      (add-hook 'after-make-frame-functions
                (lambda (frame)
                  (with-selected-frame frame
                    (load-theme bootstrap-theme :no-confirm))))
    (add-hook 'after-init-hook
              (lambda ()
                (load-theme bootstrap-theme :no-confirm))))

  (if (daemonp)
      (add-hook 'after-make-frame-functions
                (lambda (frame)
                  (with-selected-frame frame
                    (load-face-attributes bootstrap-font 130))))
    (add-hook 'emacs-startup-hook
              (lambda ()
                (load-face-attributes bootstrap-font 130))))

  (when bootstrap-no-menu-bar
    (push '(menu-bar-lines . 0) default-frame-alist)
    (setq menu-bar-mode nil))

  (when bootstrap-no-tool-bar
    (push '(tool-bar-lines . 0) default-frame-alist)
    (setq tool-bar-mode nil))

  (when bootstrap-no-scroll-bar    
    (push '(vertical-scroll-bars) default-frame-alist)
    (push '(horizontal-scroll-bars) default-frame-alist)

    (setq scroll-bar-mode nil)
    (setq horizontal-scroll-bar-mode 0)))

;;; Frame:

(defun bootstrap-startup-frame ()
  (when bootstrap-maximize-frame-at-startup
    (push '(fullscreen . maximized) default-frame-alist)))

;;; GC:

(defun emacs-restore-gc ()
  (setq gc-cons-threshold (* 16 1024 1024)))

(defun bootstrap-startup-gc ()
  (when bootstrap-optimize-gc
    (setq gc-cons-threshold most-positive-fixnum)
    (add-hook 'emacs-startup-hook #'emacs-restore-gc 200)))

;;; `file-name-handler-alist' optimization:

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

(defun bootstrap-startup-file-name-handler-alist ()
  (when bootstrap-optimize-file-name-handler-alist
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
                101))))

;;; Loading:

(defun bootstrap-startup-loading ()
  (when bootstrap-optimize-loading
    (define-advice load
        (:around (orig-fun &rest args) o-load-suffixes)
      (let ((load-suffixes '(".elc" ".el")))
        (apply orig-fun args)))

    (add-hook 'after-init-hook
              (lambda ()
                (advice-remove 'load #'load@o-load-suffixes)))))

;;; Main bootstraping function:

(defun emacs-bootstrap (&rest _)
  "Early stage function.

Bootstraping GNU Emacs.  Optimizations and etc."
  ;; Silent native compilation
  (setq-default native-comp-async-report-warnings-errors 'silent)

  (add-to-list 'load-path
               (expand-file-name "lisp/" user-emacs-directory))

  (require 'first-startup)

  ;; Install and setup font asynchronously.
  (when (first-startup-p)
	(install-adwaita-mono-nerd)

	;; Wait font installation before setting up faces.
	(define-advice load-face-attributes
		(:before (&rest _) await-font)
	  (process-await adwaitamono-nerd-font-installation-process)))

  (add-hook 'after-init-hook
            (lambda ()
              (message "Emacs initialized in %.4f with %d GCs done."
                       (float-time
                        (time-subtract
                         after-init-time
                         before-init-time))
                       gcs-done))
            300)

  (setq-default frame-title-format "Emacs Workflowww"
                custom-file (expand-file-name "custom.el" user-emacs-directory)
                cursor-type '(bar . 2)
                ;; cursor-type 'box
                cursor-in-non-selected-windows nil
                default-input-method "russian-computer"
                tab-width 4)

  (add-hook 'after-init-hook
            (lambda ()
              (blink-cursor-mode 0)
              (indent-tabs-mode 0)))

  (add-hook 'after-init-hook
            (lambda ()
              (load custom-file :no-error :no-message :no-suffix :must-suffix)))

  (add-hook 'text-mode-hook 'visual-line-mode)
  (add-hook 'prog-mode-hook 'visual-line-mode)
  ;; (add-hook 'after-init-hook 'global-visual-line-mode)

  (add-hook 'after-init-hook 'which-key-mode)
  (add-hook 'after-init-hook 'recentf-mode)

  ;; Just call all functions
  (bootstrap-startup-screen)
  (bootstrap-startup-package)
  (bootstrap-startup-ui)
  (bootstrap-startup-frame)
  (bootstrap-startup-gc)
  (bootstrap-startup-file-name-handler-alist)
  (bootstrap-startup-loading)

  (setq-default backup-directory-alist
                `(("**" . ,(expand-file-name "backups/" user-emacs-directory))))

  (setq-default auto-save-timeout 10
                auto-save-interval 100)

  (seq-map (lambda (alist)
             (mkdir (cdr alist) t))
           backup-directory-alist)

  ;; Make it asynchronously =)
  ;; (like font installation)
  (package-activate-all))

(provide 'bootstrap)

;;; bootstrap.el ends here
