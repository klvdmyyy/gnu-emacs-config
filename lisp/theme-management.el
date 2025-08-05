;;; theme-management.el --- EFlow theme management -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;; TODO: Load theme interactively
;; TODO: Load theme on `default-theme' variable change
;;
;;; Code:

;; Good light themes:
;; '(tao-yang . tao-theme)
;; '(doom-earl-grey . doom-themes)
(defvar default-theme '(doom-earl-grey . doom-themes)
  "Default theme for GNU Emacs.

  1. Autoload `default-theme' from theme-package
  2. Load theme

  How to setup variable:
  1. Just a name of theme. Load theme without any installation.
  2. (THEME-NAME . PACKAGE-NAME) - Install PACKAGE-NAME and load THEME-NAME from it.")

(defun better-load-theme (theme)
  "Load THEME like `load-theme' but better."
  (if (daemonp)
      (add-hook 'after-make-frame-functions
                (lambda (frame)
                  (with-selected-frame frame
                    (load-theme theme :no-confirm))))
    (load-theme theme :no-confirm)
    (add-hook 'elpaca-after-init-hook
	      (lambda ()
                (load-theme theme :no-confirm))))
  
  ;; Load theme now.
  (when (boundp 'after-init-time)
    (load-theme theme :no-confirm)))

;; Load theme from `default-theme' variable.
(defun load-default-theme ()
  "Parse `default-theme' and load it."
  (interactive)
  (pcase default-theme
    ((pred symbolp)
     (eval `(better-load-theme ',default-theme)))
    (`(,default-theme . ,package)
     (eval `(elpaca ',package
  	      (autoload ',default-theme ,(prin1-to-string package))
  	      (better-load-theme ',default-theme))))))

(provide 'theme-management)

;;; theme-management.el ends here
