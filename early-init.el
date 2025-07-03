;;; early-init.el --- Early Initialization -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;; Startup time performance issues:
;; - `package-activate-all' function take ~0.02 seconds (maybe can make faster)
;;
;; TODO: More optimizations from minimal-emacs.d, doom emacs, spacemacs and etc
;;
;;; Code:

(setq-default
 gc-cons-threshold most-positive-fixnum
 load-prefer-newer t)

(setq-default
 package-enable-at-startup nil
 inhibit-splash-screen t
 inhibit-startup-screen t
 inhibit-startup-echo-area-message user-login-name
 initial-buffer-choice nil
 inhibit-startup-buffer-menu t
 inhibit-x-resources t
 initial-major-mode 'fundamental-mode
 initial-scratch-message nil
 frame-inhibit-implied-resize t
 frame-resize-pixelwise t
 native-comp-async-report-warnings-errors 'silent
 cursor-in-non-selected-windows nil)

(setq-default bidi-display-reordering 'left-to-right
              bidi-paragraph-direction 'left-to-right)

(setq bidi-inhibit-bpa t)

(advice-add 'display-startup-echo-area-message :override #'ignore)
(advice-add 'display-startup-screen :override #'ignore)

(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

(add-hook 'emacs-startup-hook
	  (lambda ()
	    (setq gc-cons-threshold (* 16 1024 1024))
	    (message "Emacs started in %.3f seconds."
		     (float-time (time-subtract after-init-time
						before-init-time))))
	  ;; Why Depth is 100 !?
	  ;;
	  ;; Because we also load a lot of packages in `emacs-startup-hook'.
	  ;;
	  ;; We need to re-setup GC after startup (all starting packages)
	  100)

(defun load-module (&rest args)
  "Usage: (load-module [CATEGORY] [MODULE-NAME])

Categories: completion, appearance, language, core, programming and e.t.c

TODO: Maybe future usage with more than one module from same category:
(load-module [CATEGORY] [MODULE-NAME]...)

Load multiple loads from one category in one call of the `load-module'."
  (if (or (> (length args) 2)
          (< (length args) 2))
      (error "Incorrect load-module call. Usage: (load-module [CATEGORY] [MODULE-NAME])")
    (let* ((file-name-handler-alist nil)
           (load-suffixes '(".elc" ".el"))
           (module (apply #'format (cons (concat user-emacs-directory
                                                 "modules/%s/%s.el")
                                         (seq-map #'prin1-to-string args)))))
      (if (file-exists-p module)
          (progn (load module 'noerror t nil 'must-suffix)
                 ;; FIXME: Hardcoded for only two arguments in function call
		         ;; TODO: Implementation !? (for `modulep!' like in Doom Emacs)
                 ;; (add-to-list 'gw--loaded-modules `(,(car args) ,(cadr args)))
                 t)
        (message "Module not found: %s" module)
        nil))))

(provide 'early-init)

;;; early-init.el ends here
