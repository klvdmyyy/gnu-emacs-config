;;; init.el
;;
;;; Commentary:
;;
;;; Code:

;; (setq safe-local-variable-values
;;       '((dired-omit-files . "auto-save-list\\'")))

;; Setup user init directory
(defconst user-init-dir
  (cond ((boundp 'user-emacs-directory)
         user-emacs-directory)
        ((boundp 'user-init-directory)
         user-init-directory)
        (t "~/.emacs.d/")))

(defmacro register-user-lp (lp)
  `(add-to-list
    'load-path
    (concat user-init-dir ,lp)))

(register-user-lp "features")

(defmacro require! (module)
  `(require
    ,module
    (concat user-init-dir (prin1-to-string ,module) ".el")))

(require! 'features/core)

(require! 'features/elpaca)

(require! 'features/appearance)
(require! 'features/gcmh)
(require! 'features/completion)
(require! 'features/vertico)
(require! 'features/git)
(require! 'features/productivity)

;;; init.el ends here
