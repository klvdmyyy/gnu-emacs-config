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

;; Core

(require! 'features/core)

;; Package manager

(require! 'features/elpaca)

;; Other features

(require! 'features/appearance)

(require! 'features/gcmh)

(require! 'features/completion)
(require! 'features/vertico)

(require! 'features/git)

(require! 'features/smartparens)

(require! 'features/productivity)

(require! 'features/esh)
(require! 'features/eat)

(require! 'features/company)

;; Languages

(require! 'languages/cc)
(require! 'languages/go)
(require! 'languages/proto)
(require! 'languages/docker)
(require! 'languages/yaml)

;;; init.el ends here
