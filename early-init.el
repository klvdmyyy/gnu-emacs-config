;;; early-init.el
;;
;;; Commentary:
;;
;;; Code:

(setq package-enable-at-startup nil
      inhibit-startup-message t
      frame-resize-pixelwise t
      package-native-compile t)

(setq gc-cons-threshold (* 128 1024 1024)
      gc-cons-percentage 0.6
      read-process-output-max (* 4 1024 1024))

(setq native-comp-async-report-warning-errors 'silent)

;; `lsp-mode' use `plists' for deserialization
(setenv "LSP_USE_PLISTS" "true")

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

;; MAYBE Use `load' instead of `require' for features/modules
(register-user-lp "features")

;; TODO Setup `lisp/' separated directory
(register-user-lp "lisp")

(defmacro require! (module)
  `(require
    ,module
    (concat user-init-dir (prin1-to-string ,module) ".el")))

(provide 'early-init)

;;; early-init.el ends here
