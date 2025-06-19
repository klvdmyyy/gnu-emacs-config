;;; early-init.el
;;
;;; Commentary:
;;
;;; Code:

;; Basic optimizations
(setq package-enable-at-startup nil
      inhibit-startup-message t
      frame-resize-pixelwise t
      package-native-compile t)

(setq gc-cons-threshold (* 128 1024 1024)
      gc-cons-percentage 0.6
      read-process-output-max (* 4 1024 1024))

(setq native-comp-async-report-warning-errors 'silent)

;; User initialization directory define
(defconst user-init-dir
  (cond ((boundp 'user-emacs-directory)
         user-emacs-directory)
        ((boundp 'user-init-directory)
         user-init-directory)
        (t "~/.emacs.d/"))
  "Root directory of GNU Emacs user configuration.

Directory which contain `early-init.el' and `init.el' files (and other elisp code).")

;; Add `lisp/' folder to load path
;;
;; This folder contains my configuration related emacs-lisp code
(add-to-list 'load-path (concat user-init-dir "lisp"))

;; `lsp-mode' use `plists' for deserialization
(setenv "LSP_USE_PLISTS" "true")

(provide 'early-init)

;;; early-init.el ends here
