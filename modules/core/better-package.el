;;; better-package.el --- Package Management tweaks -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;; Known performance issues:
;; - TODO replace `package-activate-all' function which take ~0.02 seconds of startup time
;; - TODO make `package-installed-p' independent from any package manager
;;
;; Maybe move to one of the following package managers:
;; - Elpaca
;; - Straight.el
;; - Emacs Borg
;;
;;; Code:

;; Without these variables `use-package' cause issues
(setq-default use-package-always-ensure t
              use-package-always-defer t
              package-native-compile t
              package-install-upgrade-built-in t)

;; FIXME With this advice I can't upgrade any package (also built-in)
;; ;; Replace `package-installed-p'
;; ;;
;; ;; Now `package-installed-p' is independent from any package
;; ;; manager.
;; ;;
;; ;; This tweak isn't neccessary but I use it while it doesn't cause
;; ;; any issues. Sometimes it can help me :>
;; (define-advice package-installed-p
;;     (:override (name &optional min-version))
;;   (if (locate-library (prin1-to-string name))
;;       t
;;     (featurep name)))

;; Add packages to `load-path' before `package-installed-p' running
(define-advice package-installed-p
    (:before (&rest _))
  ;; It's not perfect solution.
  ;;
  ;; I think we can use better optimization.
  ;;
  ;; Also we can use other package manager with
  ;; better code base for optimization like this one.
  (when (not package--activated)
    (package-activate-all)))

;; ;; Ensure that MELPA is also used
(define-advice package-refresh-contents
    (:before (&rest _))
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/")))

;; Always set `:ensure' to `t' in `use-package'
(define-advice use-package-core
    (:around (orig-fun name args))
  (apply orig-fun
         name
         (append
          '(:ensure t)
          args)))

;;; better-package.el ends here
