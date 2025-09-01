;;; early-init.el --- Early Initialization file -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;; Code:

(setq-default gc-cons-threshold most-positive-fixnum)
(setq-default load-prefer-newer t)
(setq-default native-comp-jit-compilation t)
(setq-default native-comp-deferred-compilation native-comp-jit-compilation)
(setq-default package-native-compile t)

(let* ((load-suffixes '(".elc" ".el"))
       (file-name-handler-alist nil))
  (load (expand-file-name "lisp/bootstrap" user-emacs-directory)
        :no-error :no-message nil :must-suffix)

  (load (expand-file-name "early-config" user-emacs-directory)
        :no-error :no-message nil :must-suffix)

  (emacs-bootstrap))

(provide 'early-init)

;;; early-init.el ends here
