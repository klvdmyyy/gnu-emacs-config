;;; early-init.el --- Early Initialization file -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;; Code:

(let ((load-suffixes '(".elc" ".el")))
  (load (expand-file-name "lisp/bootstrap" user-emacs-directory)
        :no-error :no-message nil :must-suffix)

  (setq bootstrap-maximize-frame-at-startup t)

  (emacs-bootstrap))

(provide 'early-init)

;;; early-init.el ends here
