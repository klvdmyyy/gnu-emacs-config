;;; early-init.el --- Early Initialization file -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;; Code:

(let ((load-suffixes '(".elc" ".el")))
  (load (expand-file-name "lisp/bootstrap" user-emacs-directory)
        :no-error :no-message nil :must-suffix)

  (load (expand-file-name "early-config" user-emacs-directory)
		:no-error :no-message nil :must-suffix)

  (emacs-bootstrap))

(provide 'early-init)

;;; early-init.el ends here
