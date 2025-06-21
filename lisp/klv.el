;;; klv.el -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;; Code:

;; Specific constants

(defconst before-pairs-list (list ?\( ?\[ ?\{)
  "Opening pairs list constant")
(defconst after-pairs-list (list ?\) ?\] ?\})
  "Closing pairs list constant")

;; Functions

(defun which-linux-distribution ()
  "Maybe useful for future development of my GNU Emacs configuration.

I don't use it now.

[IMPORTANT] You need a `lsb_release' executable. Without it function doesn't
work and just return Unknown."
  (interactive)
  (when (eq system-type 'gnu/linux)
    (let* ((lsb_release (executable-find "lsb_release"))
           (cmd (if lsb_release (concat lsb_release " -sd") nil))
           (result (if cmd (shell-command-to-string cmd) "Unknown")))
      (message result))))

;; Macroses

(defmacro cmd! (&rest body)
  (declare (doc-string 1))
  `(lambda (&rest _) (interactive) ,@body))

(provide 'klv)

;;; klv.el ends here
