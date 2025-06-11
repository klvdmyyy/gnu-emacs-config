;;; klv.el -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;; Code:

(defconst before-pairs-list (list ?\( ?\[ ?\{)
  "Opening pairs list constant")
(defconst after-pairs-list (list ?\) ?\] ?\})
  "Closing pairs list constant")

(defvar is-first-startup nil
  "`t' --- First startup of GNU Emacs
`nil' --- Not first startup of GNU Emacs")

(defun lock-startup (&optional lock-file)
  (let ((lock-file (or lock-file (concat user-init-dir ".startup-lock"))))
    (unless (file-exists-p lock-file)
      (dired-create-empty-file lock-file)
      (setq is-first-startup t))))

(defmacro cmd! (&rest args)
  `(lambda () (interactive) ,@args))

(provide 'klv)

;;; klv.el ends here
