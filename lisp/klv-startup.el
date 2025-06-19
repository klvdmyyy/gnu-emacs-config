;;; klv-startup.el --- -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;; Code:

(defvar is-first-startup nil
  "`t' --- First startup of GNU Emacs
`nil' --- Not first startup of GNU Emacs")

(defun lock-startup (&optional lock-file)
  (let ((lock-file (or lock-file (concat user-init-dir ".startup-lock"))))
    (unless (file-exists-p lock-file)
      (dired-create-empty-file lock-file)
      (setq is-first-startup t))))

(provide 'klv-startup)

;;; klv-startup.el ends here
