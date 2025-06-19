;;; klv-personal.el --- Personal related things -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;; Code:

(defun klv--personal-parser (l)
  (pcase l
    ((pred seq-empty-p))
    (`(:name ,name . ,next-l)
     (setq user-full-name name)
     (klv--personal-parser next-l))
    (`(:email ,email . ,next-l)
     (setq user-mail-address email)
     (klv--personal-parser next-l))
    (code (klv--personal-parser (cdr l)))
    ))

(defmacro this-person (&rest args)
  `(klv--personal-parser '(,@args)))

(provide 'klv-personal)

;;; klv-personal.el ends here
