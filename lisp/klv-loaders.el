;;; klv-loaders.el --- Features loader -*- lexical-binding: t; -*-
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
  `(klv--personal-parser ',args))

(defun klv-load-features (features-list)
  "Load features"
  (dolist (feature features-list)
    (load (concat user-emacs-directory "features/" (prin1-to-string feature) ".el")
          'noerror nil nil 'must-suffix)))

(defmacro load-features (&rest args)
  "Macro for syntax highlighting only.

Wrap over `klv-load-features'"
  `(klv-load-features ',args))

(defun klv-load-languages (languages-list)
  "Load languages"
  (dolist (language languages-list)
    (load (concat user-emacs-directory "languages/" (prin1-to-string language) ".el")
          'noerror nil nil 'must-suffix)))

(defmacro load-languages (&rest args)
  "Macro for syntax highlighting only.

Wrap over `klv-load-languages'"
  `(klv-load-languages ',args))

(provide 'klv-loaders)

;;; klv-loaders.el ends here
