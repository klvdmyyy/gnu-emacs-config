;;; klv-cstructure.el --- Things for Configuration/Distribution Project Structure -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;; Project/Configuration Structure related things.
;;
;;; Code:

(defun klv--cstructure-parser (flist)
  (let ((next-flist (cdr (cdr flist)))
        (feature (prin1-to-string (car flist)))
        (enable? (car (cdr flist))))
    (if enable?
        (cons
         (substring feature 1 (length feature))
         (if (> (length flist) 2)
             (klv--cstructure-parser next-flist)
           '()))
      (if (> (length flist) 2)
          (klv--cstructure-parser next-flist)
        '()))))

(defun features! (&rest args)
  (let ((features (klv--cstructure-parser args)))
    (dolist (feature features)
      (load (concat user-init-dir "features/" feature ".el")))))

(defun languages! (&rest args)
  (let ((languages (klv--cstructure-parser args)))
    (dolist (language languages)
      (load (concat user-init-dir "languages/" language ".el")))))

(provide 'klv-cstructure)

;;; klv-cstructure.el ends here
