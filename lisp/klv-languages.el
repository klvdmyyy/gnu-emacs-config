;;; klv-languages.el --- Languages loader -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;; Code:

(provide 'klv-languages)

(defun klv-load-languages (languages-list)
  "Load languages"
  (dolist (language languages-list)
    (load (concat user-init-dir "languages/" (prin1-to-string language) ".el"))))

(defmacro load-languages (&rest args)
  "Macro for syntax highlighting only.

Wrap over `klv-load-languages'"
  `(klv-load-languages ',args))

;;; klv-languages.el ends here
