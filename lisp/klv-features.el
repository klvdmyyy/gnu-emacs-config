;;; klv-features.el --- Features loader -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;; Code:

(provide 'klv-features)

(defun klv-load-features (features-list)
  "Load features"
  (dolist (feature features-list)
    (load (concat user-init-dir "features/" (prin1-to-string feature) ".el"))))

(defmacro load-features (&rest args)
  "Macro for syntax highlighting only.

Wrap over `klv-load-features'"
  `(klv-load-features ',args))

;;; klv-features.el ends here
