;;; test.el --- ASd
;;
;;; Commentary:
;;
;;; Code:

(defmacro feature! (name &rest body)
  "Provide BODY by feature NAME."
  (declare (indent defun))
  (let ((fvn (intern (concat "enable-" (prin1-to-string (eval `(,@name))) "-feature"))))
    `(progn
       (defvar ,fvn
         "Enable this features.")

       (when ,fvn
         ,@body))))

(defmacro enable-feature! (&rest plist)
  "Enable PLIST features."
  nil)

(macroexpand
 '(feature! 'ide
            (eglot-ensure)))

(enable-feature! 'ide)

(format-time-string "%Y-%m-%d %H:%M" nil t)

(provide 'test)

;;; test.el ends here
