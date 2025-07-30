;;; lazydo.el --- Lazy Configuring. -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;; lazydo - packages lazy loading
;;
;; TODO: Part/Partition of packages (like `hydra-definitions').
;;
;;; Code:

(defmacro after! (packages &rest body)
  "Better processing of PACKAGES and BODY than `with-eval-after-load'."
  (declare (indent defun))
  (let ((packages (eval `(,@packages))))
    (cond ((not packages)
           `(progn
              ,@body))
          ((symbolp packages)
           `(with-eval-after-load ',packages
              ,@body))
          ((listp packages)
           `(with-eval-after-load ',(car packages)
              (after! ',(cdr packages)
                ,@body))))))

(defmacro hook! (hook function &rest args)
  "TODO: Documentation of this thing."
  (let* ((hook (eval `(,@hook)))
	 (function (eval `(,@function)))
	 ;; Simple args parsing
	 (lazy-load (if (member :lazy-load args) (plist-get args :lazy-load) nil))
	 (package (if (member :package args) (plist-get args :package) nil)))
    (cond ((and (commandp hook) lazy-load)
	   (let ((fnname (intern (concat "load-" (prin1-to-string function)))))
	     `(define-advice ,hook
		  (:before (&rest _) ,fnname)
		(advice-remove ',hook #',(intern (concat (prin1-to-string hook)
							 "@" (prin1-to-string fnname))))
		(require ',function))))

	  ((boundp hook)
	   (if lazy-load
	       (let ((fnname (intern (concat "load-" (prin1-to-string function)
					     "-on-" (prin1-to-string hook)))))
		 `(progn
		    ,(unless (fboundp fnname)
		       `(defun ,fnname (&rest _)
			  (interactive)
			  (require ',function)
			  (remove-hook ',hook #',fnname)))
		    (add-hook ',hook #',fnname)))
	     `(progn
		,(if package
		     (autoload ',function ,(prin1-to-string package))
		   '())
		(add-hook ',hook ',function))))

	  (t (error "Can't determine hook in `hook!' function: `%s'" hook)))))

(defun autoload! (package &rest autoloads)
  "Generate AUTOLOADS from PACKAGE by repeating `autoload' function."
  (declare (indent defun))
  (dolist (symbol autoloads)
    (cond ((listp symbol)
           (pcase-let* ((`(,symbol ,doc-string ,interactive?) symbol))
             (autoload symbol package doc-string interactive?)))
          ((symbolp symbol)
           (autoload symbol package))
          (t (error "Can't determine symbol type for autoloading with `autoload!' macro/function: %S" symbol)))))

(provide 'lazydo)

;;; lazydo.el ends here
