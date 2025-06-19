;;; klv-package.el --- Beatifull package management for GNU Emacs -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;;; Code:

(defvar klv-package-registry `(("emacs" . ,emacs-version))
  "Registry for klv package management system.

Contain all packages with some data. Also contain association with
GNU Emacs package: `(\"emacs\" . ,emacs-version)")

(cl-defmacro package! (name &key pin)
  "Install or Register package.

Work with elpaca.
If you are using elpaca, this macro just only register package
in `klv-package-registry'. Package installation provided by
the `use-package!' macro."
  (declare (indent defun))
  `())

(cl-defmacro use-package! (name &rest args)
  (declare (indent defun))
  `())

(provide 'klv-package)

;;; klv-package.el ends here
