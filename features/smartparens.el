;;; smartparens.el
;;
;;; Commentary:
;;
;;; Code:

(use-package smartparens
  :hook ((prog-mode . smartparens-mode)
	 ;; Strict mode only in lisp/scheme
	 (emacs-lisp-mode . smartparens-strict-mode)
	 (lisp-mode . smartparens-strict-mode)
	 (common-lisp-mode . smartparens-strict-mode)
	 (scheme-mode . smartparens-strict-mode))
  :config
  ;; Ignore "'" and "`" in lisp/scheme mode
  (dolist (mode '(emacs-lisp-mode
		  lisp-mode
		  common-lisp-mode
		  scheme-mode))
    (sp-local-pair mode "'" nil :when '(sp-in-string-p))
    (sp-local-pair mode "`" nil :when '(sp-in-string-p)))

  (define-key prog-mode-map (kbd "RET") (lambda () (interactive) (indent-between-pairs)))

  (sp-with-modes '(prog-mode)
    (sp-local-pair "(" nil :post-handlers '(:add indent-between-pairs))
    (sp-local-pair "{" nil :post-handlers '(:add indent-between-pairs))
    (sp-local-pair "[" nil :post-handlers '(:add indent-between-pairs)))
  (show-paren-mode 1))

(use-package smartparens-config
  :ensure nil
  :after smartparens
  :bind (:map
	 smartparens-mode-map
	 ("M-s" . nil)
	 ("M-S" . sp-forward-slurp-sexp)))

(defun indent-between-pairs ()
  "Open a new brace or bracket expression, with relevant newlines and indent. "
  (if (and (member (char-before) before-pairs-list)
           (member (char-after) after-pairs-list))
      (progn (newline)
             (newline)
             (indent-according-to-mode)
             (forward-line -1)
             (indent-according-to-mode))
    (newline-and-indent)))

;;; smartparens.el ends here
