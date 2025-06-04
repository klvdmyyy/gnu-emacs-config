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

  ;; Fix indentation between pairs
  (dolist (char '("{" "(" "["))
    (sp-local-pair 'prog-mode char nil :post-handlers '((indent-between-pair "RET"))))

  (show-paren-mode 1))

(use-package smartparens-config
  :ensure nil
  :after smartparens
  :bind (:map
	 smartparens-mode-map
	 ("M-s" . nil)
	 ("M-S" . sp-forward-slurp-sexp)))

(defun indent-between-pair (&rest _ignored)
  "Insert indentation between pairs. Used with smartparens"
  (newline)
  (indent-according-to-mode)
  (forward-line -1)
  (indent-according-to-mode))

(provide 'features/smartparens)

;;; smartparens.el ends here
