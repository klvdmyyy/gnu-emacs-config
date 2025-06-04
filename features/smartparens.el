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

  (show-paren-mode 1))

(use-package smartparens-config
  :ensure nil
  :after smartparens
  :bind (:map
	 smartparens-mode-map
	 ("M-s" . nil)
	 ("M-S" . sp-forward-slurp-sexp)))

(provide 'features/smartparens)

;;; smartparens.el ends here
