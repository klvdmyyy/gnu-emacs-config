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
  (define-key zig-mode-map (kbd ";") (lambda () (interactive) (semicolon-after-pairs)))

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

(defconst before-pairs-list (list ?\( ?\[ ?\{))
(defconst after-pairs-list (list ?\) ?\] ?\}))

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

(defun semicolon-after-pairs ()
  (if (and (member (char-before (point)) after-pairs-list)
           (member (char-before (- (point) 1)) before-pairs-list))
      (progn (insert ";")
             (backward-char 2))
    (insert ";")))

(provide 'features/smartparens)

;;; smartparens.el ends here
