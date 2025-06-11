;;; smartparens.el
;;
;;; Commentary:
;;
;;; Code:

(use-package smartparens
  :bind (:map
         prog-mode-map
         ("RET" . (cmd! (indent-between-pairs))))
  :hook ((prog-mode . smartparens-mode)
	     ;; Strict mode only in lisp/scheme
	     (emacs-lisp-mode . smartparens-strict-mode)
	     (lisp-mode . smartparens-strict-mode)
	     (common-lisp-mode . smartparens-strict-mode)
	     (scheme-mode . smartparens-strict-mode))
  :config
  (sp-with-modes '(emacs-lisp-mode
                   lisp-mode
                   common-lisp-mode
                   scheme-mode)
    (sp-local-pair "'" nil)
    (sp-local-pair "`" "'" :when '(sp-in-comment-p))
    (sp-local-pair "`" "'" :when '(:add sp-in-string-p)))
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
