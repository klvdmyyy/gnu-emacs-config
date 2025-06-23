;;; smartparens.el --- Smartparens configuration -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;; Code:

(defconst default-pairs-list
  '((?\( . ?\))
    (?\[ . ?\])
    (?\{ . ?\}))
  "List of default pairs")

(defun open-pair-p (char)
  "Return t if CHAR is opening pair"
  (member char (mapcar (lambda (pairs) (car pairs)) default-pairs-list)))

(defun close-pair-p (char)
  "Return t if CHAR is closing pair"
  (member char (mapcar (lambda (pairs) (cdr pairs)) default-pairs-list)))

(defun indent-between-pairs ()
  "Open a new brace or bracket expression, with relevant newlines and indent."
  (interactive)
  (if (and (open-pair-p (char-before))
           (close-pair-p (char-after)))
      (progn (newline)
             (newline)
             (indent-according-to-mode)
             (forward-line -1)
             (indent-according-to-mode))
    (newline-and-indent)))

(use-package smartparens
  :hook ((prog-mode . show-paren-mode)
         (prog-mode . smartparens-mode)
	     ;; Strict mode only in lisp/scheme
	     (emacs-lisp-mode . smartparens-strict-mode)
	     (lisp-mode . smartparens-strict-mode)
	     (common-lisp-mode . smartparens-strict-mode)
	     (scheme-mode . smartparens-strict-mode))
  :init
  (define-key prog-mode-map (kbd "RET") 'indent-between-pairs)
  
  :config
  (sp-with-modes '(emacs-lisp-mode
                   lisp-mode
                   common-lisp-mode
                   scheme-mode)
    (sp-local-pair "'" nil :when '(sp-in-string-p sp-in-comment-p) :actions '(forward-char))
    (sp-local-pair "`" "'" :when '(sp-in-string-p sp-in-comment-p))))

(use-package smartparens-config
  :ensure nil
  :after smartparens
  :bind (:map
	     smartparens-mode-map
	     ("M-s" . nil)
         ("M-DEL" . sp-backward-unwrap-sexp)
         ("C-<left>" . sp-forward-barf-sexp)
	     ("C-<right>" . sp-forward-slurp-sexp)))

;;; smartparens.el ends here
