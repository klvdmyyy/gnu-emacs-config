;;; smartparens.el --- Smartparens Editing module -*- lexical-binding: t; -*-
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

(with-eval-after-load 'smartparens
  (require 'smartparens-config)
  (bind-key "M-s" nil smartparens-mode-map)
  (bind-key "M-DEL" 'sp-backward-unwrap-sexp smartparens-mode-map)
  (bind-key "C-<left>" 'sp-forward-barf-sexp smartparens-mode-map)
  (bind-key "C-<right>" 'sp-forward-slurp-sexp smartparens-mode-map))

;;; smartparens.el ends here
