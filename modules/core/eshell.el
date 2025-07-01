;;; eshell.el --- EShell Core module -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;; Code:

(define-minor-mode eshell-mode-setup
  "Set up environment on `eshell-mode' invocation."
  :group 'eshell
  (if eshell-mode-setup
      (progn
        (if (and (boundp 'envrc-global-mode) envrc-global-mode)
	        (add-hook 'envrc-mode-hook (lambda () (setenv "PAGER" "")))
	      (setenv "PAGER" ""))
        (eshell/alias "l" "ls -al $1")
        (eshell/alias "e" "find-file $1")
        (eshell/alias "ee" "find-file-other-window $1")
        (eshell/alias "d" "dired $1")
        (eshell/alias "gd" "magit-diff-unstaged")
        ;; (local-unset-key 'eshell/clear)
        )))

(defun switch-to-prev-buffer-or-eshell (arg)
  (interactive "P")
  (if arg
      (eshell arg)			; or `project-eshell-or-eshell'
    (switch-to-buffer (other-buffer (current-buffer) 1))))

(defun project-eshell-or-eshell (&optional arg)
  (interactive "P")
  (if (project-current)
      (project-eshell)
    (eshell arg)))

(with-eval-after-load 'eshell
  (require 'em-alias)
  (require 'em-hist)
  
  (add-hook 'eshell-mode-hook 'eshell-mode-setup)

  (bind-key "s-e" 'switch-to-prev-buffer-or-eshell eshell-mode-map)
  (with-eval-after-load 'consult
    (bind-key "M-r" 'consult-history eshell-hist-mode-map)))

(bind-key "s-e" 'project-eshell-or-eshell)

;;; eshell.el ends here
