;;; eshell.el
;;
;;; Commentary:
;;
;;; Code:

(use-package eshell
  :ensure nil
  :commands (eshell)
  :hook ((eshell-mode . klvdmyyy-eshell-mode-setup))
  :bind (("s-e" . project-eshell-or-eshell)
	     :map eshell-mode-map
	     ("s-e" . switch-to-prev-buffer-or-eshell)
         :map eshell-hist-mode-map
         ("M-r" . consult-history))
  :init
  (require 'em-alias)
  (require 'em-hist)
  ;; (require 'project)

  :config
  (let ((eshell-cache (concat (or (getenv "XDG_CACHE_HOME") "~/.cache")
			      "/emacs/eshell/")))
    (setq eshell-aliases-file (concat eshell-cache "alias"))
    (setq eshell-history-file-name (concat eshell-cache "history"))
    (setq eshell-last-dir-ring-file-name
	  (concat eshell-cache "lastdir"))))

(defgroup klvdmyyy-eshell nil
  "Eshell customizations for a better integration with GNU Emacs config.

Heavily inspired from GNU Guix RDE's `feature-emacs-eshell'"
  :group 'klvdmyyy)

(define-minor-mode klvdmyyy-eshell-mode-setup
  "Set up environment on `eshell-mode' invocation."
  :group 'klvdmyyy
  (if klvdmyyy-eshell-mode-setup
      (progn
	(if (and (boundp 'envrc-global-mode) envrc-global-mode)
	    (add-hook 'envrc-mode-hook (lambda () (setenv "PAGER" "")))
	  (setenv "PAGER" ""))

	(eshell/alias "l" "ls -al $1")
	(eshell/alias "e" "find-file $1")
	(eshell/alias "ee" "find-file-other-window $1")
	(eshell/alias "d" "dired $1")
	(eshell/alias "gd" "magit-diff-unstaged"))
    (local-unset-key 'eshell/clear)))

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

(provide 'features/esh)

;;; eshell.el ends here
