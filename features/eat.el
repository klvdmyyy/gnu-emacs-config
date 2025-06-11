;;; eat.el
;;
;;; Commentary:
;;
;;; Code:

(use-package eat
  :commands (eat eshell)
  :bind ("s-E" . eat-project-or-eat)
  :hook ((eshell-load . eat-eshell-mode)
	 (eshell-load . eat-eshell-visual-command-mode))
  :custom
  (eat-shell (let ((zsh (executable-find "zsh")))
	       (if zsh zsh (executable-find "bash"))))
  (eat-line-input-ring-size 1024)
  (eat-kill-buffer-on-exit t)
  (eat-term-scrollback-size nil)
  (eat-enable-mouse t))

(defun eat-project-or-eat (&optional arg)
  (interactive "P")
  (if (project-current)
      (eat-project arg)
    (eat)))

;;; eat.el ends here
