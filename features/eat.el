;;; eat.el
;;
;;; Commentary:
;;
;;; Code:

(use-package eat
  :commands (eat eshell)
  :bind ("s-E" . eat)
  :hook ((eshell-load . eat-eshell-mode)
	 (eshell-load . eat-eshell-visual-command-mode))
  :custom
  (eat-line-input-ring-size 1024)
  (eat-kill-buffer-on-exit t)
  (eat-term-scrollback-size nil)
  (eat-enable-mouse t))

(provide 'features/eat)

;;; eat.el ends here
