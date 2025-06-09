;;; company.el
;;
;;; Commentary:
;;
;;; Code:

(use-package company
  :hook (prog-mode . company-mode)
  :bind (("<TAB>" . company-indent-or-complete-common)
	 :map company-active-map
	 ([remap company-complete-common-or-cycle]
	  . company-complete-selection)
	 ([backtab] . nil))
  :config
  (setq company-dabbrev-downcase 0)
  (setq company-idle-delay 0.0) ;; or 0.001/0.01 (default is 0.2)
  (setq company-minimum-prefix-length 2)
  (setq ess-r--no-company-meta t))

;; Don't use it
(use-package company-box
  :disabled t
  :after company
  :hook (company-mode . company-box-mode))

(provide 'features/company)

;;; company.el ends here
