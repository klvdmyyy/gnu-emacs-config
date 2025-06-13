;;; company.el
;;
;;; Commentary:
;;
;;; Code:

;; TEMP Fix for telega
(defun newline-maybe-telega ()
  (interactive)
  (if (eq major-mode 'telega-chat-mode)
      (telega-chatbuf-newline-or-input-send)
    (indent-between-pairs)))

(use-package company
  :hook ((prog-mode . company-mode)
         (telega-chat-mode . company-mode))
  :bind (("<TAB>" . company-indent-or-complete-common)
	     :map company-active-map
         ([remap company-complete-selection]
          . newline-maybe-telega)
	     ([remap company-complete-common-or-cycle]
	      . company-complete-selection)
	     ([backtab] . nil))
  :config
  (setq company-backends
        (seq-map
         (lambda (b)
           (cond
            ((listp b) (cons 'company-yasnippet b))
            ((eq 'company-capf b) '(company-capf :with company-yasnippet))
            (t b)))
         company-backends))
  :custom
  (company-dabbrev-downcase 0)
  (company-idle-delay 0.001) ;; or 0.001/0.01 (default is 0.2)
  (company-minimum-prefix-length 2)
  (ess-r--no-company-meta t))

;; Don't use it
(use-package company-box
  :disabled t
  :after company
  :hook (company-mode . company-box-mode))

;;; company.el ends here
