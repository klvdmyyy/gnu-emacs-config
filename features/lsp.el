;;; lsp.el
;;
;;; Commentary:
;;
;;; Code:

(use-package eglot
  :ensure nil
  :hook ((cc-mode . eglot-cc-ensure)
	 (go-mode . eglot-go-ensure)))

(defun eglot-cc-ensure ()
  (let ((language-server (executable-find "clangd")))
    (when language-server
      (eglot-ensure))))

(defun eglot-go-ensure ()
  (let ((language-server (executable-find "gopls")))
    (when language-server
      (eglot-ensure))))

(provide 'features/lsp)

;;; lsp.el ends here
