;;; lsp.el
;;
;;; Commentary:
;;
;;; Code:

(use-package eglot
  :ensure nil
  :hook ((cc-mode . eglot-ensure)
         (go-mode . eglot-ensure)))

(defun eglot-cc-ensure ()
  (let ((language-server (executable-find "clangd")))
    (if language-server
        (eglot-ensure)
      (message "No language server for C/C++"))))

(defun eglot-go-ensure ()
  (let ((language-server (executable-find "gopls")))
    (if language-server
        (eglot-ensure)
      (message "No language server for Golang"))))

(provide 'features/lsp)

;;; lsp.el ends here
