;;; lsp.el
;;
;;; Commentary:
;;
;;; Code:

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :init
  (setq lsp-keymap-prefix "C-c l")
  ;; or `lsp-deferred'
  :hook ((c-mode . lsp)
         (c++-mode . lsp)
         (go-mode . lsp)
         (lsp-mode . lsp-enable-which-key-integration))
  :custom
  (lsp-log-io nil)
  (lsp-idle-delay 0.500))

(use-package lsp-ui
  :disabled t
  :after lsp-mode
  :hook (lsp-mode . lsp-ui-mode))

(use-package eglot
  :disabled t
  :ensure nil
  :hook ((cc-mode . eglot-ensure)
         (go-mode . eglot-ensure)))

(provide 'features/lsp)

;;; lsp.el ends here
