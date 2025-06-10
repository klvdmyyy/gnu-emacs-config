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
  :hook ((c-mode . lsp-deferred)
         (c++-mode . lsp-deferred)
         (go-mode . lsp-deferred)
         (zig-mode . lsp-deferred)
         (python-mode . lsp-deferred)
         (lsp-mode . lsp-enable-which-key-integration))
  :custom
  (lsp-log-io nil)
  (lsp-idle-delay 0.500))

(use-package lsp-ui
  :disabled t
  :after lsp-mode
  :hook (lsp-mode . lsp-ui-mode))

(provide 'features/lsp)

;;; lsp.el ends here
