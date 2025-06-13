;;; telega.el --- Telegram Client in GNU Emacs -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;;; Code:

(use-package telega
  :commands (telega)
  :hook ((telega-chat-mode . toggle-input-method))
  :config
  (setq telega-company-backends (cons 'company-yasnippet telega-company-backends)))

;;; telega.el ends here
