;;; telega.el --- Telegram Client in GNU Emacs -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;;; Code:

(use-package telega
  :commands (telega)
  :hook ((telega-chat-mode . toggle-input-method))
  :bind (:map
         telega-chat-mode-map
         ("C-s" . telega-chatbuf-filter-search)
         ("M-g <" . telega-chatbuf-history-beginning)
         ("M-g >" . telega-chatbuf-read-all))
  :config
  (setq telega-company-backends (cons 'company-yasnippet telega-company-backends)))

;;; telega.el ends here
