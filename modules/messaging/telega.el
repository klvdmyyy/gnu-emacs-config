;;; telega.el --- Telegram Messaging module -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;; TODO: Implement quick reaction for telega.el
;; TODO: Add yasnippet and company-mode for telega.el
;; FIXME: Can't view video message in telega.el
;;
;;; Code:

(defvar telega-quick-reaction nil
  "Quick reaction for telega.el")

(defun telega-message-add-quick-reaction (&rest _)
  nil)

(use-package telega
  :commands (telega)
  :config
  (telega-notifications-mode 0))

;;; telega.el ends here
