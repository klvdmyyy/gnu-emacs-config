;;; org-roam-ui.el --- Org Roam UI configuration -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;;; Code:

;; TODO Setup `org-roam-ui'
(use-package simple-httpd
  :demand t)
(use-package websocket
  :demand t)

(use-package org-roam-ui
  :after (org-roam
          simple-http
          websocket
          f)
  :commands (org-roam-ui-open
             org-roam-ui-mode))


;;; org-roam-ui.el ends here
