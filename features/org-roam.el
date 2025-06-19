;;; org-roam.el --- Org Roam configuration -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;;; Code:


(use-package org-roam
  ;; MAYBE Use `after-init-hook'
  ;; FIXME With `after-init-hook' it may cause issues maybe
  :hook ((emacs-startup . org-roam-db-autosync-mode))
  :bind (:map
         org-mode-map
         ("C-c r r" . org-roam-ref-add)
         ("C-c r R" . org-roam-ref-remove)
         ("C-c r f" . org-roam-ref-find)
         ("C-c r t" . org-roam-tag-add)
         ("C-c r T" . org-roam-tag-remove)
         ("C-c r a" . org-roam-alias-add)
         ("C-c r A" . org-roam-alias-remove)
         ;; TODO See https://git.sr.ht/~abcdw/rde/tree/master/item/src/rde/features/emacs-xyz.scm#L4883
         ;; ("C-c r O" . rde-org-roam-open-ref)
         :map
         mode-specific-map
         ("n t" . org-roam-dailies-capture-today)
         ("n n" . org-roam-buffer-toggle)
         ("n f" . org-roam-node-find)
         ("n i" . org-roam-node-insert)
         ("n r" . org-roam-ref-find)
         ("n C" . org-roam-capture))
  :custom
  (org-roam-directory klv/org-roam-directory)
  (org-roam-db-location klv/org-roam-db-location)
  (org-roam-db-update-on-save t)

  ;; Display template (display tags in vertico)
  (org-roam-node-display-template
   (concat "${title:*} "
           (propertize "${tags:30}" 'face 'org-tag)))

  ;; Dailies directory
  (org-roam-dailies-directory "daily/")
  
  ;; Dailies templates
  (org-roam-dailies-capture-templates klv/roam-dailies-capture-templates)

  ;; Templates
  (org-roam-capture-templates klv/roam-capture-templates)
  :init
  (setq org-roam-v2-ack t)
  (make-directory klv/org-roam-directory t)
  (dolist (dir klv/org-roam-subdirectories)
    (make-directory (concat klv/org-roam-directory "/" dir) t)))

;;; org-roam.el ends here
