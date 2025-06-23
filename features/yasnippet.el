;;; yasnippet.el --- Snippets for GNU Emcas -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;; Code:

(use-package yasnippet
  :init
  (setq yas-snippet-dirs
        (seq-map #'file-truename
                 (list (concat user-emacs-directory "snippets"))))
  :hook ((prog-mode . yas-minor-mode)
         (org-mode . yas-minor-mode))
  :config
  (yas-reload-all 1))

;;; yasnippet.el ends here
