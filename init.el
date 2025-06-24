;;; init.el --- Initialization -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;; Code:

(let ((load-suffixes '(".elc" ".el"))
      (file-name-handler-alist nil))
  (this-person
   :name "Dmitry Klementiev"
   :email "klementievd08@yandex.ru"

   ;; Unnecessary :>
   :github "klvdmyyy"
   :gitlab "klvdmyyy"
   :sourcehut "klvdmyyy"
   :telegram "klvdmyyy"
   :instagram "klvdmyyy")

  (load-features
   core
   dired
   elpaca
   gcmh
   appearance
   esh
   completion
   all-the-icons
   company
   yasnippet
   git
   smartparens
   hl-todo
   tree-sitter
   org
   org-agenda
   org-roam
   org-roam-ui
   elfeed)

  (load-languages
   elisp
   cc
   go
   docker
   markdown
   proto
   yaml))

(provide 'init)

;;; init.el ends here
