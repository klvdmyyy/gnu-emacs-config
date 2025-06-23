;;; init.el --- Initialization -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;; Code:

;; TODO

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
   org
   org-agenda
   org-roam
   org-roam-ui
   elfeed)

  (load-languages
   ;; cc
   go
   ;; rust
   ;; zig
   ;; nix
   web
   docker
   markdown
   proto
   yaml
   ;; toml
   ))

(provide 'init)

;;; init.el ends here
