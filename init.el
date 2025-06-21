;;; init.el -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;; Initialization file.
;;
;; This file loads all external/internal:
;; - Modules
;; - Features
;; - Extensions/Plugins
;; - Specific languages
;;
;; TODO Early initialization of package management for my configuration compilation (i can't use compilation related packages)
;; TODO Read "minimal-emacs.d" configuration from "jamescherti" (take inspiration for optimizations. Now my emacs loaded in 0.3-0.5s)
;; TODO Startup benchmarking ...
;;
;;; Code:

(require 'klv-personal)
(require 'klv-startup)
(require 'klv-font)
(require 'klv-features)
(require 'klv-languages)
(require 'klv-roam)

;; TODO Move all code from `klv'. Now non-requiring it breaks configuration
(require 'klv)

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
 elpaca
 appearance
 gcmh
 completion
 git
 smartparens
 hl-todo
 org
 org-agenda
 org-roam
 org-roam-ui
 
 esh
 company
 lsp
 yasnippet
 time
 all-the-icons

 elfeed
 )

(load-languages
 cc
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
 )

;;; init.el ends here
