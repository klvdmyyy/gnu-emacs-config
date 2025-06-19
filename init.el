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
;;
;;; Code:

(require 'klv-personal)
(require 'klv-cstructure)
(require 'klv-startup)
(require 'klv-font)

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

(features!
 ;; Core features of this configuration
 :core          t ; GNU Emacs core configuration
 :elpaca        t ; Elpaca - Asynchronous package manager (but not good enough)
 :appearance    t ; Themes, Fonts, Modeline and etc
 :gcmh          t ; Better GC for GNU Emacs
 :completion    t ; Vertico, Consult, Orderless, Marginalia and etc
 :git           t ; Magit, Git Gutter and etc
 :smartparens   t ; Smartest parens what i ever seen

 ;; Productivity and other things
 :hl-todo       t ; Highlight TODO, NEXT, DONE and other things
 :org           t ; It's my life
 :org-agenda    t ; it's also my life
 :org-roam      t ; It's my knowledge
 :org-roam-ui   t ; It's my beatifull knowledge

 ;; Development things
 :esh           t ; Eshell - the best shell
 :company       t ; Complete anything
 :lsp           t ; Language Servers
 :yasnippet     t ; Snippets
 :time          t ; Time (in modeline also)
 :all-the-icons t ; Beatifull icons
 )

(languages!
 ;; Load configuration files for specific languages
 :cc            t ; C/C++ languages (FIXME A lot of ISSUES !)
 :go            t ; Go language is Good language
 :zig           t ; Like C but sometimes better and simpler
 :nix           t ; Nix expressions for NixOS and nix package manager
 :web           t ; HTML/CSS
 )

;;; init.el ends here
