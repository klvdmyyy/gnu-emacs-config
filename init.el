;;; init.el --- Initialization -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;; Code:

(load-module 'core 'better-package)

(load-module 'core 'emacs)
(load-module 'core 'dired)
(load-module 'core 'gcmh)
(load-module 'core 'eshell)
(load-module 'core 'eat)

(load-module 'emacs-lisp 'highlight-defined)
(load-module 'emacs-lisp 'elisp-autofmt)
(load-module 'emacs-lisp 'elsa)

(load-module 'appearance 'kaolin)
(load-module 'appearance 'all-the-icons)
(load-module 'appearance 'doom-modeline)
(load-module 'appearance 'rainbow)
(load-module 'appearance 'hl-todo)
(load-module 'appearance 'visual-fill-column)
(load-module 'appearance 'indent-bars)
(load-module 'appearance 'golden-ratio)

(load-module 'completion 'vertico)
(load-module 'completion 'orderless)
(load-module 'completion 'marginalia)
(load-module 'completion 'consult)
(load-module 'completion 'company)

(load-module 'navigation 'avy)
(load-module 'navigation 'ace)

(load-module 'org 'core)
(load-module 'org 'agenda)
(load-module 'org 'roam)
(load-module 'org 'roam-ui)

;; TODO: See more org-related packages in Doom Emacs modules
;; TODO: Implement following modules:
(load-module 'org 'brain)
(load-module 'org 'noter)
(load-module 'org 'ql)

(load-module 'programming 'smartparens)
(load-module 'programming 'yasnippet)
(load-module 'programming 'tree-sitter)
(load-module 'programming 'git)
(load-module 'programming 'leetcode)

(load-module 'tools 'google-translate)
(load-module 'tools 'docker)
(load-module 'tools 'kubernetes)

(load-module 'languages 'c) ; No C++ language. I just don't use it now
(load-module 'languages 'dockerfile)
(load-module 'languages 'protobuf)
(load-module 'languages 'yaml)
(load-module 'languages 'python)
(load-module 'languages 'go)

;; TODO: Move it from languages. Make markdown comparable with org-mode in my configuration
(load-module 'languages 'markdown)

(provide 'init)

;;; init.el ends here
