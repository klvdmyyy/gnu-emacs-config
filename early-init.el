;;; early-init.el
;;
;;; Commentary:
;;
;;; Code:

;; Some early optimizations
;;
;; [IMPORTANT] You really shouldn't change GC settings
;; all just set it to defaults after initialization because
;; it can cause performance issues laterly.
;;
;; My after-init setup of GC configured in GCMH package which
;; used for better GC
(setq gc-cons-threshold (* 128 1024 1024 1024)
      gc-cons-percentage 0.6
      read-process-output-max (* 4 1024 1024 1024)
      package-enable-at-startup nil
      inhibit-startup-message t
      frame-resize-pixelwise t
      package-native-compile t
      native-comp-async-report-warnings-errors 'silent)

(defconst user-init-dir (file-name-directory user-init-file)
  "Important variable for this configuration.

You can change it or rewrite but you can't remove because
it breaks all my GNU Emacs configuration

I use it for config-specific declaration e.g I can change
it if i need in my `early-init-file'.")

;; Setup lisp/ directory with source code of my distribution
;;
;; Sometimes I write interesting things in Emacs Lisp which placed
;; only in this directory.
;;
;; Other packages, languages and it configuaration placed in
;; features/, languages/ and etc
(add-to-list 'load-path (concat user-init-dir "lisp"))

;; `lsp-mode' use `plists' for deserialization
;;
;; It's just a so simple optimization for lsp-mode
;; which I use as main LSP provider
;;
;; Why not Eglot !? Idk, I just use lsp-mode
;;
;; Eglot is also good thing which you can use
(setenv "LSP_USE_PLISTS" "true")

;; All shit in `early-init-file' is pre-configured things which
;; you shouldn't touch
;;
;; For booting my GNU Emacs I using `klv-boot' package (but I not write it yet :>)

;; (require 'klv-boot)

(provide 'early-init)

;;; early-init.el ends here
