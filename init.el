;;; init.el -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;; Code:

;; MAYBE Move it to early initialization
(setq user-full-name "Dmitry Klementiev"
      user-mail-address "klementievd08@yandex.ru")

(load-feature core)

(unless (getenv "NO_EPM")
  (load-feature elpaca))

(use-package benchmark-init
  :demand t
  :config (benchmark-init/activate)
  :bind (("C-c b" . benchmark-init/show-durations-tabulated))
  :hook ((before-init . benchmark-init/activate) ; TODO move it to early-init maybe !?
         (after-init . benchmark-init/deactivate)))

(load-feature appearance)
(load-feature gcmh)
(load-feature completion)
(load-feature git)
(load-feature smartparens)
(load-feature productivity)
(load-feature esh)
(load-feature company)
(load-feature lsp)
(load-feature yasnippet)
(load-feature time)
(load-feature all-the-icons)

(load-language cc)
(load-language go)
(load-language zig)
(load-language proto)
(load-language docker)
(load-language yaml)
(load-language markdown)

(load-language nix)                     ; TODO Я думаю об использовании NixOS (или GNU Guix) в качестве основной ОС

;;; init.el ends here
