;;; init.el -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;; Code:

(setq user-full-name "Dmitry Klementiev"
      user-mail-address "klementievd08@yandex.ru")

(load-feature core)

(unless (getenv "NO_EPM")
  (load-feature elpaca))

(use-package benchmark-init
  :demand t
  :init (benchmark-init/activate)
  :bind (("C-c b" . benchmark-init/show-durations-tabulated))
  :hook ((after-init . benchmark-init/deactivate)
         (benchmark-init/tabulated-mode . hl-line-mode)))

(load-feature appearance)
(load-feature gcmh)
(load-feature completion)
(load-feature git)
(load-feature smartparens)
(load-feature productivity)
(load-feature esh)
(load-feature eat)
(load-feature company)
(load-feature lsp)
(load-feature yasnippet)
;; (load-feature display-wttr)
(load-feature time)
(load-feature all-the-icons)

(load-language cc)
(load-language go)
(load-language zig)
(load-language proto)
(load-language docker)
(load-language yaml)
(load-language markdown)

;;; init.el ends here
