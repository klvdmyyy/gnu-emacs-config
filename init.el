;;; init.el
;;
;;; Commentary:
;;
;;; Code:

;; (setq safe-local-variable-values
;;       '((dired-omit-files . "auto-save-list\\'")))

;; Core

(require! 'features/core)

;; Package manager

(require! 'features/elpaca)

;; Benchmark

(use-package benchmark-init
  :demand t
  :init (benchmark-init/activate)
  :bind (("C-c b" . benchmark-init/show-durations-tabulated))
  :hook ((after-init . benchmark-init/deactivate)
         (benchmark-init/tabulated-mode . hl-line-mode)))

;; Other features

(require! 'features/appearance)

(require! 'features/gcmh)

(require! 'features/completion)

(require! 'features/git)

(require! 'features/smartparens)

(require! 'features/productivity)

(require! 'features/esh)
(require! 'features/eat)

(require! 'features/company)

(require! 'features/lsp)

(require! 'features/display-wttr)
(require! 'features/klvdmyyy-time)

(require! 'features/all-the-icons)

;; Languages

(require! 'languages/cc)
(require! 'languages/go)
(require! 'languages/zig)
(require! 'languages/proto)
(require! 'languages/docker)
(require! 'languages/yaml)
(require! 'languages/markdown)

;;; init.el ends here
