;;; appearance.el
;;
;;; Commentary:
;;
;;; Code:

(use-font "Fira Code"
  :github "ryanoasis/nerd-fonts"
  :release "v3.1.1/FiraCode.zip")

(use-package zenburn-theme
  :hook
  (emacs-startup . (lambda () (load-theme 'zenburn t nil))))

(use-package hl-line
  :ensure nil
  :hook ((prog-mode . hl-line-mode)
         (dired-mode . hl-line-mode)
         (benchmark-init/tabulated-mode . hl-line-mode)))

(use-package display-line-numbers
  :ensure nil
  :hook ((prog-mode . display-line-numbers-mode)))

(use-package menu-bar
  :ensure nil
  :config (menu-bar-mode 0))

(use-package tool-bar
  :ensure nil
  :config (tool-bar-mode 0))

(use-package scroll-bar
  :ensure nil
  :config (scroll-bar-mode 0))

(use-package fringe
  :ensure nil
  :config (fringe-mode '(4 . 4)))

(use-package tab-bar
  :disabled t                           ; MAYBE Setup `tab-bar-mode'
  :ensure nil
  :hook (after-init . tab-bar-mode))

;;; appearance.el ends here
