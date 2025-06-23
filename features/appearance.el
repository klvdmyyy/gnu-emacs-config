;;; appearance.el --- Make GNU Emacs looks better -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;; Themes for GNU Emacs:
;; - Modus themes --- Default themes in latest GNU Emacs version which so cute and configurable but I
;; can't spend a time for customization.
;; - Zenburn - So simple theme for GNU Emacs
;; - Spacemacs - Futuristic things which I like :>
;; - Doom Themes - just all themes in one package
;;
;;; Code:

(use-font "Fira Code"
  :github "ryanoasis/nerd-fonts"
  :release "v3.1.1/FiraCode.zip")

(use-package spacemacs-theme
  :hook (emacs-startup
         . (lambda ()
             (load-theme 'spacemacs-dark t nil))))

(use-package doom-modeline
  :hook emacs-startup)

(use-package hl-line
  :ensure nil
  :hook ((prog-mode . hl-line-mode)
         (dired-mode . hl-line-mode)))

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
  :disabled t                           ; NOTE Setup `tab-bar-mode' !?
  :ensure nil
  :hook (after-init . tab-bar-mode))

;;; appearance.el
