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

(use-package visual-fill-column
  :hook (org-mode
         markdown-mode
         Info-mode
         eww-mode
         elfeed-show-mode)
  :custom
  (visual-fill-column-width 120))

(use-package spacemacs-theme
  :demand (not (daemonp))
  :hook
  (after-make-frame-functions
   . (lambda (frame)
       (with-selected-frame frame
         (load-theme 'spacemacs-dark t nil))))
  :config
  (when (not (daemonp))
    (load-theme 'spacemacs-dark t nil)))

(use-package taoline
  :ensure (taoline
           :host github
           :repo "11111000000/taoline"
           :branch "master")
  :hook emacs-startup
  :custom
  (taoline-segments
   '((:left taoline-segment-icon-and-buffer taoline-segment-git-branch)
     (:center taoline-segment-echo-message)
     (:right taoline-segment-project-name taoline-segment-time))))

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

;;; appearance.el
