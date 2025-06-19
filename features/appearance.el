;;; appearance.el
;;
;;; Commentary:
;;
;;; Code:

(defun load-face-attributes ()
  (let ((my-font (get-default-font))		; "JetBrains Mono", "Fira Code"
	(my-height 130))
    (set-face-attribute 'default nil :font my-font :height my-height)
    (set-face-attribute 'fixed-pitch nil :font my-font :height my-height)
    (set-face-attribute 'variable-pitch nil :font my-font :height my-height :weight 'regular)))

(defun load-face-attributes-to-frame (frame)
  (select-frame frame)
  (load-face-attributes))

(if (daemonp)
    (add-hook 'after-make-frame-functions #'load-face-attributes-to-frame)
  (load-face-attributes))

;; Two best themes for GNU Emacs is:
;; 1. Modus Operandi
;; 2. Modus Vivendi
(use-package modus-themes
  :disabled t
  :ensure nil
  :config
  (load-theme 'modus-operandi t nil))

;; NOTE So beatifull themes for GNU Emacs. Good alternative for `modus-themes'
(use-package ef-themes
  :disabled t
  :demand t
  :config
  (load-theme 'ef-melissa-light t nil))

(use-package solarized-theme
  :demand t
  :config
  (load-theme 'solarized-light t nil))

(use-package hl-line
  :ensure nil
  :hook ((prog-mode . hl-line-mode)
         (dired-mode . hl-line-mode)
         (benchmark-init/tabulated-mode . hl-line-mode)))

(use-package display-line-numbers
  :ensure nil
  :hook ((prog-mode . display-line-numbers-mode)))

(use-package doom-themes
  :disabled t
  :demand t
  :config
  (load-theme 'doom-one t nil))

(use-package doom-modeline
  :demand t
  :init
  (when is-first-startup
    (nerd-icons-install-fonts))
  :custom
  (doom-modeline-height 30)
  (doom-modeline-bar-width 3)
  (doom-modeline-hud nil)
  (doom-modeline-percent-position '(-3 "%p"))
  
  :config
  (doom-modeline-mode 1))

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
