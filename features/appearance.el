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
  :ensure nil
  :config
  (load-theme 'modus-operandi t nil))

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

;;; appearance.el ends here
