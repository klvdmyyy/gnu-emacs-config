;;; git.el
;;
;;; Commentary:
;;
;;; Code:

(use-package git-modes
  :mode ((".gitignore\\'" .  gitignore-mode)
	     (".gitconfig\\'" . gitconfig-mode)
	     (".gitattributes\\'" . gitattributes-mode)))

(use-package diff-hl
  ;; Using diff-hl only with window system
  :if (window-system)
  :hook (prog-mode . diff-hl-mode)
  :custom
  (diff-hl-bmp-max-width 4))

(use-package git-gutter
  ;; Using git-gutter only in Terminal
  :unless (window-system)
  :hook (prog-mode . git-gutter-mode)
  :custom
  (git-gutter:modified-sign "=")
  (git-gutter:added-sign "+")
  (git-gutter:deleted-sign "-")
  (git-gutter:window-width 1)
  :config
  (set-face-background 'git-gutter:modified (face-foreground 'git-gutter:modified))
  (set-face-background 'git-gutter:added (face-foreground 'git-gutter:added))
  (set-face-background 'git-gutter:deleted (face-foreground 'git-gutter:deleted)))

;; Magit from elpaca need latest version of transient
;; which is not in repo (lower than minimum required)
(use-package transient
  :ensure (transient
	       :host github
	       :repo "magit/transient"
	       :tag "v0.9.1"))

(use-package magit
  :commands (magit
	         magit-diff-unstaged)
  :bind ("C-x g" . magit))

;;; git.el ends here
