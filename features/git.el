;;; git.el
;;
;;; Commentary:
;;
;;; Code:

(use-package git-modes
  :mode ((".gitignore\\'" .  gitignore-mode)
	     (".gitconfig\\'" . gitconfig-mode)
	     (".gitattributes\\'" . gitattributes-mode)))

(use-package git-gutter
  :hook (prog-mode . git-gutter-mode)
  :custom
  (git-gutter:modified-sign "=")
  (git-gutter:added-sign "+")
  (git-gutter:deleted-sign "-")
  (git-gutter:window-width 1)
  :config
  (set-face-background 'git-gutter:modified nil)
  (set-face-foreground 'git-gutter:modified "orange")
  (set-face-background 'git-gutter:added nil)
  (set-face-foreground 'git-gutter:added "green")
  (set-face-background 'git-gutter:deleted nil)
  (set-face-foreground 'git-gutter:deleted "red"))

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

(provide 'features/git)

;;; git.el ends here
