;;; git.el
;;
;;; Commentary:
;;
;;; Code:

(use-package git-modes
  :mode ((".gitignore\\'" .  gitignore-mode)
	 (".gitconfig\\'" . gitconfig-mode)
	 (".gitattributes\\'" . gitattributes-mode)))

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
