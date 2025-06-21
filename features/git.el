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
  ;; Using git-gutter only in Terminal
  ;; :unless (window-system)
  :hook (prog-mode . git-gutter-mode)
  :custom
  (git-gutter:modified-sign "M")        ; or `='
  (git-gutter:added-sign "A")           ; or `+'
  (git-gutter:deleted-sign "D")         ; or `-'
  (git-gutter:window-width 1)
  :custom-face
  (git-gutter:modified ((t (:foreground "orange" :background "transparent"))))
  (git-gutter:added ((t (:foreground "green" :background "transparent"))))
  (git-gutter:deleted ((t (:foreground "red" :background "transparent")))))

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
