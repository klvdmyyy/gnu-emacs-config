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
  :hook prog-mode
  :custom
  (git-gutter:modified-sign "M")        ; or `='
  (git-gutter:added-sign "A")           ; or `+'
  (git-gutter:deleted-sign "D")         ; or `-'
  (git-gutter:window-width 1)
  :custom-face
  (git-gutter:modified ((t (:foreground "orange" :background "transparent"))))
  (git-gutter:added ((t (:foreground "green" :background "transparent"))))
  (git-gutter:deleted ((t (:foreground "red" :background "transparent")))))

(use-package magit
  :pin nongnu
  :commands (magit
	         magit-diff-unstaged)
  :bind ("C-x g" . magit))

(use-package magit-todos
  :after magit
  :hook ((magit-mode . magit-todos-mode))
  :custom
  (magit-todos-keyword-suffix "\\(?:[([][^])]+[])]\\)?:"))

;; TODO: Implement Forge
(use-package forge
  :after magit)

;;; git.el ends here
