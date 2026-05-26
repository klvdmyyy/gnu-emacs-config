;;; init.el --- Init -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;; Code:

(defvar default-text-scale 10
  "Text scale used by default.
See `global-text-scale-adjust' function which called with this variable
in emacs' `use-package'.")

(defvar git-installed (and (executable-find "git")
			   t)
  "Value is nil if git executable not find on your computer.")

;; Print the error if git isn't installed
(unless git-installed
  (error "Git executable can't be found on your computer. It is necessary for package installation!"))

(defvar notes-known-keywords '("ideas"
			       "personal")
  "Known keywords/tags for note taking. (Denote/Org Roam)

Just would be passed into `denote-known-keywords' variable.")

;;; Emacs

(defun setup-default-text-scale ()
  "Setting up default text scale.
See `default-text-scale' variable.


Press <f2> to change text scale locally."
  (interactive)
  (global-text-scale-adjust default-text-scale))

(defun load-custom-file ()
  (load custom-file :no-error :no-message :no-suffix :must-suffix))

(use-package emacs
  :hook ((emacs-startup . setup-default-text-scale)
	 (after-init . load-custom-file)
	 (prog-mode . display-line-numbers-mode))
  :custom
  (default-input-method "russian-computer")
  (ring-bell-function 'ignore)
  (display-line-numbers-type 'relative)

  ;; Don't make *~ backup files.
  (make-backup-files nil)
  :config

  ;; Disable cursor blinking
  (blink-cursor-mode -1))

(use-package recentf
  :ensure nil
  :hook (emacs-startup . recentf-mode)
  :custom
  (recentf-max-menu-items 20)
  (recentf-max-saved-items 40))

;;; Package management

(use-package package
  :ensure nil
  :init (package-activate-all))

;;; UI/UX

;; Setup modus themes
(use-package modus-themes
  :custom
  (modus-themes-headings
   '((1 . (1.4))
     (2 . (1.3))
     (3 . (1.2))
     (t . (1.1)))))

;; Load theme
(use-package ef-themes
  :if git-installed
  :ensure t
  :vc (:url "https://github.com/protesilaos/ef-themes" :rev "2.1.0")
  :hook (emacs-startup . ef-themes-take-over-modus-themes-mode)
  :config
  ;; (modus-themes-load-theme 'ef-elea-dark)
  ;; (modus-themes-load-theme 'ef-dark)
  ;; (modus-themes-load-theme 'ef-dream)
  ;; (modus-themes-load-theme 'ef-eagle)
  ;; (modus-themes-load-theme 'ef-night)
  (modus-themes-load-theme 'ef-owl))

;;; Completion

(use-package vertico
  :if git-installed
  :ensure t
  :vc (:url "https://github.com/minad/vertico" :rev "2.8")
  :hook (emacs-startup . vertico-mode))

(use-package consult
  :if git-installed
  :ensure t
  :vc (:url "https://github.com/minad/consult" :rev "3.4")
  :bind (("C-s" . consult-line)
	 ("C-x b" . consult-buffer)
	 ("M-g M-g" . consult-goto-line)))

(use-package marginalia
  :if git-installed
  :ensure t
  :vc (:url "https://github.com/minad/marginalia" :rev "2.10")
  :after vertico
  :hook (vertico-mode . marginalia-mode))

;; Optionally use the `orderless' completion style.
(use-package orderless
  :if git-installed
  :vc (:url "https://github.com/oantolin/orderless" :rev "1.6")
  :custom
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (orderless-style-dispatchers '(+orderless-consult-dispatch orderless-affix-dispatch))
  ;; (orderless-component-separator #'orderless-escapable-split-on-space)
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles partial-completion))))
  (completion-category-defaults nil) ;; Disable defaults, use our settings
  (completion-pcm-leading-wildcard t)) ;; Emacs 31: partial-completion behaves like substring

;;; Key bindings

(use-package hydra
  :if git-installed
  :ensure t
  :vc (:url "https://github.com/abo-abo/hydra" :rev "0.15.0")
  :bind (("<f2>" . hydra-zoom/body))
  :config
  (defhydra hydra-zoom (global-map "<f2>")
    "
Press _g_ to zoom in
Press _l_ to zoom out
"
    ("g" text-scale-increase nil)
    ("l" text-scale-decrease nil)))

;;; Org Mode

(use-package org
  :ensure nil
  :hook ((org-mode . org-indent-mode)))

(use-package org-capture
  :ensure nil)

(use-package org-agenda
  :ensure nil
  :bind (("C-c a" . org-agenda))
  :custom
  (org-agenda-files
   '()))

;;; Utilities

(use-package project
  :ensure nil
  :bind (("C-c f" . project-find-file)))

(define-minor-mode eshell-mode-setup
  "Setting up environment on `eshell-mode' invocation."
  :group 'eshell
  (if eshell-mode-setup
      (progn
	(if (and (boundp 'envrc-global-mode) envrc-global-mode)
	    (add-hook 'envrc-mode-hook (lambda () (setenv "PAGER" "")))
	  (setenv "PAGER" ""))
	(eshell/alias "x" "exit")
	(eshell/alias "ff" "project-find-file")
	(eshell/alias "fd" "find-dired $PWD \"\"")
	(eshell/alias "rg" "consult-ripgrep")
	(eshell/alias "gg" "consult-git-grep")
	(eshell/alias "l" "ls -al $1")
	(eshell/alias "e" "find-file $1")
	(eshell/alias "ee" "find-file-other-window $1")
	(eshell/alias "d" "dired $1")
	(eshell/alias "gd" "magit-diff-unstaged")
	(eshell/alias "clear" "clear-scrollback"))
    t))

(defun project-eshell-or-eshell (&optional arg)
  (interactive "P")
  (if (project-current)
      (project-eshell)
    (eshell arg)))

(defun switch-to-prev-buffer-or-eshell (arg)
  (interactive "P")
  (if arg
      (eshell arg)          ; or `project-eshell-or-eshell'
    (switch-to-buffer (other-buffer (current-buffer) 1))))

(defun eshell/shortened-pwd ()
  "Return the shortened PWD.

~/.config/emacs -> ~/.c/emacs

~/.config/emacs/lisp -> ~/.c/e/lisp"
  (let ((splited (string-split
                  ;; TEMP: Temporary fix because `file-name-directory' sometimes
                  ;; can provide nil value. (for example with "~" abbreviated directory)
                  (or (file-name-directory (abbreviate-file-name (eshell/pwd))) "")
                  "/")))
    (concat
     (string-join
      (seq-map
       (lambda (name)
         (if (<= (length name) 2)
             name
           (if (string-equal (substring name 0 1) ".")
               (substring name 0 2)
             (substring name 0 1))))
       splited)
      "/")
     (file-name-base (abbreviate-file-name
                      (eshell/pwd))))))

(defun my-eshell-prompt ()
  "My custom prompt for Emacs' eshell."
  (concat
   ;; "\n"
   "(" user-login-name ") "
   (eshell/shortened-pwd) " "
   (concat "[" (format-time-string "%H:%M:%S") "] ")
   ;; TODO: Pretty Printed Last Status (from archive branch of repository)
   ;; (eshell/pp-last-status)
   "\n$ "))

(use-package eshell
  :ensure nil
  :hook ((eshell-mode . eshell-mode-setup))
  :custom
  (eshell-prompt-function #'my-eshell-prompt)
  :bind (("C-c e" . project-eshell-or-eshell)
	 :map eshell-mode-map
	 ("C-c e" . switch-to-prev-buffer-or-eshell)
	 :map eshell-hist-mode-map
	 ("M-r" . consult-history))
  :config
  (require 'em-alias)
  (require 'em-hist))

(use-package eat
  :if git-installed
  :ensure t
  :vc (:url "https://codeberg.org/akib/emacs-eat")
  :hook ((eshell-load . eat-eshell-mode)
	 (eshell-load . eat-eshell-visual-command-mode))
  :bind (("C-c t" . eat))
  :custom
  (eat-kill-buffer-on-exit 1))

;; FIXME:
;; I must update transient manually through the `package-upgrade'
;; before magit installation to use it.
(use-package magit
  :if git-installed
  :ensure t
  :vc (:url "https://github.com/magit/magit" :rev "v4.5.0" :lisp-dir "lisp")
  :bind (("C-x g" . magit-status)))

;; (use-package magit-todos
;;   :if git-installed
;;   :ensure t
;;   :vc (:url "https://github.com/alphapapa/magit-todos" :rev "v1.8.1")
;;   :hook (magit-mode . magit-todos-mode))

;;; Utilities: Dependencies

(use-package dash
  :if git-installed
  :ensure t
  :vc (:url "https://github.com/magnars/dash.el" :rev "2.20.0"))

(use-package transient
  :if git-installed
  :ensure t
  :vc (:url "https://github.com/magit/transient" :rev "v0.13.3" :lisp-dir "lisp"))

;;; Editing

(use-package multiple-cursors
  :ensure t
  :vc (:url "https://github.com/magnars/multiple-cursors.el" :rev "1.5.0")
  :bind (("C-S-c C-S-c" . mc/edit-lines)
	 ("C->" . mc/mark-next-like-this)
	 ("C-<" . mc/mark-previous-like-this)
	 ("C-c C-<" . mc/mark-all-like-this)))

(use-package elec-pair
  :ensure nil
  :hook ((prog-mode . electric-pair-mode)))

(use-package electric
  :ensure nil
  :hook ((prog-mode . electric-indent-mode)))

(use-package smartparens
  ;; FIXME: It breaking multiple cursor plugin (I don't know why)
  :disabled t
  :if git-installed
  :ensure t
  :vc (:url "https://github.com/fuco1/smartparens" :rev "1.11.0")
  :hook ((prog-mode . smartparens-mode)
	 (emacs-lisp-mode . smartparens-strict-mode))
  :config
  ;; Load default config
  (require 'smartparens-config))

;;; Denote

(use-package denote
  :if git-installed
  :ensure t
  :vc (:url "https://github.com/protesilaos/denote" :rev "4.1.3")
  :hook (dired-mode . denote-dired-mode)
  :bind (("C-c n n" . denote)
	 ("C-c n r" . denote-rename-file)
	 ("C-c n l" . denote-link)
	 ("C-c n b" . denote-backlinks)
	 ;; NOTE: using `consult-denote' instead
	 ;; ("C-c n g" . denote-grep)
	 ("C-c n d" . denote-dired))
  :config
  ;; Automatically rename Denote buffers when opening them so that
  ;; instead of their long file name they have, for example, a literal
  ;; "[D]" followed by the file's title.  Read the docstring of
  ;; `denote-rename-buffer-format' for how to modify this.
  (denote-rename-buffer-mode 1)
  
  :custom
  (denote-prompts '(title keywords subdirectory))
  (denote-directory "~/Denotes")
  (denote-known-keywords notes-known-keywords))

(use-package consult-denote
  :if git-installed
  :ensure t
  :vc (:url "https://github.com/protesilaos/consult-denote" :rev "0.4.2")
  :bind
  (("C-c n f" . consult-denote-find)
   ("C-c n g" . consult-denote-grep))
  :config
  (consult-denote-mode 1))

;; Languages

(use-package cmake-mode
  :if git-installed
  :ensure t
  :vc (:url "https://github.com/emacsmirror/cmake-mode" :rev "25340a7")
  :mode ("CMakeLists.txt\\'" "\\.cmake\\'"))

;; Local variables:
;; byte-compile-warnings: (not obsolete free-vars)
;; End:

;;; init.el ends here
