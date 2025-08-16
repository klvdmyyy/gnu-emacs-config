;;; init.el --- Initialization file -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;; Code:

(eval-when-compile
  (require 'lazydo))

(defconst user-packages-dependencies
  '(dash))

(defconst user-packages
  '(vertico
    orderless
	consult
    corfu
	embark
	embark-consult
    smartparens
	golden-ratio
    ace-window
	flycheck
	magit))

(setopt package-archives
        '(("gnu" . "https://elpa.gnu.org/packages/")
          ("nongnu" . "https://elpa.nongnu.org/nongnu/")
          ;; ("melpa" . "https://melpa.org/packages/")
          ))

(dolist (package (append user-packages-dependencies
                         user-packages))
  (unless (package-installed-p package)
    (package-install package)))

;;; Emacs Lisp Autoindent Mode:

(defun emacs-lisp-indent-buffer ()
  "Indent Emacs Lisp buffer from `point-min' to `point-max'."
  (interactive)
  (indent-region (point-min) (point-max))
  (untabify))

(define-minor-mode emacs-lisp-auto-indent-mode
  "Minor mode for Emacs Lisp autoindentation before save."
  :group 'emacs-lisp-auto-indent
  (if emacs-lisp-auto-indent-mode
      (add-hook 'before-save-hook 'emacs-lisp-indent-buffer)
    (remove-hook 'before-save-hook 'emacs-lisp-indent-buffer)))

(add-hook 'emacs-lisp-mode-hook 'emacs-lisp-auto-indent-mode)

;;; Vertico:

(hook! 'pre-command-hook 'vertico
       :lazy-load t)

(after! 'vertico
  (vertico-mode 1))

;;; Orderless:

(hook! 'self-insert-command 'orderless
       :lazy-load t)

(after! 'orderless
  (setq completion-styles '(orderless basic)))

;;; Consult:

(bind-keys ("s-B" . consult-buffer)
           ([remap switch-to-buffer] . consult-buffer)
           ([remap imenu] . consult-imenu)
           ("C-s" . consult-line)
           ([remap goto-line] . consult-goto-line))

;;; Embark:

(autoload! "embark"
  '(embark-act nil t)
  '(embark-dwim nil t)
  '(embark-bindings nil t))

(bind-keys ("C-." . embark-act)		   ; pick some comfortable binding
           ("C-;" . embark-dwim)	   ; good alternative for M-.
           ("C-h B" . embark-bindings)) ; alternative for `describe-bindings'

(after! 'embark
  ;; FIXME: `org-open-at-point-global' can't open link to heading (in TOC for example)
  ;;
  ;; I solve it just by replacing `org-open-at-point-global' by default
  ;; `org-open-at-point' function when current major mode is Org
  (define-advice org-open-at-point-global
      (:around (orig-fun) current-mode-is-org)
    (if (eq major-mode #'org-mode)
        (funcall #'org-open-at-point)
      (funcall orig-fun))))

(after! 'embark-consult
  (add-hook 'embark-collect-mode-hook 'consult-preview-at-point-mode))

;;; Corfu:

(hook! 'self-insert-command 'corfu
       :lazy-load t)

(after! 'corfu
  (setq corfu-cycle t)
  (setq tab-always-indent 'complete)
  (global-corfu-mode 1)

  (require 'corfu-popupinfo)
  (corfu-popupinfo-mode 1)

  (bind-keys* :map corfu-map
              ("TAB" . corfu-complete)
              ("M-d" . corfu-popupinfo-toggle)
              :map corfu-popupinfo-map
              ("M-n" . corfu-popupinfo-scroll-up)
              ("M-p" . corfu-popupinfo-scroll-down)))

;;; Smartparens:

(autoload! "smartparens"
  '(smartparens-mode nil t)
  '(smartparens-strict-mode nil t))

(add-hook 'prog-mode-hook 'smartparens-mode)
(add-hook 'prog-mode-hook 'smartparens-strict-mode)

(after! 'smartparens
  (require 'smartparens-config)
  (bind-keys :map smartparens-mode-map
             ("M-s" . nil)
             ("M-DEL" . sp-backward-unwrap-sexp)
             ("C-<left>" . sp-forward-barf-sexp)
             ("C-<right>" . sp-forward-slurp-sexp)))

;;; Golden Ratio:

(hook! 'split-window-below 'golden-ratio
	   :lazy-load t)

(hook! 'split-window-right 'golden-ratio
	   :lazy-load t)

(after! 'golden-ratio
  (golden-ratio-mode 1)
  (add-to-list 'golden-ratio-extra-commands 'ace-window)
  (add-to-list 'golden-ratio-extra-commands 'avy-goto-char-2)
  (add-to-list 'golden-ratio-extra-commands 'avy-goto-word-0))

;;; Ace Window:

(autoload 'ace-window "ace-window" nil t)
(bind-key "M-o" 'ace-window)

;;; Tree-Sitter:

(defun treesit-install-all ()
  "Install all language grammars.

From `treesit-language-source-alist' variable
by `treesit-install-language-grammar' function.

This function install language grammar only when it unavailable."
  (interactive)
  (mapc
   (lambda (lang)
     (when (not (treesit-language-available-p lang))
       (treesit-install-language-grammar lang)))
   (mapcar #'car treesit-language-source-alist)))


;; Tree Sitter source
(setq treesit-language-source-alist
      '((go "https://github.com/tree-sitter/tree-sitter-go")
        (gomod "https://github.com/camdencheek/tree-sitter-go-mod")
        (c "https://github.com/tree-sitter/tree-sitter-c")
        ;; (zig "https://github.com/maxxnino/tree-sitter-zig")
        ;; (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
        (dockerfile "https://github.com/camdencheek/tree-sitter-dockerfile")
        (python "https://github.com/tree-sitter/tree-sitter-python")
        (bash "https://github.com/tree-sitter/tree-sitter-bash")
        (json "https://github.com/tree-sitter/tree-sitter-json")
        ;; (meson "https://github.com/tree-sitter-grammars/tree-sitter-meson")
        ;; (cmake "https://github.com/uyha/tree-sitter-cmake")
		))

(add-hook 'after-init-hook
          (lambda ()
            (run-with-timer 1 nil 'treesit-install-all)))

;;; LSP (Language Server Protocol):

(when (daemonp)
  (require 'eglot))

;;; Flycheck:

(add-hook 'after-init-hook 'global-flycheck-mode)

;;; JSON + Tree-Sitter:

(add-to-list 'major-mode-remap-alist '(js-json-mode . json-ts-mode))

;;; Bash + Tree-Sitter:

(add-to-list 'major-mode-remap-alist '(sh-mode . bash-ts-mode))

(add-hook 'bash-ts-mode-hook 'eglot-ensure)

;;; Python + Tree-Sitter:

(add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode))

(add-hook 'python-ts-mode-hook 'eglot-ensure)

;;; C + Tree-Sitter:

(add-to-list 'major-mode-remap-alist '(c-mode . c-ts-mode))

(add-hook 'c-ts-mode-hook 'eglot-ensure)

;;; Go + Tree-Sitter:

(add-to-list 'auto-mode-alist '("\\.go\\'" . go-ts-mode))
(add-to-list 'auto-mode-alist '("go.mod\\'" . go-mod-ts-mode))

(add-hook 'go-ts-mode-hook 'eglot-ensure)

;;; Eshell:

(define-minor-mode eshell-mode-setup
  "Set up environment on `eshell-mode' invocation."
  :group 'eshell
  (if eshell-mode-setup
      (progn
        ;; FIXME: eshell throw error at `completion-at-point' with `all-the-icons-completion-mode' enabled.
        ;; This is just a temporary fix which disable it.
        (when (boundp 'all-the-icons-completion-mode)
          (all-the-icons-completion-mode 0))
        (if (and (boundp 'envrc-global-mode) envrc-global-mode)
            (add-hook 'envrc-mode-hook (lambda () (setenv "PAGER" "")))
          (setenv "PAGER" ""))
        (eshell/alias "l" "ls -al $1")
        (eshell/alias "e" "find-file $1")
        (eshell/alias "ee" "find-file-other-window $1")
        (eshell/alias "d" "dired $1")
        (eshell/alias "gd" "magit-diff-unstaged")
        ;; (local-unset-key 'eshell/clear)
        )
    (when (boundp 'all-the-icons-completion-mode)
      (all-the-icons-completion-mode 1))))

(defun switch-to-prev-buffer-or-eshell (arg)
  (interactive "P")
  (if arg
      (eshell arg)			; or `project-eshell-or-eshell'
    (switch-to-buffer (other-buffer (current-buffer) 1))))

(defun project-eshell-or-eshell (&optional arg)
  (interactive "P")
  (if (project-current)
      (project-eshell)
    (eshell arg)))

(after! 'eshell
  (require 'em-alias)
  (require 'em-hist)

  (add-hook 'eshell-mode-hook 'eshell-mode-setup)

  (bind-key "s-e" 'switch-to-prev-buffer-or-eshell eshell-mode-map)
  (autoload 'consult-history "consult")
  (bind-key "M-r" 'consult-history eshell-hist-mode-map))

(bind-key "s-e" 'project-eshell-or-eshell)

;;; Magit:

(when (daemonp)
  (require 'magit))

(bind-key "C-x g" 'magit)

(provide 'init)

;;; init.el ends here
