;;; init.el --- Initialization file -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;; TODO: Setup Org (Agenda, Roam, Pomodoro and etc).
;; MAYBE: Setup `auto-insert-mode'.
;; TODO: Setup avy (`avy-goto-char-2' and `avy-goto-word-0')
;;
;;; Code:

(setopt user-full-name "Klementiev Dmitry"
		user-mail-address "klementievd08@yandex.ru")

(eval-when-compile
  (require 'lazydo))

(defconst user-packages-dependencies
  '(dash)
  "Dependencies for `user-packages'.")

(defconst user-packages
  '(vertico
    orderless
	marginalia
    consult
    corfu
    embark
    embark-consult
    smartparens
    visual-fill-column
    golden-ratio
    ace-window
    sideline
    sideline-eglot
    sideline-flymake
    magit
    cape
    yasnippet
    yasnippet-capf
	dired-gitignore
	leetcode
	nerd-icons)
  "Packages for user configuration.")

(setopt package-archives
        '(("gnu" . "https://elpa.gnu.org/packages/")
          ("nongnu" . "https://elpa.nongnu.org/nongnu/")
          ("melpa" . "https://melpa.org/packages/")))

(dolist (package (append user-packages-dependencies
                         user-packages))
  (unless (package-installed-p package)
    (package-install package)))

;;; Formatting:

(autoload 'format-on-save-mode "format-on-save"
  "Format buffer before save."
  t)

(add-hook 'prog-mode-hook 'format-on-save-mode)

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

;;; Marginalia:

(after! 'vertico
  (require 'marginalia))

(after! 'marginalia
  (marginalia-mode 1))

;;; Consult:

(setq-default
 ;; consult-async-input-debounce 0
 ;; consult-async-input-throttle 0
 ;; consult-async-refresh-delay 0
 consult-async-min-input 3)

(bind-keys ("s-B" . consult-buffer)
           ([remap switch-to-buffer] . consult-buffer)
           ([remap imenu] . consult-imenu)
           ("C-s" . consult-line)
           ([remap goto-line] . consult-goto-line)
           ("M-g e" . consult-flymake))

;;; Embark:

(autoload! "embark"
  '(embark-act nil t)
  '(embark-dwim nil t)
  '(embark-bindings nil t))

(bind-keys ("C-." . embark-act)        ; pick some comfortable binding
           ("C-;" . embark-dwim)       ; good alternative for M-.
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

(defconst default-pairs-list
  '((?\( . ?\))
    (?\[ . ?\])
    (?\{ . ?\}))
  "List of default pairs.")

(defun open-pair-p (char)
  "Return t if CHAR is opening pair."
  (member char (mapcar (lambda (pairs) (car pairs)) default-pairs-list)))

(defun close-pair-p (char)
  "Return t if CHAR is closing pair."
  (member char (mapcar (lambda (pairs) (cdr pairs)) default-pairs-list)))

(defun indent-between-pairs ()
  "Open a new brace or bracket expression, with relevant newlines and indent."
  (interactive)
  (if (and (open-pair-p (char-before))
           (close-pair-p (char-after)))
      (progn
        (newline-and-indent)
        (unless (eq (char-after) '?\n)
          (newline)
          (indent-according-to-mode)
          (forward-line -1)
          (indent-according-to-mode)))
    (newline-and-indent)))

(bind-key "RET" 'indent-between-pairs prog-mode-map)

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

;;; Visual fill column:

(add-hook 'prog-mode-hook 'visual-fill-column-mode)
(add-hook 'text-mode-hook 'visual-fill-column-mode)

(setq-default fill-column 160
              visual-fill-column-width 160)

(after! 'visual-fill-column
  (setopt visual-fill-column-center-text nil
		  visual-fill-column-enable-sensible-window-split t) ; Split windows vertically
  )

;;; Golden Ratio:

(hook! 'split-window-below 'golden-ratio
       :lazy-load t)

(hook! 'split-window-right 'golden-ratio
       :lazy-load t)

(hook! 'display-buffer-in-side-window 'golden-ratio
       :lazy-load t)

(after! 'golden-ratio
  (golden-ratio-mode 1)
  (add-to-list 'golden-ratio-extra-commands 'ace-window)
  (add-to-list 'golden-ratio-extra-commands 'avy-goto-char-2)
  (add-to-list 'golden-ratio-extra-commands 'avy-goto-word-0))

;;; Ace Window:

(autoload 'ace-window "ace-window" nil t)
(bind-key "M-o" 'ace-window)

(after! 'ace-window
  ;; Possible values:
  ;; - global
  ;; - frame
  ;; - visible (visible frames)
  (setq aw-scope 'visible))

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
        ;; (c "https://github.com/tree-sitter/tree-sitter-c")
        ;; (rust "https://github.com/tree-sitter/tree-sitter-rust")
        ;; (zig "https://github.com/maxxnino/tree-sitter-zig")
        ;; (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
        ;; (dockerfile "https://github.com/camdencheek/tree-sitter-dockerfile")
        ;; (python "https://github.com/tree-sitter/tree-sitter-python")
        (bash "https://github.com/tree-sitter/tree-sitter-bash")
        ;; (json "https://github.com/tree-sitter/tree-sitter-json")
        ;; (meson "https://github.com/tree-sitter-grammars/tree-sitter-meson")
        ;; (cmake "https://github.com/uyha/tree-sitter-cmake")
        ))

(add-hook 'after-init-hook
          (lambda ()
            (run-with-timer 1 nil 'treesit-install-all)))

;;; LSP (Language Server Protocol):

(when (daemonp)
  (require 'eglot))

;;; Sideline:

(setq-default sideline-backends-left '(sideline-flymake)
              sideline-backends-right '(sideline-eglot))

(after! 'sideline
  (setq sideline-backends-left-skip-current-line t   ; don't display on current line (left)
        sideline-backends-right-skip-current-line t  ; don't display on current line (right)
        sideline-order-left 'down                    ; or 'up
        sideline-order-right 'up                     ; or 'down
        sideline-format-left "%s   "                 ; format for left aligment
        sideline-format-right "   %s"                ; format for right aligment
        sideline-priority 100                        ; overlays' priority
        sideline-display-backend-name t))            ; display the backend name

(add-hook 'prog-mode-hook 'flymake-mode)
(add-hook 'flymake-mode-hook 'sideline-mode)

;;; Bash + Tree-Sitter:

(add-to-list 'major-mode-remap-alist '(sh-mode . bash-ts-mode))

(add-hook 'bash-ts-mode-hook 'eglot-ensure)

;;; Go + Tree-Sitter:

(setq-default go-ts-mode-indent-offset 4)

(add-to-list 'auto-mode-alist '("\\.go\\'" . go-ts-mode))
(add-to-list 'auto-mode-alist '("go.mod\\'" . go-mod-ts-mode))

(add-hook 'go-ts-mode-hook 'eglot-ensure)

(autoload 'go-testing-project "go-testing"
  "Run testing in project for all Golang files."
  t)

;;; Eshell:

(autoload 'nerd-icons-faicon "nerd-icons" nil nil)

(defmacro with-face (STR &rest PROPS)
  "Return STR propertized with PROPS."
  (declare (indent defun))
  `(propertize ,STR 'face (list ,@PROPS)))

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

(defun eshell/pp-last-status ()
  (let ((status (number-to-string eshell-last-command-status)))
	(if (string-equal status "0")
		(with-face (concat (nerd-icons-faicon "nf-fa-check") " " status)
		  :foreground "#63c990"
		  :weight 'bold)
	  (with-face (concat (nerd-icons-faicon "nf-fa-xmark") " " status)
		:foreground "#c75f5f"
		:weight 'bold))))

(defun my-eshell-prompt ()
  "My custom prompt for Emacs shell."
  (concat
   "\n"
   "("
   user-login-name
   ") "
   (eshell/shortened-pwd) " "
   (concat "[" (format-time-string "%H:%M:%S") "] ")
   (eshell/pp-last-status)
   "\n$ "))

(setopt eshell-prompt-function
		#'my-eshell-prompt)

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
		;; Use `eshell/clear-scrollback' instead of `eshell/clear'
		(eshell/alias "clear" "clear-scrollback")
		;; (eshell/alias "cl" "clear-scrollback")
        (eshell/alias "x" "exit")
        ;; TODO: Make more convenient FZF (files, grep and etc).
        (eshell/alias "ff" "project-find-file")
		(eshell/alias "fd" "find-dired $PWD \"\"")
        (eshell/alias "rg" "consult-ripgrep")
		(eshell/alias "gg" "consult-git-grep")
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
      (eshell arg)          ; or `project-eshell-or-eshell'
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

;;; Dired:

;; (setq-default dired-omit-files "\\`\\.git\\'")
;; (add-hook 'dired-mode-hook 'dired-omit-mode)

(add-hook 'dired-mode-hook 'dired-gitignore-mode)
(after! 'dired-gitignore
  (bind-key "C-d" 'dired-gitignore-mode dired-mode-map))

;;; Cape:

(add-hook 'completion-at-point-functions #'cape-file)
(add-hook 'completion-at-point-functions #'cape-history)

;;; Yasnippet + Yasnippet-Capf:

(autoload 'yas-global-mode "yasnippet" nil t)
(add-hook 'after-init-hook 'yas-global-mode)

(autoload 'yasnippet-capf "yasnippet-capf" nil t)
(add-hook 'completion-at-point-functions #'yasnippet-capf)

;;; Leetcode Client:

(setq-default leetcode-directory "~/leetcode"
			  leetcode-save-solutions t
			  leetcode-prefer-language "golang"
			  leetcode-prefer-sql "mysql")

(autoload 'leetcode "leetcode"
  "Open Leetcode client for Emacs."
  t)

(provide 'init)

;;; init.el ends here
