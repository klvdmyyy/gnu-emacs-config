;;; eat.el --- Core Eat Terminal module -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;; Code:

(defun eat-project-or-eat (&optional arg)
  (interactive "P")
  (if (project-current)
      (eat-project arg)
    (eat)))

(defun switch-to-prev-buffer-or-eat (arg)
  (interactive "P")
  (if arg
      (eat nil arg)
    (switch-to-buffer (other-buffer (current-buffer) 1))))

(use-package eat
  :commands (eat)
  ;; :requires eshell
  :hook ((eshell-load . eat-eshell-mode)
         (eshell-load . eat-eshell-visual-command-mode))
  :bind (("s-E" . eat-project-or-eat)
         :map eat-mode-map
         ("s-E" . switch-to-prev-buffer-or-eat)
         ;; TODO Maybe bind to switching for Eshell !?
         ("s-e" . nil))
  :custom
  ;; Priority: nu (nushell) -> zsh -> fish -> bash -> babashka -> sh
  (explicit-shell-file-name (or (executable-find "nu")   ; Nushell (Interesting thing written in Rust)
                                (executable-find "zsh")  ; ZSH
                                (executable-find "fish") ; Like ZSH but simpler
                                (executable-find "bash") ; Default Bash
                                (executable-find "bb")   ; Clojure shell (Babashka)
                                (executable-find "sh"))) ; omg :>
  (eat-line-input-ring-size 1024)
  (eat-kill-buffer-on-exit t)
  (eat-term-scrollback-size nil)
  (eat-enable-mouse t))

;;; eat.el ends here
