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

(use-package eat
  :commands (eat eshell)
  :hook ((eshell-load . eat-eshell-mode)
         (eshell-load . eat-eshell-visual-command-mode))
  :bind (("s-E" . eat-project-or-eat))
  :custom
  ;; Priority: babashka -> nu (nushell) -> zsh -> fish -> bash -> sh
  (explicit-shell-file-name (or (executable-find "bb") ; Clojure shell
                                (executable-find "nu") ; Nushell
                                (executable-find "zsh") ; ZSH
                                (executable-find "fish") ; Like ZSH but simpler
                                (executable-find "bash") ; Default Bash
                                (executable-find "sh"))) ; omg :>
  (eat-line-input-ring-size 1024)
  (eat-kill-buffer-on-exit t)
  (eat-term-scrollback-size nil)
  (eat-enable-mouse t))

;;; eat.el ends here
