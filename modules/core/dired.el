;;; dired.el --- Dired Core module -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;; Code:

(with-eval-after-load 'dired
  (add-hook 'dired-mode-hook 'dired-omit-mode)
  (setq-default dired-omit-files "\\`\\'"))

;;; dired.el ends here
